module VoxLogicA.Interpreter

open VoxLogicA.Reducer


open System.Threading.Tasks

open VoxLogicA.Resources
open System.Collections.Generic

type Result<'t, 'kind when 'kind: equality> =
    { task: Task
      result: Resource<'t, 'kind> }

type OperatorImplementation<'t, 'kind when 'kind: equality>
    (
        requirements,
        run: Resources<'t, 'kind> -> array<Resource<'t, 'kind>> -> Task<Result<'t, 'kind>>
    ) =

    member __.Requires = requirements

    member __.Run (resources: Resources<'t, 'kind>) arguments =
        assert resources.Respect requirements
        run resources arguments

type ExecutionEngine<'t, 'kind when 'kind: equality> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t, 'kind>

type ComputeUnit<'t, 'kind when 'kind: equality>
    (
        id: int,
        operatorImplementation: OperatorImplementation<'t, 'kind>,
        arguments: seq<Task<Resource<'t, 'kind>>>
    ) =
    let outputTCS = TaskCompletionSource<seq<Resource<'t, 'kind>> * Result<'t, 'kind>>()

    member __.Id = id
    member val Requirements = operatorImplementation.Requires

    /// returns only the output resource so that its reference count can be increased before passing it to other ComputeUnits
    member this.Start(resources: Resources<'t, 'kind>) =
        task {
            assert resources.Respect this.Requirements

            let! inputs = Task.WhenAll(Seq.toArray arguments)
            let! output = operatorImplementation.Run resources inputs

            outputTCS.SetResult(inputs, output)
        }

    member val Awaiters = new HashSet<obj>()

    /// Method to read the result
    member __.Result =
        task {

            let! output = outputTCS.Task

            do! (snd output).task

            return (fst output), (snd output).result
        }

type Interpreter<'t, 'kind when 'kind: equality>
    (
        executionEngine: ExecutionEngine<'t, 'kind>,
        resourceManager: ResourceManager<'t, 'kind>
    ) =
    let manageResources = true // set to false for testing
    let computeUnits = new Dictionary<int, ComputeUnit<'t, 'kind>>()
    let started = new HashSet<int>()
    let queries = new Dictionary<int, TaskCompletionSource<int>>()
    let results = new Dictionary<int, TaskCompletionSource<Resource<'t, 'kind>>>()

    let toManage =
        new Dictionary<int, TaskCompletionSource<int * Resources<'t, 'kind> * seq<Resource<'t, 'kind>> * Resource<'t, 'kind>>>()

    let finish = new TaskCompletionSource()

    member __.Finish() = finish.SetResult()

    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id operation ->
                let arguments =
                    Seq.toArray
                    <| Seq.map
                        (fun i ->
                            task {
                                let! _, x = computeUnits[i].Result
                                return x
                            })
                        operation.arguments

                let cu =
                    ComputeUnit(id, executionEngine.ImplementationOf operation.operator, arguments)

                computeUnits[id] <- cu
                queries[id] <- new TaskCompletionSource<_>()
                toManage[id] <- new TaskCompletionSource<_>()
                results[id] <- new TaskCompletionSource<_>()


                for argumentId in operation.arguments do
                    ErrorMsg.Logger.Debug $"ADDING AWAITER {cu.Id} to {argumentId}"

                    ignore
                    <| computeUnits[ argumentId ].Awaiters.Add cu)

            program.operations

    member __.QueryAsync(cuId: OperationId) =
        assert computeUnits.ContainsKey cuId
        queries[ cuId ].SetResult cuId
        results[cuId].Task

    member this.Compute() =
        let allocate requirements =
            task {

                let resourcesOpt = resourceManager.Allocate(requirements)

                match resourcesOpt with
                | Some resources -> return resources
                | None ->
                    let! resourcesOpt = resourceManager.Wait requirements

                    match resourcesOpt with
                    | None ->
                        return
                            (ErrorMsg.fail
                                $"Not enough resources to satisfy requirements {requirements} for computeUnit #{id}")
                    | Some resources -> return resources
            }

        let run (cuId: int) =
            task {
                if not (started.Contains cuId) then
                    ignore <| started.Add cuId
                    ErrorMsg.Logger.Test $"STARTING cuId {cuId}"
                    let cu = computeUnits[cuId]
                    let! resources = allocate cu.Requirements

                    if not manageResources then
                        ErrorMsg.Logger.Debug "Resource management is disabled"

                    if manageResources then
                        for resource in resources.Values do
                            ErrorMsg.Logger.Test $"ASSIGNING RESOURCE {resource} to {cuId}"
                            resource.AssignTo cu

                    do! cu.Start resources
                    ErrorMsg.Logger.Test $"***** STARTED {cuId} ***"

                    ignore
                    <| Task.Run(
                        (fun () ->
                            ignore
                            <| task {
                                let! (arguments, result) = cu.Result // TODO when and where to capture the exception?
                                result.AssignTo this
                                results[ cuId ].SetResult result
                                ErrorMsg.Logger.Test $"FINISHED cuId {cuId}"

                                toManage[cuId]
                                    .SetResult(cuId, resources, arguments, result)
                            })
                    )

                    return ()
            }

        let manage
            (
                cuId: int,
                resources: Resources<'t, 'kind>,
                arguments: seq<Resource<'t, 'kind>>,
                result: Resource<'t, 'kind>
            ) =
            task {
                ErrorMsg.Logger.Test $"Managing {cuId}"
                let cu = computeUnits[cuId]

                if manageResources then

                    for awaiter in cu.Awaiters do
                        ErrorMsg.Logger.Test
                            $"ASSIGNING RESOURCE {result} to awaiter {(awaiter :?> ComputeUnit<'t, 'kind>).Id} of {cuId}"

                        result.AssignTo awaiter

                    let deallocate =
                        Seq.toArray
                        <| Seq.concat [ resources.Values :> seq<Resource<'t, 'kind>>
                                        arguments ]

                    for resource in deallocate do
                        ErrorMsg.Logger.Test $"REVOKING RESOURCE {resource} from {cuId}"
                        resource.Reclaim cu

                        if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                            ErrorMsg.Logger.Test $"RELEASING RESOURCE {resource}"
                            resourceManager.Return resource
            // UNTIL HERE
            }

        task {
            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            let q =
                Array.init queries.Count (fun i ->
                    task {
                        let! id = queries[i].Task
                        return Choice1Of2 id
                    })

            let m =
                Array.init toManage.Count (fun i ->
                    task {
                        let! (id, resources, arguments, result) = toManage[i].Task
                        return Choice2Of2(id, resources, arguments, result)
                    })

            let all = Array.concat [ q; m ]

            let cmdQueue =
                Array.mapi
                    (fun i t ->
                        (i,
                         task {
                             let! r = t
                             return (i, r)
                         }))
                    all

            while not finish.Task.IsCompleted do
                let! t' = Task.WhenAny(Array.map snd cmdQueue) // NOTE: WhenAny will ignore already completed tasks
                let! _ = t'
                let completed = Array.filter (fun (i,t: Task<_>) -> t.IsCompleted) cmdQueue
                for (i,t) in completed do // BY THE ABOVE NOTE
                    let! (i,cmd) = t 
                    cmdQueue[i] <- (i, (new TaskCompletionSource<_>()).Task)
                    match cmd with
                    | Choice1Of2 id ->
                        ErrorMsg.Logger.Test $"*** CHOICE 1"

                        for i = 0 to id do
                            ErrorMsg.Logger.Test $"Running {id}"
                            do! run i
                            ErrorMsg.Logger.Test "*** CONTINUING TO MAIN THREAD ***"
                    | Choice2Of2 (id, resources, arguments, result) -> 
                        ErrorMsg.Logger.Test $"*** CHOICE 2"
                        do! manage (id, resources, arguments, result)

        }
