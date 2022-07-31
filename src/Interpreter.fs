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
    let allocated = new Dictionary<int, TaskCompletionSource<int * Resources<'t,'kind>>>()
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
                allocated[id] <- new TaskCompletionSource<_>()


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
        let allocate requirements cuId =
            task {
                ErrorMsg.Logger.Test $"Called allocate on {cuId}"

                let resourcesOpt = resourceManager.Allocate(requirements)

                match resourcesOpt with
                | Some resources ->
                    ErrorMsg.Logger.Test $"Allocated resources for cu {cuId}"
                    allocated[cuId].SetResult(cuId,resources)

                | None ->
                    // failwith "ABOUT TO WAIT; CHECK COMMENT at Interpreter.fs:130"
                    // Comment for after summer:
                    // this approach is wrong, waiting here blocks the main thread.
                    // One should wait in the Thread.run which is some lines below this.
                    // However since after waiting one needs to assing resources,
                    // and that must be synchronous, then waiting instead should become a separate
                    // message in the message queue.
                    // Btw the message queues should be unified with a single message type.
                    ignore <| Task.Run (fun () -> ignore (task {
                        ErrorMsg.Logger.Test $"Waiting for resources for cu {cuId}"
                        let! resourcesOpt = resourceManager.Wait requirements

                        match resourcesOpt with
                        | None ->
                            ErrorMsg.Logger.Test $"Resources for cu {cuId} cannot be allocated"

                            return
                                (ErrorMsg.fail
                                    $"Not enough resources to satisfy requirements {requirements} for computeUnit #{id}")
                        | Some resources ->
                            ErrorMsg.Logger.Test "Obtained resources for cu {cuId} after waiting"
                            allocated[cuId].SetResult(cuId,resources)
                    }))

            }

        let preRun (cuId: int) =
            task {
                //ErrorMsg.Logger.Test $"About to start {cuId}; started is {started.Contains cuId}"
                ErrorMsg.Logger.Test $"Called preRun on {cuId}"
                if not (started.Contains cuId) then
                    ignore <| started.Add cuId
                    ErrorMsg.Logger.Test $"STARTING cuId {cuId}"
                    let cu = computeUnits[cuId]
                    do! allocate cu.Requirements cuId
            }

        let doRun (cuId : int,resources : Resources<'t,'kind>) =
            task {
                    ErrorMsg.Logger.Test $"Called doRun on {cuId}"
                    let cu = computeUnits[cuId]
                    ErrorMsg.Logger.Test $"GOT all resources for {cuId}"

                    if not manageResources then
                        ErrorMsg.Logger.Debug "Resource management is disabled"

                    if manageResources then
                        for resource in resources.Values do
                            ErrorMsg.Logger.Test $"ASSIGNING RESOURCE {resource} to {cuId}"
                            resource.AssignTo cu

                    do! cu.Start resources
                    ErrorMsg.Logger.Test $"***** STARTED {cuId} ***"

                    ignore
                    <| Task.Run( // TODO why is this required?
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
            let q = Array.init queries.Count (fun i -> queries[i].Task)

            let m = Array.init toManage.Count (fun i -> toManage[i].Task)

            let r = Array.init allocated.Count (fun i -> allocated[i].Task)

            let mkQueue (x: array<Task<_>>) (name : string) =
                Array.mapi
                    (fun i t ->
                        (i,
                         task {
                             let! r = t
                             ErrorMsg.Logger.Test $"Receved on queue {name} id {i}"
                             return (i, r)
                         }))
                    x

            let tignore (_,t) =
                task {
                    let! _ = t
                    return ()
                }

            let qQ = mkQueue q "q"
            let mQ = mkQueue m "m"
            let rQ = mkQueue r "r"

            let aQ =
                mkQueue (
                    Array.concat [ Array.map tignore qQ
                                   Array.map tignore mQ
                                   Array.map tignore rQ ]
                ) "a"

            while not finish.Task.IsCompleted do
                ErrorMsg.Logger.Test "AWAITING NEXT MESSAGE"
                let! newMessageTask = Task.WhenAny(Array.map snd aQ)
                let! (i, _) = newMessageTask
                let completed = Array.filter (fun (i, t: Task<_>) -> t.IsCompleted) aQ

                for (j, _) in completed do
                    aQ[i] <- (i, (new TaskCompletionSource<_>()).Task)

                ErrorMsg.Logger.Test "NEW MESSAGE"

                let completed = Array.filter (fun (i, t: Task<_>) -> t.IsCompleted) mQ

                for (i: int,
                     t: Task<int * (int * Resources<'t, 'kind> * seq<Resource<'t, 'kind>> * Resource<'t, 'kind>)>) in
                    completed do // BY THE ABOVE NOTE
                    mQ[i] <- (i, (new TaskCompletionSource<_>()).Task)
                    let! (i, (id, resources, arguments, result)) = t
                    do! manage (id, resources, arguments, result)

                let completed = Array.filter (fun (i, t: Task<_>) -> t.IsCompleted) rQ

                for (i: int,
                     t: Task<int * (int * Resources<'t, 'kind>)>) in
                    completed do // BY THE ABOVE NOTE
                    rQ[i] <- (i, (new TaskCompletionSource<_>()).Task)
                    let! (i, (j, resources)) = t
                    do! doRun (i, resources)

                let completed = Array.filter (fun (i, t: Task<_>) -> t.IsCompleted) qQ

                for (i, (t: Task<int * int>)) in completed do // BY THE ABOVE NOTE
                    qQ[i] <- (i, (new TaskCompletionSource<_>()).Task)
                    let! (i, id) = t

                    for j = 0 to id do
                        ErrorMsg.Logger.Test $"About to run {id}"
                        do! preRun j
        }
