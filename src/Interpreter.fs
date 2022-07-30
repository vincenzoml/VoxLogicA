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
    let mutable started = false
    let outputTCS = TaskCompletionSource<seq<Resource<'t, 'kind>> * Result<'t, 'kind>>()

    member __.Id = id
    member val Requirements = operatorImplementation.Requires

    member __.Started = started

    /// returns only the output resource so that its reference count can be increased before passing it to other ComputeUnits
    member this.Start(resources: Resources<'t, 'kind>) =
        assert resources.Respect this.Requirements

        task {
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
    let computeUnits = new Dictionary<int, ComputeUnit<'t, 'kind>>()
    let started = new HashSet<int>()
    let tid = System.Threading.Thread.CurrentThread.ManagedThreadId


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

                for argumentId in operation.arguments do
                    ErrorMsg.Logger.Debug $"ADDING AWAITER {cu.Id} to {argumentId}"
                    ignore
                    <| computeUnits[argumentId].Awaiters.Add cu)

            program.operations

    member __.QueryAsync(id: OperationId) =
        assert computeUnits.ContainsKey id

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
                    assert ErrorMsg.Logger.Assert $"STARTING cuId {cuId}"
                    let cu = computeUnits[cuId]
                    let! resources = allocate cu.Requirements

                    for resource in resources.Values do
                        assert ErrorMsg.Logger.Assert $"ASSIGNING RESOURCE {resource} to {cuId}"
                        resource.AssignTo cu

                    do! cu.Start resources
                    let opt = new System.Threading.Channels.UnboundedChannelOptions()
                    opt.AllowSynchronousContinuations <- false
                    
                    let ch = System.Threading.Channels.Channel.CreateUnbounded(opt)

                    let t = task {
                        let! x = cu.Result
                        let b = ch.Writer.TryWrite x
                        assert b
                    }

                    let! (arguments,result) = ch.Reader.ReadAsync()
                    // assert (System.Threading.Thread.CurrentThread.ManagedThreadId = tid)
                    assert ErrorMsg.Logger.Assert $"FINISHED cuId {cuId}"

                    for awaiter in cu.Awaiters do
                        assert ErrorMsg.Logger.Assert $"ASSIGNING RESOURCE {result} to awaiter {(awaiter :?> ComputeUnit<'t,'kind>).Id} of {cuId}"
                        result.AssignTo awaiter

                    let deallocate = Seq.toArray <| Seq.concat [ resources.Values :> seq<Resource<'t,'kind>>; arguments ]

                    for resource in deallocate do
                        assert ErrorMsg.Logger.Assert $"REVOKING RESOURCE {resource} from {cuId}"
                        resource.Reclaim cu

                        if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                            assert ErrorMsg.Logger.Assert $"RELEASING RESOURCE {resource}"
                            resourceManager.Return resource
            }

        task {
            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            let toRun = Seq.toArray (Seq.filter ((>=) id) computeUnits.Keys)

            for cuId in toRun do
                do! run cuId

            return! computeUnits[id].Result
        }
