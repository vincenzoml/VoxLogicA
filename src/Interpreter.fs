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

open System.Threading
open System.Collections.Concurrent

// type ThreadOwningSyncCtx() =
//     inherit SynchronizationContext()

//     let _queue = new BlockingCollection<(SendOrPostCallback * obj)>()

//     member _.DoWork(cancellationToken: CancellationToken) =
//         while not cancellationToken.IsCancellationRequested do
//             let (callback, state) = _queue.Take()
//             callback.Invoke state
//         ()

//     override _.Post(callback, state) =
//         _queue.Add((callback, state))
//         ()

//     override _.Send(callback, state) =
//         let tcs = TaskCompletionSource()
//         let cb s =
//             callback.Invoke s
//             tcs.SetResult()
//         _queue.Add((cb, state))
//         tcs.Task.Wait()
//         ()

type Interpreter<'t, 'kind when 'kind: equality>
    (
        executionEngine: ExecutionEngine<'t, 'kind>,
        resourceManager: ResourceManager<'t, 'kind>
    ) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t, 'kind>>()
    let started = new HashSet<int>()
    let tid = 
        let r = System.Threading.Thread.CurrentThread.ManagedThreadId
        ErrorMsg.Logger.Test "thrd" $"{r}"
        r
    // let syncCtx = ThreadOwningSyncCtx()

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

    member this.QueryAsync(id: OperationId) =
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
                    ErrorMsg.Logger.Test "temp" $"STARTING cuId {cuId}"
                    let cu = computeUnits[cuId]
                    let! resources = allocate cu.Requirements

                    for resource in resources.Values do
                        ErrorMsg.Logger.Test "temp" $"ASSIGNING RESOURCE {resource} to {cuId}"
                        resource.AssignTo cu

                    assert (System.Threading.Thread.CurrentThread.ManagedThreadId = tid)
                    (cu.Start resources).Result
                    assert (System.Threading.Thread.CurrentThread.ManagedThreadId = tid)

                    let! (arguments,result) = cu.Result

                    ErrorMsg.Logger.Test "temp" $"FINISHED cuId {cuId}"

                    for awaiter in cu.Awaiters do
                        ErrorMsg.Logger.Test "temp" $"ASSIGNING RESOURCE {result} to awaiter {(awaiter :?> ComputeUnit<'t,'kind>).Id} of {cuId}"
                        result.AssignTo awaiter

                    let deallocate = Seq.toArray <| Seq.concat [ resources.Values :> seq<Resource<'t,'kind>>; arguments ]

                    for resource in deallocate do
                        ErrorMsg.Logger.Test "temp" $"REVOKING RESOURCE {resource} from {cuId}"
                        resource.Reclaim cu

                        if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                            ErrorMsg.Logger.Test "temp" $"RELEASING RESOURCE {resource}"
                            resourceManager.Return resource
            }

        task {
            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            let toRun = Seq.toArray (Seq.filter ((>=) id) computeUnits.Keys)
            // do SynchronizationContext.SetSynchronizationContext syncCtx


            for cuId in toRun do
                do! run cuId
            // let cts = new CancellationTokenSource()

            // syncCtx.DoWork(cts.Token)


            return! computeUnits[id].Result
        }
