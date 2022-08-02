module VoxLogicA.Interpreter

open VoxLogicA.Reducer


open System.Threading.Tasks

open VoxLogicA.Resources
open System.Collections.Generic

type OperatorImplementation<'t, 'kind when 'kind: equality>
    (
        requirements,
        run: Resources<'t, 'kind> -> array<Resource<'t, 'kind>> -> Task<Resource<'t, 'kind>>
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
        arguments: seq<ComputeUnit<'t,'kind>>    ) =

    member __.Id = id
    member val Requirements = operatorImplementation.Requires
    member val Arguments = arguments
    /// returns only the output resource so that its reference count can be increased before passing it to other ComputeUnits
    member this.Start (resources: Resources<'t, 'kind>) (inputs: array<Resource<'t, 'kind>>) =
        task {
            assert resources.Respect this.Requirements            
            return! operatorImplementation.Run resources inputs
        }

    member val Awaiters = new HashSet<obj>()

open System.Threading
open System.Collections.Concurrent

type ThreadOwningSyncCtx() =
    inherit SynchronizationContext()

    let _queue = new BlockingCollection<(SendOrPostCallback * obj)>()

    member _.DoWork(cts : CancellationToken) =
        while not cts.IsCancellationRequested do
            ErrorMsg.Logger.Test "work" "work started"
            let (callback, state) = _queue.Take()
            callback.Invoke state
            ErrorMsg.Logger.Test "work" "work finished"
        ()

    override _.Post(callback, state) =
        _queue.Add((callback, state))
        ()

    override _.Send(callback, state) =
        let tcs = TaskCompletionSource()
        let cb s =
            callback.Invoke s
            tcs.SetResult()
        _queue.Add((cb, state))
        tcs.Task.Wait()
        ()

type Work<'t, 'kind when 'kind: equality> =
    | Waiting
    | Finished of Resource<'t,'kind>
    | Running of Resources<'t,'kind> * seq<Resource<'t,'kind>> * Task<Resource<'t,'kind>>
    member this.IsGoing = match this with | Running _ -> true | _ -> false


type Interpreter<'t, 'kind when 'kind: equality>
    (
        executionEngine: ExecutionEngine<'t, 'kind>,
        resourceManager: ResourceManager<'t, 'kind>
    ) =
    let computeUnits = new Dictionary<int,ComputeUnit<'t,'kind>>()
    
    let queries = new Dictionary<int,TaskCompletionSource<Resource<'t,'kind>>>()

    member val SynchronizationContext = ThreadOwningSyncCtx()

    member this.Run (cts) =
        this.SynchronizationContext.DoWork(cts)

    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id (operation : Operation) ->
                let arguments =
                    Seq.toArray
                    <| Seq.map
                        (fun i -> computeUnits[i])
                        operation.arguments

                let cu =
                    ComputeUnit(id, executionEngine.ImplementationOf operation.operator, arguments)

                computeUnits[id] <- cu

                for argumentId in operation.arguments do
                    ErrorMsg.Logger.Debug $"ADDING AWAITER {cu.Id} to {argumentId}"
                    ignore
                    <| computeUnits[argumentId].Awaiters.Add cu)

            program.operations

    member this.Session() =
        task {
            SynchronizationContext.SetSynchronizationContext this.SynchronizationContext
            let session = new Dictionary<int, Work<'t, 'kind>>()
            for cuId in computeUnits.Keys do
                session[cuId] <- Waiting

            let rec readyRec l acc = 
                match l with
                | [] -> Some (Array.ofList (List.rev acc))
                | (cu : ComputeUnit<'t,'kind>)::cus ->
                    match session[cu.Id] with
                    | Finished result -> 
                        readyRec cus (result::acc)
                    | _ -> None
                
            let ready wId = 
                assert session.ContainsKey wId
                match session[wId] with
                | Waiting ->
                    readyRec (Seq.toList <| computeUnits[wId].Arguments) []
                | _ -> None
            
            let mutable finish = false
            while not finish do
                for wId in Seq.sort session.Keys do // TODO: change data structure
                    match ready wId with
                    | None -> ()
                    | Some inputs ->
                        let cu = computeUnits[wId]
                        let resourcesOpt = resourceManager.Allocate cu.Requirements
                        match resourcesOpt with
                        | None -> ()
                        | Some resources ->
                            let tcs = new TaskCompletionSource<Resource<'t,'kind>>()
                            for resource in resources.Values do
                                ErrorMsg.Logger.Test "temp" $"ASSIGNING RESOURCE {resource} to {wId}"
                                resource.AssignTo cu
                            ErrorMsg.Logger.Test "temp" $"STARTING wId {wId}"
                            
                            let t = task {
                                let! r = cu.Start resources inputs
                                tcs.SetResult r
                            }

                            session[wId] <- Running (resources,inputs,tcs.Task)

                        SynchronizationContext.SetSynchronizationContext this.SynchronizationContext
                        let running = 
                            Seq.toArray (query {
                                for kv in session do
                                where kv.Value.IsGoing
                                select (kv.Key,(match kv.Value with Running (i,r,t) -> (i,r,t)))
                            })
                        let tasks = Array.map (fun (_,(_,_,x)) -> (x :> Task)) running
                        let i = Task.WaitAny tasks
                        SynchronizationContext.SetSynchronizationContext this.SynchronizationContext
                        let (wId,(resources,arguments,toutput)) = running[i]
                        let result = toutput.Result
                        session[wId] <- Finished result
                        if queries.ContainsKey wId then
                            result.AssignTo this
                            queries[wId].SetResult result
                        let cu = computeUnits[wId]                    
                        ErrorMsg.Logger.Test "temp" $"FINISHED wId {wId}"

                        for awaiter in cu.Awaiters do
                            ErrorMsg.Logger.Test "temp" $"ASSIGNING RESOURCE {result} to awaiter {(awaiter :?> ComputeUnit<'t,'kind>).Id} of {cu.Id}"
                            result.AssignTo awaiter

                        let deallocate = Seq.toArray <| Seq.concat [ resources.Values :> seq<Resource<'t,'kind>>; arguments ]

                        for resource in deallocate do
                            ErrorMsg.Logger.Test "temp" $"REVOKING RESOURCE {resource} from {wId}"
                            resource.Reclaim cu

                            if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                                ErrorMsg.Logger.Test "temp" $"RELEASING RESOURCE {resource}"
                                resourceManager.Return resource    
        }

    member __.QueryAsync(id: OperationId) =
        assert computeUnits.ContainsKey id
        let tcs = new TaskCompletionSource<_>()
        queries[id] <- tcs
        tcs.Task
        



