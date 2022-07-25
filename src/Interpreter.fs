module VoxLogicA.Interpreter

open VoxLogicA.Reducer

open System.Collections.Generic

open System.Threading.Tasks

open VoxLogicA.Resources

(*

tasks always return a Handle<'t> not a 't

Handles have a Read method

SEE https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/october/csharp-accessing-xml-documentation-via-reflection

http://moiraesoftware.github.io/blog/2012/01/22/FSharp-Dataflow-agents-I/
*)

type OperatorImplementation<'t,'kind when 'kind : equality>(requirements, run) =
    member __.Requires = requirements

    member __.Run: Resources<'t,'kind> -> array<Resource<'t,'kind>> -> Task<Resource<'t,'kind>> =
        fun resources args ->
            assert resources.ComplyWith requirements
            run resources args

type ExecutionEngine<'t,'kind when 'kind : equality> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t,'kind>

type ComputeUnit<'t,'kind when 'kind : equality>(operatorImplementation: OperatorImplementation<'t,'kind>, arguments: array<ComputeUnit<'t,'kind>>) =
    let result = TaskCompletionSource<Resource<'t,'kind>>() // But see also the new Channel type in dotnet as an alternative.
    let mutable running = false

    member __.Running = running

    member val Requirements = operatorImplementation.Requires

    member this.Run(resources : Resources<'t,'kind>) =
        assert resources.ComplyWith this.Requirements
        assert not running
        running <- true

        ignore
        <| task {
            let inputThreads =
                Array.map (fun (x: ComputeUnit<'t,'kind>) -> (x.Task: Task<Resource<'t,'kind>>)) arguments

            let! inputs = Task.WhenAll inputThreads

            try
                let! r = operatorImplementation.Run resources inputs
                result.SetResult r
            with
            | e -> result.SetException e
        }

    member __.Task = result.Task

// let runTaskGraph (program: WorkPlan) = Array.mapi mkComputeUnit program.tasks

type Scheduler<'t,'kind when 'kind : equality> =
    member __.X = ""

type Interpreter<'t,'kind when 'kind : equality>(executionEngine: ExecutionEngine<'t,'kind>,resourceManager: ResourceManager<'t,'kind>) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t,'kind>>()
    let ready = new Dictionary<_,_>(computeUnits)

    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id operation ->
                let arguments =
                    Seq.toArray
                    <| Seq.map (fun i -> computeUnits[i]) operation.arguments

                computeUnits[id] <- ComputeUnit(
                    executionEngine.ImplementationOf operation.operator,
                    arguments
                ))
            program.operations

    member __.QueryAsync(id: OperationId) =
        assert computeUnits.ContainsKey id
        task {
            let run id (resources : Resources<'t,'kind>) = task {
                    let cu = ready[id]
                    ignore <| ready.Remove id
                    cu.Run resources
                    let! _ = cu.Task
                    resourceManager.Return resources
            }

            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            for cuId in Seq.filter ((<=) id) ready.Keys do
                let cu = computeUnits[cuId]

                let resourcesOpt = resourceManager.Allocate(cu.Requirements)
                match resourcesOpt with
                    | Some resources ->
                        do! run cuId resources
                    | None ->
                        let! resourcesOpt = resourceManager.Wait cu.Requirements
                        match resourcesOpt with
                        | None -> ErrorMsg.fail $"Not enough resources to satisfy requirement {cu.Requirements} for computeUnit #{id}"
                        | Some resources ->
                            do! run cuId resources

            return computeUnits[id].Task
        }