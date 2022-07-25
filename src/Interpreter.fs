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

    member this.Run(resources) =
        // assert this.Resources.ComplyWith this.Requirements
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


type Interpreter<'t,'kind when 'kind : equality>(executionEngine: ExecutionEngine<'t,'kind>,resourceManager: IResourceManager<'t,'kind>) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t,'kind>>()

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

    member __.Query(id: OperationId) =
        assert computeUnits.ContainsKey id
        task {
            System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest

            for cu in computeUnits.Values do
                if not cu.Running then
                    let! resources = resourceManager.Allocator(cu.Requirements)
                    cu.Run(resources)

            return! computeUnits[id].Task
        }