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

type OperatorImplementation<'t>(requirements, run) =
    member __.Requires = requirements

    member __.Run: Resources<'t> -> array<Resource<'t>> -> Task<Resource<'t>> =
        fun resources args ->
            assert resources.ComplyWith requirements
            run resources args

    new(t) = OperatorImplementation(Requirements.Null, (fun _ _ -> t))
    new(run) = OperatorImplementation(Requirements.Null, run)

type ExecutionEngine<'t> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t>

type ComputeUnit<'t>(operatorImplementation: OperatorImplementation<'t>, arguments: array<ComputeUnit<'t>>) =
    let result = TaskCompletionSource<Resource<'t>>() // But see also the new Channel type in dotnet as an alternative.
    let mutable running = false

    member val Requirements = operatorImplementation.Requires

    member val Resources = Resources()

    member this.Run() =
        assert this.Resources.ComplyWith this.Requirements

        if not running then
            running <- true

            ignore
            <| task {
                let inputThreads =
                    Array.map (fun (x: ComputeUnit<'t>) -> (x.Task: Task<Resource<'t>>)) arguments

                let! inputs = Task.WhenAll inputThreads

                try
                    let! r = operatorImplementation.Run this.Resources inputs
                    result.SetResult r
                with
                | e -> result.SetException e
            }

    member __.Task = result.Task

// let runTaskGraph (program: WorkPlan) = Array.mapi mkComputeUnit program.tasks

// TEST

let rec fib x =
    if x < 2 then
        1
    else
        fib (x - 1) + fib (x - 2)

type A =
    abstract member M: unit

type ArithmeticsResourceType =
    | ResInt
    interface ResourceType

type ArithmeticsResource() =
    static let mutable id = 0

    do
        ErrorMsg.Logger.Debug $"Resource #{id} created"
        id <- id + 1

    member val Contents: int = 0 with get, set

    override __.ToString() = $"Resource #{id}"


type Arithmetics() =
    let opFib =
        OperatorImplementation(
            Requirements([ ("internal", ResInt :> ResourceType) ]),
            (fun resources args ->

                let task =

                    new Task<_>(
                        (fun () ->
                            ErrorMsg.Logger.Debug $"{args[1]}]: running fib({args[0]}) on resources ${resources}"
                            let inp = (args[0].Value: ArithmeticsResource)
                            let resource = resources.ByKey "internal" // Same key as in the requirements
                            let result = fib inp.Contents // Could use resource if needed
                            resource.Value.Contents <- result
                            ErrorMsg.Logger.Debug $"{args[1]}]: finished fib({args[0]}) on resources ${resources}"
                            resource), // NOTE: can return the same resource; if the result must be a different resource it must be added to the requirements with a specific key
                        TaskCreationOptions.PreferFairness
                    )

                task.Start()
                task)
        )

    interface ExecutionEngine<ArithmeticsResource> with
        member __.ImplementationOf s =
            match s with
            | Number n ->
                OperatorImplementation(
                    Requirements([ ("result", ResInt :> ResourceType) ]),
                    (fun resources _ ->
                        task {
                            let resource = resources["result"]
                            resource.Value.Contents <- int n
                            return resource
                        })
                )
            | (Identifier "fib"
            | Identifier "+") -> opFib
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"

type Interpreter<'t>(executionEngine: ExecutionEngine<'t>) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t>>()

    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id operation ->
                let arguments =
                    Seq.toArray
                    <| Seq.map (fun i -> computeUnits[i]) operation.arguments

                computeUnits[id] <- ComputeUnit(
                    executionEngine,
                    executionEngine.ImplementationOf operation.operator,
                    arguments
                ))
            program.operations

    member __.Query(id: OperationId) =
        assert computeUnits.ContainsKey id
        System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest

        for cu in computeUnits.Values do // TODO: allocate resources
            cu.Run()

        computeUnits[id].Task

    member __.A = ()
