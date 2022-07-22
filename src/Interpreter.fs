module VoxLogicA.Interpreter

open VoxLogicA.Reducer

open System.Collections.Generic

open System.Threading.Tasks

(*

tasks always return a Handle<'t> not a 't

Handles have a Read method

SEE https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/october/csharp-accessing-xml-documentation-via-reflection

http://moiraesoftware.github.io/blog/2012/01/22/FSharp-Dataflow-agents-I/
*)

type ResourceType = Unit

type ResourceKey = string

type Requirements(dict: IReadOnlyDictionary<ResourceKey, ResourceType>) =
    member __.ByKey x = dict[x]
    static member Null = Requirements(new Dictionary<_, _>())

and Resource(resourceType) =
    let mutable assignedTo = None
    member val ResourceType: ResourceType = resourceType
    member __.AssignedTo = assignedTo

    member __.AssignTo rs =
        assert assignedTo.IsNone
        assignedTo <- Some rs

    member __.Reclaim() =
        assert assignedTo.IsSome
        assignedTo <- None

and Resources() =
    let byKey = new Dictionary<ResourceKey, Resource>()
    let byType = new Dictionary<ResourceType, list<Resource>>()

    member __.ComplyWith(requirements: Requirements) =
        Seq.forall
            id
            (seq {
                for kv in byKey do
                    yield requirements.ByKey kv.Key = kv.Value.ResourceType
            })

    member __.ByKey k = byKey[k]
    member __.ByType t = byType[t] :> seq<Resource>

    member this.Assign(k, resource: Resource) =
        assert not (byKey.ContainsKey k)
        byKey[k] <- resource
        let t = resource.ResourceType
        byType[t] <- resource :: byType[t]
        resource.AssignTo this

    member __.Reclaim() = // deliberately not fine-grained
        let v = byKey.Values

        for res in v do
            res.Reclaim()

        byKey.Clear()
        byType.Clear()
        v

type OperatorImplementation<'t>(requirements, run) =
    member __.Requires = requirements

    member __.Run: Resources -> array<'t> -> Task<'t> =
        fun resources args ->
            assert resources.ComplyWith requirements
            run resources args

    new(t) = OperatorImplementation(Requirements.Null, (fun _ _ -> t))
    new(run) = OperatorImplementation(Requirements.Null, run)

type ExecutionEngine<'t> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t>

type ComputeUnit<'t>
    (
        executionEngine: ExecutionEngine<'t>,
        operatorImplementation: OperatorImplementation<'t>,
        arguments: array<ComputeUnit<'t>>
    ) =
    let result = TaskCompletionSource<'t>() // But see also the new Channel type in dotnet as an alternative.
    let mutable running = false

    member val Requirements = operatorImplementation.Requires

    member val Resources = Resources()

    member this.Run(resources: Resources) =
        assert resources.ComplyWith this.Requirements

        if not running then
            running <- true
            ignore
            <| task {
                let inputs =
                    Array.map (fun (x: ComputeUnit<'t>) -> (x.Task: Task<'t>).Result) arguments

                try
                    let! r = operatorImplementation.Run resources inputs
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

let opFib = OperatorImplementation (fun _ args ->

                    let task =

                        new Task<_>(
                            (fun () ->
                                ErrorMsg.Logger.Debug $"running fib({args[0]},{args[1]})"
                                fib args[0]) (*, TaskCreationOptions.LongRunning *)
                        )

                    task.Start()
                    task)
type Arithmetics() =

    interface ExecutionEngine<int> with
        member __.ImplementationOf s =
            match s with
            | Number n -> OperatorImplementation(task { return (int n) })
            | (Identifier "fib"| Identifier "+") -> opFib
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"

type Interpreter<'t>(executionEngine: ExecutionEngine<'t>) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t>>()

    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id workUnit ->
                let arguments =
                    Seq.toArray
                    <| Seq.map (fun i -> computeUnits[i]) workUnit.arguments

                computeUnits[id] <- ComputeUnit(
                    executionEngine,
                    executionEngine.ImplementationOf workUnit.operator,
                    arguments
                ))
            program.workUnits

    member __.Query(id: WorkUnitid) =
        assert computeUnits.ContainsKey id

        for cu in computeUnits.Values do // TODO: allocate resources
            cu.Run(Resources())

        computeUnits[id].Task

    member __.A = ()
