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

type ResourceKey = string

type Requirements<'ResourceType>(dict: IReadOnlyDictionary<ResourceKey, 'ResourceType>) =
    member __.ByKey x = dict[x]

    static member Null = Requirements(new Dictionary<_, _>())

    // new(reqs: seq<ResourceKey * 'ResourceType>) =
    //     assert (Seq.length (Seq.groupBy id (Seq.map fst reqs)) = Seq.length reqs)
    //     let dict = new Dictionary<_, _>()

    //     for (resourceKey, resourceType) in reqs do
    //         dict[resourceKey] <- resourceType

    //     Requirements(dict)

and Resource<'t, 'ResourceType>(value: 't, resourceType) =
    let mutable assignedTo = new HashSet<_>() // TODO: do this in debug; for release, use just a reference count.
    member val ResourceType: 'ResourceType = resourceType
    member __.AssignedTo = assignedTo

    member __.AssignTo resources = assignedTo.Add resources

    member __.Reclaim(res) =
        assert assignedTo.Contains res
        ignore <| assignedTo.Remove res // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value

and Resources<'t, 'ResourceType when 'ResourceType: equality>() =
    let byKey = new Dictionary<ResourceKey, Resource<'t, 'ResourceType>>()
    let byType = new Dictionary<'ResourceType, list<Resource<'t, 'ResourceType>>>()

    member __.ComplyWith(requirements: Requirements<'ResourceType>) =
        Seq.forall
            id
            (seq {
                for kv in byKey do
                    yield requirements.ByKey kv.Key = kv.Value.ResourceType
            })

    member __.ByKey k = byKey[k]

    member __.ByType t =
        byType[t] :> seq<Resource<'t, 'ResourceType>>

    member this.Assign(k, resource: Resource<'t, 'ResourceType>) =
        assert not (byKey.ContainsKey k)
        byKey[k] <- resource
        let t = resource.ResourceType
        byType[t] <- resource :: byType[t]
        resource.AssignTo this

    member this.Reclaim() = // deliberately not fine-grained
        let v = byKey.Values

        for res in v do
            res.Reclaim(this)

        byKey.Clear()
        byType.Clear()
        v

type OperatorImplementation<'t, 'ResourceType when 'ResourceType: equality>(requirements : 'ResourceType, run) =
    member __.Requires = requirements

    // member __.Run: Resources<'t, 'ResourceType>
    //     -> array<Resource<'t, 'ResourceType>>
    //     -> Task<Resource<'t, 'ResourceType>> =
    //     fun resources args ->
    //         assert resources.ComplyWith requirements
    //         run resources args

    // new(t) = OperatorImplementation(Requirements.Null, (fun _ _ -> t))
    new(run) = OperatorImplementation(Requirements.Null : Requirements<'ResourceType>, run)

type ExecutionEngine<'t, 'ResourceType when 'ResourceType: equality> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t, 'ResourceType>

type ComputeUnit<'t, 'ResourceType when 'ResourceType: equality>
    (
        operatorImplementation: OperatorImplementation<'t, 'ResourceType>,
        arguments: array<ComputeUnit<'t, 'ResourceType>>
    ) =
    let result = TaskCompletionSource<Resource<'t, 'ResourceType>>() // But see also the new Channel type in dotnet as an alternative.
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
                    Array.map
                        (fun (x: ComputeUnit<'t, 'ResourceType>) -> (x.Task: Task<Resource<'t, 'ResourceType>>))
                        arguments

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

type ArithmeticsResourceType = ResInt

type ArithmeticsResource() =
    static let mutable id = 0

    do
        ErrorMsg.Logger.Debug $"Resource #{id} created"
        id <- id + 1

    member val Contents: int = 0 with get, set


type Arithmetics() =
    let opFib =
        let reqs: seq<ResourceKey * ArithmeticsResourceType> = [ ("internal", ResInt) ]
        let run _ _ = failwith "stub"
        let d = new Dictionary<ResourceKey,ArithmeticsResourceType>() 
        d.Add("internal",ResInt)
        let d' = d :> IReadOnlyDictionary<_,_>
        OperatorImplementation(Requirements d',run)

    //  (fun res args ->

    //     let task =

    //         new Task<_>(
    //             (fun () ->
    //                 ErrorMsg.Logger.Debug $"{args[1]}]: running fib({args[0]}) on resources ${res}"
    //                 let r = fib args[0]
    //                 ErrorMsg.Logger.Debug $"{args[1]}]: finished fib({args[0]}) on resources ${res}"
    //                 r),
    //             TaskCreationOptions.PreferFairness
    //         )

    //     task.Start()
    //     task)

    interface ExecutionEngine<int,ArithmeticsResourceType> with
        member __.ImplementationOf s =
            match s with
            //| Number n -> OperatorImplementation(task { return (int n) })
            | Identifier "fib"
            | Identifier "+" -> opFib
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"

type Interpreter<'t,'ResourceType when 'ResourceType : equality>(executionEngine: ExecutionEngine<'t,'ResourceType>) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t,'ResourceType>>()

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
