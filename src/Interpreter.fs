module VoxLogicA.Interpreter

open VoxLogicA.Reducer

open System.Collections.Generic

(*

tasks always return a Handle<'t> not a 't

Handles have a Read method

SEE https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/october/csharp-accessing-xml-documentation-via-reflection

http://moiraesoftware.github.io/blog/2012/01/22/FSharp-Dataflow-agents-I/
*)

type ResourceType = Unit

type ResourceKey = string

type Requirements = IDictionary<ResourceKey,ResourceType>

type Resource(resourceType) =
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
    let byId = new Dictionary<ResourceKey,Resource>()
    let byType = new Dictionary<ResourceType,list<Resource>>()

    member __.ById id = byId[id]
    member __.ByType t = byType[t] :> seq<Resource>

    member this.Assign(id,resource : Resource) =
        assert not (byId.ContainsKey id)
        byId[id] <- resource
        let t = resource.ResourceType
        byType[t] <- resource::byType[t]
        resource.AssignTo this

    member __.Reclaim() = // deliberately not fine-grained
        let v = byId.Values
        for res in v do
            res.Reclaim()
        byId.Clear()
        byType.Clear()
        v





    // new(s: seq<ResourceType * Set<Resource>>) =
    //     new ResourceData(s,Set.union)
    // //     let dict = new Dictionary<_, _>()

    //     Seq.iter
    //         (fun (resourceType, a) ->
    //             dict[resourceType] <- if dict.ContainsKey resourceType then
    //                                       Set.union dict[resourceType] a
    //                                   else
    //                                       a)
    //         s

    //     Resources(dict)


[<AbstractClass>]
type OperatorImplementation<'t>(requirements: Requirements) =
    member __.Requires = requirements
    abstract member Run: seq<'t> -> 't

[<AbstractClass>]
type ExecutionEngine<'t>() =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t>


[<AbstractClass>]
type ComputeUnit<'t>(executionEngine: ExecutionEngine<'t>, operator, arguments) =
    let operatorImplementation = executionEngine.ImplementationOf operator

    member val Requirements = operatorImplementation.Requires

    member val Resources = Resources()

    member __.Run()

// let mkComputeUnit id (task: Task) = failwith "stub"

// let runTaskGraph (program: WorkPlan) = Array.mapi mkComputeUnit program.tasks
