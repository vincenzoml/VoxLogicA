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
type Resource = { resourceType: ResourceType }

type Constraint =
    | Exact of int
    static member combine c1 c2 =
        match c1, c2 with
        | (Exact i1, Exact i2) -> Exact(i1 + i2)

type Requirements(dict: Dictionary<ResourceType, Constraint>) =
    new(s: seq<ResourceType * Constraint>) =
        let dict = new Dictionary<_, _>()

        do
            for resourceType, a in s do
                dict[resourceType] <- if dict.ContainsKey resourceType then
                                          Constraint.combine dict[resourceType] a
                                      else
                                          a

        Requirements(dict)


type Resources(dict: Dictionary<ResourceType, Set<Resource>>) =
    new(s: seq<ResourceType * Set<Resource>>) =
        let dict = new Dictionary<_, _>()

        do
            for resourceType, a in s do
                dict[resourceType] <- if dict.ContainsKey resourceType then
                                          Set.union dict[resourceType] a
                                      else
                                          a

        Resources(dict)

[<AbstractClass>]
type OperatorImplementation<'t>(requirements: Requirements) =
    member __.Requires = requirements
    abstract member Run: seq<'t> -> 't

[<AbstractClass>]
type ExecutionEngine<'t>() =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t>

[<AbstractClass>]
type ComputeUnit<'t>(executionEngine: ExecutionEngine<'t>, taskId, task) =
    let operatorImplementation = executionEngine.ImplementationOf task.operator

    member __.TaskId = taskId
    member __.Task = task

    member val Requirements = operatorImplementation.Requires
    member val Resources = Resources []

let mkComputeUnit id (task: Task) = failwith "stub"

let runTaskGraph (program: WorkPlan) = Array.mapi mkComputeUnit program.tasks
