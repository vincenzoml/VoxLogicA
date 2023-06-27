module VoxLogicA.Resources

open System.Collections.Generic

open VoxLogicA.Reducer

type Kind = Image of int | Float | T

// A memory id identifies the virtual location
// of an operation, toghether with its type
type MemoryId = Loc of int*Kind

type ResultType = Ok of Kind | TypeError of string

// An operation annotated with its virtual
// memory location
type AnnotatedOperation = 
    {
        operation: Operation
        candidates: list<OperationId>
    }

type AllocationPlan = 
    {
        operations: array<AnnotatedOperation>
        goals: array<Goal>
    }

    override this.ToString() =
        let t =
            String.concat "\n"
            <| Array.mapi (fun i el -> $"{i} -> {el}") this.operations

        let g =
            String.concat ","
            <| Array.map (fun x -> x.ToString()) this.goals

        $"goals: {g}\noperations:\n{t}"

    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.operations.Length - 1 do
            let annOperation = this.operations[i]
            str <-
                str
                + $"{i} [label=\"[{i}] {annOperation.operation.ToString() + annOperation.candidates.ToString()}\"];\n"

            for argument in annOperation.operation.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"

module private Utils = 
    type IndexedOperations =
        { byId: Dictionary<OperationId, Operation> }

        member this.TryFind id =
            if this.byId.ContainsKey(id) then
                Some this.byId[id]
            else
                None
        
        member this.Add operator =
            this.byId.Add(operator.id, operator)
    let emptyIndOperations () =
        { byId = new Dictionary<_, _>() }

let rec computeCandidates (op : Operation) (operations : array<Operation>) = 
    let indexOf = (Array.findIndex ((=) op))
    let args = operations[indexOf operations].arguments
    let mutable current = Seq.toList (args)

    for arg in args do
        let candidate = operations[arg]
        current <- current @ (computeCandidates candidate operations)
    current
        
// Takes a program (a Reducer.Workplan) and computes
// an allocation strategy. For each operation, compute
// the set of memory locations where this may be allocated,
// as all its dependencies has been computed.
let computeStrategy (program : Reducer.WorkPlan) : AllocationPlan = 
    let length = program.operations.Length + program.goals.Length
    let mutable annOperations = []
    let ops = program.operations

    for op in ops do
        let annOperation = {
            operation = op
            candidates = computeCandidates op ops
        }
        annOperations <- annOperations @ [annOperation]
        
    { operations = Array.ofList annOperations
      goals = program.goals
    }

type Color = White | Grey | Black

type OpNode = Node of OperationId * Arguments * Color

let buildGraphFromWorkplan (workplan : WorkPlan) = 
    let operations = workplan.operations
    let mutable candidates = Array.init operations.Length (fun i -> [])
    let mutable nodes = []
    let goals = workplan.goals
    let mutable graph : list<list<OpNode>> = []
     
    let opDict = Utils.emptyIndOperations ()
    for op in operations do
        opDict.Add(op)

    for goal in goals do
        let id = 
            match goal with
            | GoalPrint(_, id) | GoalSave(_, id) -> id
        let opGoal = opDict.TryFind id
        graph <- graph @ [[Node(opGoal.Value.id, opGoal.Value.arguments, Black)]]

    for op in operations do
        nodes <- nodes @ [Node(op.id, op.arguments, White)]

    for goal in graph do
        match goal.Head with 
        | Node(id, args, colour) ->
            for arg in args do
                let argNode = List.filter (fun x -> x.id = arg) nodes
                match argNode.Head with
                | Node(id, args, White) -> candidates[goal.id] <- [id]