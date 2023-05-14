module VoxLogicA.Resources

open System.Collections.Generic

open VoxLogicA.Reducer

type Kind = Image of int | Float | T

// A memory id identifies the virtual location
// of an operation, toghether with its type
type MemoryId = Loc of int*Kind

type ResultType = Ok of Kind | TypeError of string

// Defines the set of reusable locations for 
// the operation identified by the field id
type Candidate = 
    { Id : OperationId
      Locs : array<MemoryId>
    }

// An operation annotated with its virtual
// memory location
type AnnotatedOperation = 
    {
        operation: Operation
        location: MemoryId
    }

type AllocationPlan = 
    {
        operations: array<Operation>
        goals: array<Goal>
        locations: array<MemoryId>
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
            let annOperation = 
                { operation = this.operations[i]
                  location = this.locations[i] } 

            str <-
                str
                + $"{i} [label=\"[{i}] {annOperation.operation.ToString() + annOperation.location.ToString()}\"];\n"

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

    let rec computeCandidatesRec id (indexedOps : IndexedOperations) candidates =
        let currentOp = indexedOps.TryFind id
        match currentOp with 
        | Some op -> 
            let dependencies = op.arguments
            dependencies
        | None -> Seq.empty

    let computeCandidates (operations : array<Operation>) id (candidates: array<Arguments>) =
        let indexedOps = 
            { byId = new Dictionary<OperationId, Operation>() }
        for op in operations do
            indexedOps.Add op
        candidates.SetValue(computeCandidatesRec id indexedOps candidates, id)

// Takes a program (a Reducer.Workplan) and computes
// an allocation strategy. For each operation, compute
// the set of memory locations where this may be allocated,
// as all its dependencies has been computed.
let computeStrategy (program : Reducer.WorkPlan) : AllocationPlan = 
    let length = program.operations.Length + program.goals.Length
    let resources = Array.init length (fun i -> Loc(i,T))

    let goals = program.goals
    let mutable (candidates : array<Arguments>) = Array.init length (fun i -> [])
    for goal in goals do
        match goal with
        | GoalSave (_, id) -> 
            candidates.SetValue(Utils.computeCandidates program.operations id candidates, id)
        | GoalPrint (_, id) -> 
            candidates.SetValue(Utils.computeCandidates program.operations id candidates, id)

    { operations = program.operations
      goals = program.goals
      locations = resources}