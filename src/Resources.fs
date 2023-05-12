module VoxLogicA.Resources

open VoxLogicA.Reducer

type Kind = Image of int | Float | T

type MemoryId = Loc of int*Kind

type ResultType = Ok of Kind | TypeError of string

type AnnotatedOperation = 
    {
        operation: Operation
        location: MemoryId
    }

type Resources = 
    static member ComputeStrategy (program : Reducer.WorkPlan) : array<MemoryId> = 
        let length = program.operations.Length + program.goals.Length
        let resources = Array.init length (fun i -> Loc(i,T))
        resources
    static member ToDot(program: Reducer.WorkPlan) =
        let length = program.operations.Length + program.goals.Length
        let resources = Array.init length (fun i -> Loc(i,T))
        let mutable str = "digraph {"

        for i = 0 to program.operations.Length - 1 do
            let annOperation = 
                { operation = program.operations[i]
                  location = resources.[i] } 

            str <-
                str
                + $"{i} [label=\"[{i}] {annOperation.operation.ToString() + annOperation.location.ToString()}\"];\n"

            for argument in annOperation.operation.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"