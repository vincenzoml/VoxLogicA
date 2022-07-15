module VoxLogicA.Interpreter

open VoxLogicA.Reducer

let runReduced (program : WorkPlan) = 
    for goal in program.goals do
        printfn $"GOAL: {goal}"
        
