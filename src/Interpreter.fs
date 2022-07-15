module VoxLogicA.Interpreter

open VoxLogicA.Reducer

(*

tasks always return a Handle<'t> not a 't

Handles have a Read method

*)

let runTaskGraph (program : WorkPlan) =
    for goal in program.goals do
        printfn $"GOAL: {goal}"
