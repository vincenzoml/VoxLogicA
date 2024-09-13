module VoxLogicA.Labelling

open Reducer

let label (workplan : WorkPlan) =
    Array.toList(workplan.operations)

