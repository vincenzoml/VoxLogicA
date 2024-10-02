module VoxLogicA.Labelling

open Reducer

// The "label" function will be the main function that returns a "labelling",
// that is, the output of the new algorithm. 
//
// For now, it only returns a copy of the array of DAG nodes, in unspecified
// order, just as they come from the Reducer module.

// Check the type WorkPlan and associated types.

let label (workplan : WorkPlan) =
    Array.copy workplan.operations

