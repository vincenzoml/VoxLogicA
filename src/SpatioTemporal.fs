module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser

type FrameId = Id of string*int
type Env = list<FrameId>

let rec evalExpr (e : Expression) : string =
    match e with
    | ECall (_,ide, exprList) ->
        if ide = "<>" then "This is a diamond" else "This is something else"
    | _ -> e.ToString()


let evalCommand (c : Command) : string = 
    match c with
    | Declaration (_, _, expr) -> evalExpr expr
    | _ -> c.ToString()

let rec flattenSpatioTemporal (syntax : list<Command>) =
    match syntax with
    | [] -> []
    | c::cs -> ((evalCommand c)::(flattenSpatioTemporal cs))
    | _ -> failwith "stub"