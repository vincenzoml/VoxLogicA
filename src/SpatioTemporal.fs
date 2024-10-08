module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser

let mutable time = 0

let rec updateExpr exp =
    match exp with
    | ECall(p, "frame", []) -> ECall(p, "frame" + (string time), [])
    | ECall(p, ide, ex::exps) ->
        if ide = "<>" then
            time <- time+1
            updateExpr ex
            //match ex with 
            //| ECall(pos, id, e::exs) ->
            //    ECall(pos, id, (updateExpr e)::exs)
            //| _ -> failwith "Not a valid syntax"
        else
            ECall(p, ide, (updateExpr ex)::exps)
    | e -> e

let rec flattenSpatioTemporal (syntax : list<Command>) =
    match syntax with
    | [] -> []
    | c::cs -> 
        match c with
        | Declaration(name,args,expr) ->
            let newExpr = updateExpr expr
            time <- 0
            Declaration(name, args, newExpr)::(flattenSpatioTemporal cs)
        | _ -> c::(flattenSpatioTemporal cs)