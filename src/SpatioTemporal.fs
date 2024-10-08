module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser

let mutable time = 0.0
let mutable maxTime = 0.0

type VideoEnv = Videos of list<string*string*float> with
    member this.Env = 
        match this with
        | Videos videos -> videos
    member this.Bind(name, video, length) = Videos ((name,video,length)::this.Env)

let mutable videoEnv = Videos []

let rec updateExpr exp =
    match exp with
    | ECall(p, "frame", []) -> ECall(p, "frame" + (string time), [])
    | ECall(p, ide, ex::exps) ->
        if ide = "<>" then
            time <- time+1.0
            if time > maxTime then
                failwith "Exceeded trace length"
            else 
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
            match expr with
            | ECall(_, "bind", [EString s; ENumber n]) -> 
                videoEnv <- videoEnv.Bind(name,s,n)
                maxTime <- n
                c::(flattenSpatioTemporal cs)
            | _ ->   
                let newExpr = updateExpr expr
                time <- 0
                Declaration(name, args, newExpr)::(flattenSpatioTemporal cs)
        | _ -> c::(flattenSpatioTemporal cs)