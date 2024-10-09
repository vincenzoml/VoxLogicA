module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser

let mutable time = 0.0

type VideoEnv = Videos of list<string*string*float> with
    member this.Env = 
        match this with
        | Videos videos -> videos
    member this.Bind(name, video, length) = Videos ((name,video,length)::this.Env)
    member this.Lookup video = 
        let env = this.Env
        let rec lookup vid vl =
            match env with
            | [] -> failwith "Unbound video"
            | (n,v,l)::vs -> if video = n then (n,v,l) else (lookup vid vs)
        in lookup video env

let mutable videoEnv = Videos []

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec updateExpr exp =
    match exp with
    | ECall(p, ide, []) ->
        match ide with
        | Prefix "video" _ -> 
            let (_,_,l) = videoEnv.Lookup(ide)
            if time >= l then
                failwith "Exceeded trace length"
            else 
                ECall(p, ide + "_" + (string time), [])
        | "<>" -> failwith "Diamond takes one argument"
        | _ -> ECall(p, ide, [])
    | ECall(p, ide, ex::exps) ->
        match ide with
        | "<>" ->
            time <- time+1.0
            updateExpr ex
        | _ -> ECall(p, ide, (updateExpr ex)::exps)
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
                c::(flattenSpatioTemporal cs)
            | _ ->   
                let newExpr = updateExpr expr
                time <- 0
                Declaration(name, args, newExpr)::(flattenSpatioTemporal cs)
        | _ -> c::(flattenSpatioTemporal cs)