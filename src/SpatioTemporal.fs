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
            match vl with
            | [] -> failwith "Unbound video"
            | (n,v,l)::vs -> if video = n then (n,v,l) else (lookup vid vs)
        in lookup video env

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec updateExpr exp (venv : VideoEnv) =
    match exp with
    | ECall(p, ide, []) ->
        match ide with
        | Prefix "video" _ -> 
            let (_,_,l) = venv.Lookup(ide)
            if time >= l then
                failwith "Exceeded trace length"
            else 
                ECall(p, ide + "_" + (string time), [])
        | "<>" -> failwith "Diamond must take one argument"
        | _ -> ECall(p, ide, [])
    | ECall(p, ide, ex::exps) ->
        match ide with
        | "<>" ->
            time <- time+1.0
            updateExpr ex venv
        | _ -> ECall(p, ide, (updateExpr ex venv)::exps)
    | e -> e

let rec flattenSpatioTemporal (syntax : list<Command>) (venv : VideoEnv) =
    match syntax, venv with
    | [], venv -> ([],venv)
    | c::cs, venv -> 
        match c with
        | Declaration(name,args,expr) ->
            match expr with
            | ECall(_, "bind", [EString s; ENumber n]) -> 
                let venv1 = venv.Bind(name,s,n)
                let commands, venv2 = flattenSpatioTemporal cs venv1
                (c::(commands), venv2)
            | _ ->   
                let newExpr = updateExpr expr venv
                time <- 0
                let commands, venv1 = flattenSpatioTemporal cs venv
                (Declaration(name, args, newExpr)::(commands), venv1)
        | _ ->
            let commands, venv1 = flattenSpatioTemporal cs venv
            (c::(commands), venv1)