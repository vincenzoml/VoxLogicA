module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser

let mutable time = 0.0

type Env<'T> = Env of list<string*'T> with
    member this.Environment = 
        match this with
        | Env env -> env

    member this.Bind(name, el) = Env ((name, el)::this.Environment)
    member this.Lookup(element : string) : 'T  = 
        let env = this.Environment
        let rec lookup el els =
            match els with
            | [] -> failwith "Unbound element"
            | (n,v)::es -> if el = n then v else (lookup el es)
        in lookup element env

type Video = Vid of string*float

type FunAbstraction = EFun of Expression*Env<list<Expression>>

//type VideoEnv = Videos of list<string*string*float> with
//    member this.Env = 
//        match this with
//        | Videos videos -> videos
//    member this.Bind(name, video, length) = Videos ((name,video,length)::this.Env)
//    member this.Lookup video = 
//        let env = this.Env
//        let rec lookup vid vl =
//            match vl with
//            | [] -> failwith "Unbound video"
//            | (n,v,l)::vs -> if video = n then (n,v,l) else (lookup vid vs)
//        in lookup video env

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let rec updateExpr exp (venv : Env<Video>) (eenv : Env<list<Expression>>) (fenv : Env<FunAbstraction>)  =
    match exp with
    | ECall(p, ide, []) ->
        match ide with
        | Prefix "video" _ -> 
            let vid = venv.Lookup(ide)
            match vid with
            | Vid(n,l) ->
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
            updateExpr ex venv eenv fenv
        //| "until" -> 
        //    let mutable arguments = Array.empty
        //    for e in ex::exps do
        //        match e with
        //        | ECall(_,id,_) -> arguments <- Array.append [|id|] arguments
        //    ex
        | _ -> 
            let funAbs = fenv.Lookup(ide)
            let exp = eenv.Lookup(ide)
            match funAbs with
            | EFun (e, env) ->
                let newExp = updateExpr e venv eenv fenv
                let mutable newExpList = []
                for e in exps do 
                    newExpList <- (updateExpr e venv eenv fenv)::newExpList
                ECall(p, ide, newExp::newExpList)
    | e -> updateExpr e venv eenv fenv

let rec flattenSpatioTemporal (syntax : list<Command>) (venv : Env<Video>) (expenv : Env<list<Expression>>) (funenv : Env<FunAbstraction>)=
    match syntax, venv, expenv, funenv with
    | [], venv, eenv, fenv -> ([],venv, eenv, fenv)
    | c::cs, venv, eenv, fenv -> 
        match c with
        | Declaration(name,args,expr) ->
            match expr with
            | ECall(_, "bind", [EString s; ENumber n]) -> 
                let venv1 = venv.Bind(name,(Vid(s,n)))
                let commands, venv2, eenv2, fenv2 = flattenSpatioTemporal cs venv1 eenv fenv
                (c::(commands), venv2, eenv2, fenv2)
            | ECall(_, ide, exps) ->
                let eenv1 = eenv.Bind(ide, exps)
                let funAbs = EFun (expr,eenv1)
                let fenv1 = funenv.Bind(ide,funAbs)
                let commands, venv2, eenv2, fenv2 = flattenSpatioTemporal cs venv eenv1 fenv1
                c::commands, venv2, eenv2, fenv2
            | _ ->   
                //let newExpr = updateExpr expr venv
                //time <- 0
                let commands, venv1, eenv2, fenv2 = flattenSpatioTemporal cs venv eenv fenv
                (c::commands), venv1, eenv2, fenv2
        | Print(p, name, exp) ->
            match exp with
            | ECall(_,ide,elist) ->
                if ide <> "<>" then
                    let exp = eenv.Lookup(ide)
                    let funAbs = fenv.Lookup(ide)
                    match funAbs with
                    | EFun(a,b) ->
                        let newExp = updateExpr a venv b fenv
                        let commands, venv1, eenv1, fenv1 = flattenSpatioTemporal cs venv eenv fenv
                        Print(p, name, newExp)::commands, venv, eenv, fenv
                else
                    let newExp = updateExpr exp venv eenv fenv
                    let commands, venv1, eenv1, fenv1 = flattenSpatioTemporal cs venv eenv fenv
                    Print(p, name, newExp)::commands, venv, eenv, fenv
        | _ ->
            let commands, venv1, eenv1, fenv1 = flattenSpatioTemporal cs venv eenv fenv
            (c::(commands), venv1, eenv1, fenv1)