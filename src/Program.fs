// SYNTAX
type Expression =
    | ECall of string * Expression
    | EConst
    | EDeclaration of string * string * Expression * Expression
type DVal =
    | Result of unit
    | Fun of Environment * string * Expression

and Environment = Map<string, DVal>

let rec reduceExpr (env: Environment) (expr: Expression) (cont: unit -> unit) =
    match expr with
    | EDeclaration (ide, formalArgs, body, rest) -> 
        let ev' = Map.add ide (Fun(env, formalArgs, body)) env
        reduceExpr ev' rest cont 
    | EConst -> cont ()
    | ECall (ide, arg) ->
        reduceExpr env arg <|
            fun actualArg ->
                match (Map.find ide env) with
                | (Fun (denv, formalArg, body)) ->
                    let callEnv = Map.add formalArg (Result actualArg) env                                                           
                    reduceExpr callEnv body <| cont
                | (Result r) -> cont r          

let rec mkExpr n cont =
    if n = 0 
    then cont (EDeclaration("t0", "x" , EConst, ECall("t0", EConst )))
    else mkExpr (n-1) <| fun e -> cont (EDeclaration($"t{n+1}", "x", ECall($"t{n}", EConst), e))
    
let n = 
    try
        (int (System.Environment.GetCommandLineArgs()[1]))
    with
       | _ -> 1000000        

mkExpr n <|
    fun expr ->
        printfn $"Expression prepared"
        reduceExpr Map.empty expr <|
            fun redux -> 
                printfn $"Done ({redux})"
        printfn "all done"
