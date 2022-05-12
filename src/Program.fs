// SYNTAX
type Expression =
    | ECall of string * (Expression list)
    | EDeclaration of string * (string list) * Expression * Expression
type DVal =
    | Result of unit
    | Fun of Environment * List<string> * Expression

and Environment = Map<string, DVal>

let rec bindList env idelist exprlist = 
        match (idelist, exprlist) with
        | (ide :: ides, expr :: exprs) -> bindList (Map.add ide expr env) ides exprs
        | ([], []) -> env
        | _ -> failwith "Internal error in module Reducer. Please report."

let rec reduceExpr (env: Environment) (expr: Expression) (cont: unit -> unit) =
    match expr with
    | EDeclaration (ide, formalArgs, body, rest) -> 
        let ev' = Map.add ide (Fun(env, formalArgs, body)) env
        reduceExpr ev' rest cont 
    | ECall (_,[]) -> cont ()
    | ECall (ide, args) -> // TODO: use pos
        let rec reduceArgs args accum cont =
            match args with
            | [] -> cont (List.rev accum)
            | arg :: args' ->
                reduceExpr env arg
                <| fun eval -> reduceArgs args' (eval :: accum) cont

        reduceArgs args []
        <| fun actualArgs ->
            match (Map.find ide env) with
            | (Fun (denv, formalArgs, body)) ->
                let callEnv =
                    if formalArgs.Length = args.Length then
                        bindList denv formalArgs (List.map Result actualArgs)
                    else
                        failwith (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)

                reduceExpr callEnv body <| cont
            | (Result r) -> cont r          

let rec mkExpr n cont =
    if n = 0 
    then cont (EDeclaration("t0", [ "x" ], ECall("x", []), ECall("t0",[ ECall("x",[]) ] )))
    else mkExpr (n-1) <| fun e -> cont (EDeclaration($"t{n+1}", ["x"], ECall($"t{n}", [ ECall("x", []) ]), e))
    
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

