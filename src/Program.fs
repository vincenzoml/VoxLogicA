// SYNTAX
type Expression =
    | ECall of string * (Expression list)
type Command =
    | Declaration of string * (string list) * Expression
    
type Program = Program of (list<Command> * Expression)

type DVal =
    | Result of unit
    | Fun of Environment * List<string> * Expression

and Environment = Map<string, DVal>

let rec bindList env idelist exprlist = 
        match (idelist, exprlist) with
        | (ide :: ides, expr :: exprs) -> bindList (Map.add ide expr env) ides exprs
        | ([], []) -> env
        | _ -> failwith "Internal error in module Reducer. Please report."

let rec reduceProgramRec (env: Environment) (Program (p,ret)) cont =
    match p with
    | [] -> reduceExpr env ret cont
    | command :: commands ->
        reduceCommand env command
        <| fun env' -> reduceProgramRec env' (Program (commands,ret)) cont

and reduceCommand env command cont =
    match command with
    | Declaration (ide, formalArgs, body) -> cont (Map.add ide (Fun(env, formalArgs, body)) env)

and reduceExpr (env: Environment) (expr: Expression) (cont: unit -> unit) =
    match expr with
    | ECall (_,[]) -> ()
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

let reduceProgram prog =
    reduceProgramRec Map.empty prog id

let prog n =
    let declarations =
        (Declaration("t0", [ "x" ], ECall("x", [])))
        :: (List.init n (fun i -> Declaration($"t{i + 1}", [ "x" ], ECall($"t{i}", [ ECall("x", []) ]))))

    (declarations, ECall($"t{n}", [ ECall ("finish",[]) ]))

let (decl,ret) =
    prog
    <| try
        (int (System.Environment.GetCommandLineArgs()[1]))
       with
       | _ -> 1000000

printfn $"Program length: {decl.Length}"
let redux = reduceProgram (Program (decl,ret))
printfn $"{redux}"

