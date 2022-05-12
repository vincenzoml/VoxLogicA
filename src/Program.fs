// SYNTAX
type Expression = 
    | ECall of string * (Expression list)
    | ENumber of float    
type Command = 
    | Declaration of string * (string list) * Expression    
    | Print of Expression
    
type Program = Program of list<Command>
    
// SEMANTICS 
type Eval = float

type DVal =
    | Result of Eval
    | Fun of Environment * List<string> * Expression

and Environment =
    | Environment of Map<string, DVal>
    member this.Find ide =
        Map.find
            ide
            (match this with
             | Environment env -> env)

    member this.Bind ide expr =
        Environment(
            Map.add
                ide
                expr
                (match this with
                 | Environment env -> env)
        )

    member this.BindList idelist exprlist =
        match (idelist, exprlist) with
        | (ide :: ides, expr :: exprs) -> (this.Bind ide expr).BindList ides exprs
        | ([], []) -> this
        | _ -> failwith "Internal error in module Reducer. Please report."

let emptyEnvironment = Environment Map.empty

let rec reduceProgramRec<'a> (env : Environment,prints : List<Eval>) (Program p) (cont : List<Eval> -> 'a) =
    match p with
    | [] -> cont (List.rev prints)
    | command :: commands ->
        reduceCommand (env, prints) command <|
            fun (env',tasks') -> 
                reduceProgramRec (env',tasks') (Program commands) cont 

and reduceCommand (env, prints) command cont =
    match command with
    | Declaration (ide, formalArgs, body) -> 
        cont (env.Bind ide (Fun(env, formalArgs, body)), prints)
    | Print expr -> // TODO: use pos
        reduceExpr env expr <|
            fun result ->
                cont (env, result::prints)

and reduceExpr (env: Environment) (expr : Expression) (cont : Eval -> 'a) =
    match expr with
    | ENumber f -> cont f
    | ECall (ide, args) -> // TODO: use pos
        let rec reduceArgs args accum cont =
            match args with 
            | [] -> cont (List.rev accum)
            | arg::args' -> 
                reduceExpr env arg <|
                    fun eval ->
                        reduceArgs args' (eval::accum) cont

        reduceArgs args [] <|
            fun actualArgs ->
                try
                    match env.Find ide with
                    | Fun (denv, formalArgs, body) ->
                        let callEnv =
                            if formalArgs.Length = args.Length then
                                denv.BindList formalArgs (List.map Result actualArgs)
                            else
                                failwith (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)                                    

                        reduceExpr callEnv body <| cont                            
                    | Result r -> 
                        cont r
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    failwith $"Undefined identifier {ide}"


let reduceProgram prog =
    reduceProgramRec (emptyEnvironment, []) prog id

let prog n = 
    let declarations = (Declaration ("t0",["x"],ECall ("x",[])))::(List.init n (fun i -> Declaration ($"t{i+1}",["x"],ECall ($"t{i}",[ECall ("x",[])]) )))
    (List.append declarations [Print (ECall ($"t{n}",[ENumber 0]))])

let p = prog <| try (int (System.Environment.GetCommandLineArgs()[1])) with _ -> 100000
printfn $"Program length: {p.Length}"
let redux = reduceProgram (Program p)
printfn $"Redux length: {redux.Length}"

// To run: dotnet run -c release 100000 (stack overflow); smaller numbers will works (try 10)