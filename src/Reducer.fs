module VoxLogicA.Reducer

open FSharp.Collections
// SYNTAX
type Expression = 
    | ECall of string * (Expression list)
    | ENumber of float    
type Command = 
    | Declaration of string * (string list) * Expression    
    | Print of string * Expression
    
type Program = Program of list<Command>
    
// SEMANTICS 
type Operator =
    | Number of float
    | Identifier of string

type Task = Task of Operator * List<Task>
type Tasks = 
    | Tasks of List<Task>
    member this.Add op args =
        match this with
        | Tasks tasks ->           
            let t = Task (op,args) 
            (t,Tasks (t::tasks))

type DVal =
    | Task of Task
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

let rec reduceProgramRec<'a> (env : Environment, tasks) (Program p) (cont : Tasks -> 'a) =
    match p with
    | [] -> cont tasks
    | command :: commands ->
        reduceCommand (env, tasks) command <|
            fun (env',tasks') -> 
                reduceProgramRec (env',tasks') (Program commands) cont 

and reduceCommand (env, tasks) command cont =
    match command with
    | Declaration (ide, formalArgs, body) -> 
        cont (env.Bind ide (Fun(env, formalArgs, body)), tasks)
    | Print (str, expr) -> // TODO: use pos
        reduceExpr (env, tasks) expr <|
            fun (taskId, tasks') ->
                cont (env, tasks')

and reduceExpr (env: Environment, tasks: Tasks) (expr : Expression) (cont : Task * Tasks -> 'a) =
    match expr with
    | ENumber f -> cont <| tasks.Add (Number f) []
    | ECall (ide, args) -> // TODO: use pos
        let rec reduceArgs args tasks' accum cont =
            match args with 
            | [] -> cont (List.rev accum,tasks')
            | arg::args' -> 
                reduceExpr (env,tasks') arg <|
                    fun (taskId,tasks'') ->
                        reduceArgs args' tasks'' (taskId::accum) cont

        reduceArgs args tasks [] <|
            fun (actualArgs,tasks') ->
                try
                    match env.Find ide with
                    | Fun (denv, formalArgs, body) ->
                        let callEnv =
                            if formalArgs.Length = args.Length then
                                denv.BindList formalArgs (List.map Task actualArgs)
                            else
                                failwith (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)                                    

                        reduceExpr (callEnv, tasks') body <| cont                            
                    | Task t -> 
                        cont(t, tasks)
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    cont <| tasks'.Add (Identifier ide) actualArgs


let reduceProgram prog =
    reduceProgramRec (emptyEnvironment, Tasks []) prog id

