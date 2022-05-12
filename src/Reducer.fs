module VoxLogicA.Reducer

open Parser
open FSharp.Collections
open ErrorMsg
type Operator =
    | Number of float
    | String of string
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
    | Fun of Environment * List<string> * Parser.Expression

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
        | _ -> fail "Internal error in module Reducer. Please report."

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
    | Print (pos, str, expr) -> // TODO: use pos
        reduceExpr [ ($"print {str}", pos) ] (env, tasks) expr <|
            fun (taskId, tasks') ->
                cont (env, tasks')

and reduceExpr (stack: ErrorMsg.Stack) (env: Environment, tasks: Tasks) (expr : Expression) (cont : Task * Tasks -> 'a) =
    match expr with
    | ENumber f -> cont <| tasks.Add (Number f) []
    | EString s -> cont <| tasks.Add (String s) []
    | ECall (pos, ide, args) -> // TODO: use pos
        let stack' = (ide, pos) :: stack        
        let rec reduceArgs args tasks' accum cont =
            match args with 
            | [] -> cont (List.rev accum,tasks')
            | arg::args' -> 
                reduceExpr stack' (env,tasks') arg <|
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
                                failWithStacktrace
                                    (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)
                                    stack'

                        reduceExpr stack' (callEnv, tasks') body <| cont                            
                    | Task t -> 
                        cont(t, tasks)
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    cont <| tasks'.Add (Identifier ide) actualArgs


let reduceProgram prog =
    reduceProgramRec (emptyEnvironment, Tasks []) prog id

