module VoxLogicA.Task

open Parser
open FSharp.Collections
open ErrorMsg

type identifier = string

type Operator =
    | Identifier of identifier
    | Number of float
    | Bool of bool
    | String of string

type Arguments = List<int>

type Taskid = int

type Task =
    { id: Taskid
      operator: Operator
      arguments: Arguments }

type Tasks =
    private
        { byTerm: Map<(Operator * Arguments), Task>
          byId: Map<Taskid, Task> }

    member this.FindOrCreate operator arguments =
        try
            (this.Find operator arguments, this)
        with
        | :? System.Collections.Generic.KeyNotFoundException ->
            this.Create operator arguments

    member this.Find operator arguments =
        (Map.find (operator, arguments) this.byTerm).id

    member this.Create operator arguments =
        let newId = this.byTerm.Count

        let newTask =
            {   id = newId
                operator = operator
                arguments = arguments }

        let byTerm' = Map.add (operator, arguments) newTask this.byTerm
        let byId' = Map.add newId newTask this.byId
        (newId, { byTerm = byTerm'; byId = byId' })

type Goal =
    | GoalSave of string * Taskid
    | GoalPrint of string * Taskid

type WorkPlan = { tasks: Tasks; goals: Set<Goal> }

type DVal = Task of Taskid | Fun of List<identifier> * Parser.Expression

type private Environment = Environment of Map<identifier, DVal> with
    member this.Find ide = Map.find ide (match this with Environment env -> env)
    member this.Bind ide expr = Environment (Map.add ide expr (match this with Environment env -> env))
    member this.BindList idelist exprlist =
        match (idelist,exprlist) with
        | (ide::ides,expr::exprs) -> 
            (this.Bind ide expr).BindList ides exprs
        | ([],[]) -> this
        | _ -> fail "Internal error in module Reducer. Please report."

let rec private reduceProgram (env, tasks, goals) (Program p) =
    match p with
    | [] -> { tasks = tasks; goals = goals }
    | command :: commands -> reduceProgram (reduceCommand (env, tasks, goals) command) (Program commands)

and private reduceCommand (env, tasks, goals) command =
    match command with
    | Save (pos, filename, expr) -> // TODO: use pos
        let (taskId, tasks') = reduceExpr [] (env, tasks) expr
        (env, tasks', Set.add (GoalSave(filename, taskId)) goals)
    | _ -> failwith "stub"

and private reduceExpr (stack : ErrorMsg.Stack) (env : Environment, tasks) expr =
    match expr with
    | Parser.Number f -> tasks.FindOrCreate (Number f) []
    | Parser.Bool b -> tasks.FindOrCreate (Bool b) []
    | Parser.String s -> tasks.FindOrCreate (String s) []
    | Parser.Call (pos, ide, args) -> // TODO: use pos
        let stack' = (ide,pos)::stack
        let rec reduceArgs args (reducedArgs, tasks') =
            match args with
            | [] -> (List.rev reducedArgs, tasks')
            | arg :: args' ->
                let (taskId, tasks') = reduceExpr stack' (env, tasks) arg
                reduceArgs args' (taskId :: reducedArgs, tasks')

        let (actualArgs, tasks') = reduceArgs args ([], tasks)

        try
            (tasks'.Find (Identifier ide) actualArgs, tasks')
        with
        | :? System.Collections.Generic.KeyNotFoundException -> 
            try 
                match env.Find ide with
                | Fun (formalArgs,body) ->
                    let callEnv = 
                        if formalArgs.Length = args.Length then 
                            env.BindList formalArgs (List.map Task actualArgs)
                        else failWithStacktrace (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length) stack'           
                    reduceExpr stack' (callEnv, tasks') body
                | Task t -> (t,tasks)
            with :? System.Collections.Generic.KeyNotFoundException -> 
                tasks'.Create (Identifier ide) actualArgs 
