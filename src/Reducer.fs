module VoxLogicA.Reducer

open Parser
open FSharp.Collections
open ErrorMsg

type identifier = string

type Operator =
    | Identifier of identifier
    | Number of float
    | Bool of bool
    | String of string
    override this.ToString() =
        match this with
        | Identifier x -> x
        | Number x -> x.ToString()
        | Bool x -> x.ToString()
        | String x -> x.ToString()

type Arguments = List<int>

type Taskid = int

// "Internal" representation of tasks; differs from the "external" because Taskid is not needed as a field after reduction
type private TaskInt = 
    { id: Taskid
      operator: Operator
      arguments: Arguments }

type private Tasks =
    { byTerm: Map<(Operator * Arguments), TaskInt>
      byId: Map<Taskid, TaskInt> }

    member this.FindOrCreate operator arguments =
        try
            (this.Find operator arguments, this)
        with
        | :? System.Collections.Generic.KeyNotFoundException -> this.Create operator arguments

    member this.Find operator arguments =
        (Map.find (operator, arguments) this.byTerm).id

    member this.Create operator arguments =
        let newId = this.byId.Count

        let newTask =
            { id = newId
              operator = operator
              arguments = arguments }

        let byTerm' = Map.add (operator, arguments) newTask this.byTerm
        let byId' = Map.add newId newTask this.byId
        (newId, { byTerm = byTerm'; byId = byId' })

    member this.Alias operator arguments (taskid : Taskid) =
        let task = Map.find taskid this.byId
        let byTerm' = Map.add (operator, arguments) task this.byTerm
        { byTerm = byTerm'; byId = this.byId }


let private emptyTasks = { byTerm = Map.empty; byId = Map.empty }

type Goal =
    | GoalSave of string * Taskid
    | GoalPrint of string * Taskid

type private DVal =
    | Task of Taskid
    | Fun of Environment * List<identifier> * Parser.Expression

and private Environment =
    | Environment of Map<identifier, DVal>
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

let private emptyEnvironment = Environment Map.empty

let rec private reduceProgramRec (env, tasks, goals) (Program p) =
    match p with
    | [] -> (tasks, goals)
    | command :: commands -> reduceProgramRec (reduceCommand (env, tasks, goals) command) (Program commands)

and private reduceCommand (env, tasks, goals) command =
    match command with
    | Save (pos, filename, expr) -> // TODO: use pos
        let (taskId, tasks') = reduceExpr [ ($"save {filename}", pos) ] (env, tasks) expr
        (env, tasks', Set.add (GoalSave(filename, taskId)) goals)
    | Declaration (ide, formalArgs, body) -> (env.Bind ide (Fun(env, formalArgs, body)), tasks, goals)
    | Print (pos, str, expr) -> // TODO: use pos
        let (taskId, tasks') = reduceExpr [ ($"print {str}", pos) ] (env, tasks) expr
        (env, tasks', Set.add (GoalPrint(str, taskId)) goals)
    | _ -> failwith "stub"

and private reduceExpr (stack: ErrorMsg.Stack) (env: Environment, tasks: Tasks) expr =
    match expr with
    | ENumber f -> tasks.FindOrCreate (Number f) []
    | EBool b -> tasks.FindOrCreate (Bool b) []
    | EString s -> tasks.FindOrCreate (String s) []
    | ECall (pos, ide, args) -> // TODO: use pos
        let stack' = (ide, pos) :: stack

        let rec reduceArgs args (reducedArgs, tasks') =
            match args with
            | [] -> (List.rev reducedArgs, tasks')
            | arg :: args' ->
                let (taskId, tasks'') = reduceExpr stack' (env, tasks') arg
                reduceArgs args' (taskId :: reducedArgs, tasks'')

        // TODO: before reduction, one should already have checked if ide is defined in env, and then if the number of arguments matches
        let (actualArgs, tasks') = reduceArgs args ([], tasks)

        try
            (tasks'.Find (Identifier ide) actualArgs, tasks') 
        with
        | :? System.Collections.Generic.KeyNotFoundException ->
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
                    let (task'',tasks'') = reduceExpr stack' (callEnv, tasks') body
                    let tasks''' = tasks''.Alias (Identifier ide) actualArgs task''
                    (task'',tasks''')
                | Task t -> (t, tasks)                
            with
            | :? System.Collections.Generic.KeyNotFoundException -> tasks'.Create (Identifier ide) actualArgs

type Task =
    { operator: Operator
      arguments: list<int> }

let private taskOfTaskInt (t: TaskInt) : Task =
    { operator = t.operator
      arguments = t.arguments }
      
type WorkPlan =
    { tasks: array<Task>
      goals: Set<Goal> }

    member this.AsDot =
        let mutable str = "digraph {"

        for i = 0 to this.tasks.Length - 1 do
            let task = this.tasks[i]

            str <-
                str
                + $"{i} [label=\"{task.operator.ToString()}\"];\n"

            for argument in task.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"

let reduceProgram prog =
    let (tasks, goals) = reduceProgramRec (emptyEnvironment, emptyTasks, Set.empty) prog

    { tasks = Array.init tasks.byId.Count (fun i -> 
        try taskOfTaskInt tasks.byId[i]
        with _ ->             
            fail $"{tasks.byId}\n{tasks.byTerm}\nnot found id {i}")
      goals = goals }
