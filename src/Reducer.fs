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

type Task =
    { operator: Operator
      arguments: Arguments }

    override this.ToString() =
        let sep = ","
        let args = 
            if this.arguments.Length > 0 
            then $"({String.concat sep (List.map (fun x -> x.ToString()) this.arguments)})"
            else ""
        $"{this.operator}{args}"

type Taskid = int

// "Internal" representation of tasks; differs from the "external" because Taskid is not needed after reduction
type private TaskInt = { id: Taskid; task: Task }


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
              task =
                { operator = operator
                  arguments = arguments } }

        let byTerm' = Map.add (operator, arguments) newTask this.byTerm
        let byId' = Map.add newId newTask this.byId
        (newId, { byTerm = byTerm'; byId = byId' })

    member this.Alias operator arguments (taskid: Taskid) =
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

let rec private reduceProgramRec<'a> (env : Environment, tasks, goals) (Program p) (cont : Tasks * Set<Goal> -> 'a) =
    match p with
    | [] -> cont (tasks, goals)
    | command :: commands ->
        reduceCommand (env, tasks, goals) command <|
            fun (env',tasks',goals') -> 
                reduceProgramRec (env',tasks',goals') (Program commands) cont 

and private reduceCommand (env, tasks, goals) command cont =
    match command with
    | Save (pos, filename, expr) -> // TODO: use pos
        reduceExpr [ ($"save {filename}", pos) ] (env, tasks) expr <|
            fun (taskId, tasks') ->
                cont (env, tasks', Set.add (GoalSave(filename, taskId)) goals)
    | Declaration (ide, formalArgs, body) -> 
        cont (env.Bind ide (Fun(env, formalArgs, body)), tasks, goals)
    | Print (pos, str, expr) -> // TODO: use pos
        reduceExpr [ ($"print {str}", pos) ] (env, tasks) expr <|
            fun (taskId, tasks') ->
                cont (env, tasks', Set.add (GoalPrint(str, taskId)) goals)
    | _ -> failwith "stub"

and private reduceExpr (stack: ErrorMsg.Stack) (env: Environment, tasks: Tasks) (expr : Expression) (cont : Taskid * Tasks -> 'a) =
    match expr with
    | ENumber f -> cont <| tasks.FindOrCreate (Number f) []
    | EBool b -> cont <| tasks.FindOrCreate (Bool b) []
    | EString s -> cont <| tasks.FindOrCreate (String s) []
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
                cont <| (tasks'.Find (Identifier ide) actualArgs, tasks')
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

                        reduceExpr stack' (callEnv, tasks') body <|
                            fun (task'', tasks'') ->                         
                                let tasks''' = tasks''.Alias (Identifier ide) actualArgs task''
                                cont (task'', tasks''')
                    | Task t -> 
                        cont(t, tasks)
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    cont <| tasks'.Create (Identifier ide) actualArgs
type WorkPlan =
    { tasks: array<Task>
      goals: Set<Goal> }

    override this.ToString() =
        let t = String.concat "\n" <| Array.mapi (fun i el -> $"{i} -> {el}") this.tasks
        $"goals: STUB\ntasks:\n{t}" 

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
    let (tasks, goals) = reduceProgramRec (emptyEnvironment, emptyTasks, Set.empty) prog id
    { tasks = Array.init tasks.byId.Count (fun i -> tasks.byId[i].task)
      goals = goals }



(* get enumerator of anything 
let inline private getEnumeratorFromArrayLike x =
    let inline count (t: ^T) : int = (^T : (member Count : int) (t))
    let inline item (t: ^T) (x: int) : ^a = (^T : (member get_Item: int -> ^a) (t, x))
    (seq { for i = 0 to count x - 1 do yield item x i }).GetEnumerator()

type MyThing (length: int) =
    member x.Count = length
    member x.Item with get (c) =
        if c < length then
            c
        else
            failwith "boom"
    interface System.Collections.Generic.IEnumerable<int> with
        member x.GetEnumerator() = getEnumeratorFromArrayLike x
        member x.GetEnumerator() : System.Collections.IEnumerator = getEnumeratorFromArrayLike x :> _

let m = MyThing(3)
for i in m do printfn "MyThing: %d" i
*)

(* Another way

let inline private getEnumeratorFromArrayLike x =
    let inline count (t: ^T) : int = (^T : (member Count : int) (t))
    let inline item (t: ^T) (x: int) : ^a = (^T : (member get_Item: int -> ^a) (t, x))
    (seq { for i = 0 to count x - 1 do yield item x i }).GetEnumerator()

type MyThing (length: int) =
    member x.Count = length
    member x.Item with get (c) =
        if c < length then
            c
        else
            failwith "boom"
    interface System.Collections.Generic.IEnumerable<int> with
        member x.GetEnumerator() = getEnumeratorFromArrayLike x
        member x.GetEnumerator() : System.Collections.IEnumerator = getEnumeratorFromArrayLike x :> _

let m = MyThing(3)
for i in m do printfn "MyThing: %d" i
*)

(* even simpler

open System.Collections.Generic
module CustomList =
    let getEnumerator<'t> count (item:int -> 't) =
        seq {
            for i in 0..count()-1 do
                yield item(i)
        } :?> IEnumerator<'t>

    type CustomList<'t>(list: ResizeArray<'t>) =
        let count () = list.Count
        let item index = list.[index]

        member this.Count with get () = count ()
        member this.Item with get(index:int) = item index
        member this.GetEnumerator(): IEnumerator<'t> = getEnumerator count item
        
*)