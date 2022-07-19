module VoxLogicA.Reducer

open System.Collections.Generic

type identifier = string
type Taskid = int

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

type Arguments = list<Taskid>

type Task =
    {   operator: Operator
        arguments: Arguments }

    override this.ToString() =
        let sep = ","

        let args =
            if this.arguments.Length > 0 then
                $"({String.concat sep (List.map (fun x -> x.ToString()) this.arguments)})"
            else
                ""

        $"{this.operator}{args}"

type Goal =
    | GoalSave of string * Taskid
    | GoalPrint of string * Taskid

module private Internals =

    open Parser
    open ErrorMsg


    // "Internal" representation of tasks; differs from the "external" because Taskid is not needed after reduction
    type TaskInt = { id: Taskid; task: Task }

    let memoize = true

    type Tasks =
        { byTerm: Dictionary<(Operator * Arguments), TaskInt>
          byId: Dictionary<Taskid, TaskInt> }

        member this.FindOrCreate operator arguments =
            if memoize then
                match this.TryFind operator arguments with
                | Some taskId -> taskId
                | None -> this.Create operator arguments
            else
                this.Create operator arguments

        member this.TryFind operator arguments =
            if memoize then
                if this.byTerm.ContainsKey(operator, arguments) then
                    Some this.byTerm[operator, arguments].id
                else
                    None
            else
                None

        member this.Create operator arguments =
            let newId = this.byId.Count

            let newTask =
                { id = newId
                  task =
                    { operator = operator
                      arguments = arguments } }

            if memoize then
                this.byTerm[ (operator, arguments) ] <- newTask

            this.byId[ newId ] <- newTask
            newId


        member this.Alias operator arguments (taskid: Taskid) =
            let task = this.byId[taskid]
            this.byTerm[ (operator, arguments) ] <- task


    let emptyTasks () =
        { byTerm = new Dictionary<_, _>(10000)
          byId = new Dictionary<_, _>(10000) }
    type DVal =
        | Task of Taskid
        | Fun of Environment * list<identifier> * Parser.Expression

    and Environment =
        | Environment of Map<identifier, DVal>
        member this.TryFind ide =
            Map.tryFind
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

    let rec reduceProgramRec<'a>
        (env: Environment, tasks, (goals: HashSet<Goal>), parsedImports: HashSet<string>)
        (Program p)
        (cont: Tasks -> 'a)
        =
        match p with
        | [] -> cont tasks
        | command :: commands ->
            reduceCommand (env, tasks, goals, parsedImports) command
            <| fun (env', imports) ->
                let newProgram = (Program(List.concat [ imports; commands ]))
                reduceProgramRec (env', tasks, goals, parsedImports) newProgram cont

    and reduceCommand
        (env, tasks, (goals: HashSet<Goal>), parsedImports)
        command
        (cont: Environment * list<Command> -> 'a)
        =
        match command with
        | Save (pos, filename, expr) -> // TODO: use pos
            reduceExpr [ ($"save {filename}", pos) ] (env, tasks) expr
            <| fun taskId ->
                ignore <| goals.Add(GoalSave(filename, taskId))
                cont (env, [])
        | Declaration (ide, formalArgs, body) -> cont (env.Bind ide (Fun(env, formalArgs, body)), [])
        | Print (pos, str, expr) -> // TODO: use pos
            reduceExpr [ ($"print {str}", pos) ] (env, tasks) expr
            <| fun taskId ->
                ignore <| goals.Add(GoalPrint(str, taskId))
                cont (env, [])
        | Import filename ->
            let libdir =
                $"{System.IO.Path.GetDirectoryName(
                       System
                           .Diagnostics
                           .Process
                           .GetCurrentProcess()
                           .MainModule
                           .FileName
                   )}/imgql"

            let find filename =
                if System.IO.File.Exists filename then
                    Some filename
                else
                    let nfilename = filename + ".imgql"

                    if System.IO.File.Exists nfilename then
                        Some nfilename
                    else
                        None

            let path =
                let try1 = System.IO.Path.GetFullPath filename

                match find try1 with
                | Some try1 -> try1
                | None ->
                    if not (filename.StartsWith "/") then
                        let try2 = System.IO.Path.GetFullPath(System.IO.Path.Combine(libdir, filename))

                        match find try2 with
                        | Some try2 -> try2
                        | None -> fail $"Import '{filename}' not found in current dir or {libdir}"
                    else
                        raise <| fail $"Import '{filename}' not found"

            if not (parsedImports.Contains(path)) then
                ErrorMsg.Logger.Debug
                <| sprintf "Importing file \"%s\"" path

                let parsed = parseImport path
                cont (env, parsed)
            else
                cont (env, [])

    and reduceExpr
        (stack: ErrorMsg.Stack)
        (env: Environment, tasks: Tasks)
        (expr: Expression)
        (cont: Taskid -> 'a)
        =
        match expr with
        | ENumber f -> cont <| tasks.FindOrCreate (Number f) []
        | EBool b -> cont <| tasks.FindOrCreate (Bool b) []
        | EString s -> cont <| tasks.FindOrCreate (String s) []
        | ECall (pos, ide, args) -> // TODO: use pos
            let stack' = (ide, pos) :: stack

            let rec reduceArgs args accum cont =
                match args with
                | [] -> cont (List.rev accum)
                | arg :: args' ->
                    reduceExpr stack' (env, tasks) arg
                    <| fun taskId -> reduceArgs args' (taskId :: accum) cont

            reduceArgs args []
            <| fun actualArgs ->

                match tasks.TryFind (Identifier ide) actualArgs with
                | Some task'' ->
                    if memoize then
                        cont task''
                    else
                        fail "Found task in cache without memoization, which is impossible. Please report."
                | None ->
                    match env.TryFind ide with
                    | Some (Fun (denv, formalArgs, body)) ->
                        let callEnv =
                            if formalArgs.Length = args.Length then
                                denv.BindList formalArgs (List.map Task actualArgs)
                            else
                                failWithStacktrace
                                    (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)
                                    stack'

                        reduceExpr stack' (callEnv, tasks) body
                        <| fun task'' ->
                            tasks.Alias (Identifier ide) actualArgs task''
                            cont task''
                    | Some (Task t) -> cont t
                    | None -> cont <| tasks.Create (Identifier ide) actualArgs
// with
// | :? System.Collections.Generic.KeyNotFoundException ->
//
type WorkPlan =
    { tasks: array<Task>
      goals: array<Goal> }

    override this.ToString() =
        let t =
            String.concat "\n"
            <| Array.mapi (fun i el -> $"{i} -> {el}") this.tasks

        let g =
            String.concat "," <| Array.map (fun x -> x.ToString()) this.goals

        $"goals: {g}\ntasks:\n{t}"

    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.tasks.Length - 1 do
            let task = this.tasks[i]

            str <-
                str
                + $"{i} [label=\"[{i}] {task.ToString()}\"];\n"

            for argument in task.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"

let reduceProgram prog =
    let goals = new HashSet<_>()

    let tasks =
        Internals.reduceProgramRec (Internals.emptyEnvironment, Internals.emptyTasks (), goals, new HashSet<_>()) prog id

    { tasks = Array.init tasks.byId.Count (fun i -> tasks.byId[i].task)
      goals = Array.ofSeq goals }

(* get enumerator of anything
    let inline getEnumeratorFromArrayLike x =
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

    let inline getEnumeratorFromArrayLike x =
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
