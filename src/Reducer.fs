module VoxLogicA.Reducer


open System.Collections.Generic

type identifier = string
type WorkUnitid = int

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

type Arguments = seq<WorkUnitid>

type WorkUnit =
    {   operator: Operator
        arguments: Arguments }

    override this.ToString() =
        let sep = ","

        let args =
            if Seq.length this.arguments > 0 then
                $"({String.concat sep (Seq.map (fun x -> x.ToString()) this.arguments)})"
            else
                ""

        $"{this.operator}{args}"

type Goal =
    | GoalSave of string * WorkUnitid
    | GoalPrint of string * WorkUnitid

module private Internals =

    open Parser
    open ErrorMsg


    // "Internal" representation of workUnits; differs from the "external" because WorkUnitid is not needed after reduction
    type WorkUnitInt = { id: WorkUnitid; workUnit: WorkUnit }

    let memoize = true

    type WorkUnits =
        { byTerm: Dictionary<(Operator * Arguments), WorkUnitInt>
          byId: Dictionary<WorkUnitid, WorkUnitInt> }

        member this.FindOrCreate operator arguments =
            if memoize then
                match this.TryFind operator arguments with
                | Some workUnitId -> workUnitId
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

            let newWorkUnit =
                { id = newId
                  workUnit =
                    { operator = operator
                      arguments = arguments } }

            if memoize then
                this.byTerm[ (operator, arguments) ] <- newWorkUnit

            this.byId[ newId ] <- newWorkUnit
            newId


        member this.Alias operator arguments (workUnitId: WorkUnitid) =
            let workUnit = this.byId[workUnitId]
            this.byTerm[ (operator, arguments) ] <- workUnit


    let emptyWorkUnits () =
        { byTerm = new Dictionary<_, _>(10000)
          byId = new Dictionary<_, _>(10000) }
    type DVal =
        | WorkUnit of WorkUnitid
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
        (env: Environment, workUnits, (goals: HashSet<Goal>), parsedImports: HashSet<string>)
        (Program p)
        (cont: WorkUnits -> 'a)
        =
        match p with
        | [] -> cont workUnits
        | command :: commands ->
            reduceCommand (env, workUnits, goals, parsedImports) command
            <| fun (env', imports) ->
                let newProgram = (Program(List.concat [ imports; commands ]))
                reduceProgramRec (env', workUnits, goals, parsedImports) newProgram cont

    and reduceCommand
        (env, workUnits, (goals: HashSet<Goal>), parsedImports)
        command
        (cont: Environment * list<Command> -> 'a)
        =
        match command with
        | Save (pos, filename, expr) -> // TODO: use pos
            reduceExpr [ ($"save {filename}", pos) ] (env, workUnits) expr
            <| fun workUnitId ->
                ignore <| goals.Add(GoalSave(filename, workUnitId))
                cont (env, [])
        | Declaration (ide, formalArgs, body) -> cont (env.Bind ide (Fun(env, formalArgs, body)), [])
        | Print (pos, str, expr) -> // TODO: use pos
            reduceExpr [ ($"print {str}", pos) ] (env, workUnits) expr
            <| fun workUnitId ->
                ignore <| goals.Add(GoalPrint(str, workUnitId))
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
        (env: Environment, workUnits: WorkUnits)
        (expr: Expression)
        (cont: WorkUnitid -> 'a)
        =
        match expr with
        | ENumber f -> cont <| workUnits.FindOrCreate (Number f) []
        | EBool b -> cont <| workUnits.FindOrCreate (Bool b) []
        | EString s -> cont <| workUnits.FindOrCreate (String s) []
        | ECall (pos, ide, args) -> // TODO: use pos
            let stack' = (ide, pos) :: stack

            let rec reduceArgs args accum cont =
                match args with
                | [] -> cont (List.rev accum)
                | arg :: args' ->
                    reduceExpr stack' (env, workUnits) arg
                    <| fun workUnitId -> reduceArgs args' (workUnitId :: accum) cont

            reduceArgs args []
            <| fun actualArgs ->

                match workUnits.TryFind (Identifier ide) actualArgs with
                | Some workUnit'' ->
                    if memoize then
                        cont workUnit''
                    else
                        fail "Found workUnit in cache without memoization, which is impossible. Please report."
                | None ->
                    match env.TryFind ide with
                    | Some (Fun (denv, formalArgs, body)) ->
                        let callEnv =
                            if formalArgs.Length = args.Length then
                                denv.BindList formalArgs (List.map WorkUnit actualArgs)
                            else
                                failWithStacktrace
                                    (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)
                                    stack'

                        reduceExpr stack' (callEnv, workUnits) body
                        <| fun workUnit'' ->
                            workUnits.Alias (Identifier ide) actualArgs workUnit''
                            cont workUnit''
                    | Some (WorkUnit t) -> cont t
                    | None -> cont <| workUnits.Create (Identifier ide) actualArgs
// with
// | :? System.Collections.Generic.KeyNotFoundException ->
//
type WorkPlan =
    { workUnits: array<WorkUnit>
      goals: array<Goal> }

    override this.ToString() =
        let t =
            String.concat "\n"
            <| Array.mapi (fun i el -> $"{i} -> {el}") this.workUnits

        let g =
            String.concat "," <| Array.map (fun x -> x.ToString()) this.goals

        $"goals: {g}\nworkUnits:\n{t}"

    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.workUnits.Length - 1 do
            let workUnit = this.workUnits[i]

            str <-
                str
                + $"{i} [label=\"[{i}] {workUnit.ToString()}\"];\n"

            for argument in workUnit.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"

let reduceProgram prog =
    let goals = new HashSet<_>()

    let workUnits =
        Internals.reduceProgramRec (Internals.emptyEnvironment, Internals.emptyWorkUnits (), goals, new HashSet<_>()) prog id

    { workUnits = Array.init workUnits.byId.Count (fun i -> workUnits.byId[i].workUnit)
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
