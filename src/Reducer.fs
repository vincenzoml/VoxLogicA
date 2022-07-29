module VoxLogicA.Reducer


open System.Collections.Generic

type identifier = string
type OperationId = int

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

type Arguments = seq<OperationId>

type Operation =
    { operator: Operator
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
    | GoalSave of string * OperationId
    | GoalPrint of string * OperationId

module private Internals =

    open Parser
    open ErrorMsg


    // "Internal" representation of operations; differs from the "external" because OperationId is not needed after reduction
    type InternalOperation =
        { id: OperationId
          operation: Operation }

    let memoize = true

    type Operations =
        { byTerm: Dictionary<(Operator * Arguments), InternalOperation>
          byId: Dictionary<OperationId, InternalOperation> }

        member this.FindOrCreate operator arguments =
            if memoize then
                match this.TryFind operator arguments with
                | Some operationId -> operationId
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

            let newOperation =
                { id = newId
                  operation =
                    { operator = operator
                      arguments = arguments } }

            if memoize then
                this.byTerm[ (operator, arguments) ] <- newOperation

            this.byId[ newId ] <- newOperation
            newId


        member this.Alias operator arguments (operationId: OperationId) =
            let operation = this.byId[operationId]
            this.byTerm[ (operator, arguments) ] <- operation


    let emptyOperations () =
        { byTerm = new Dictionary<_, _>()
          byId = new Dictionary<_, _>() }

    type DVal =
        | Operation of OperationId
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

    let rec reduceProgramRec
        (env: Environment, operations, (goals: HashSet<Goal>), parsedImports: HashSet<string>)
        (pRef: ref<list<Command>>)
        =
        match pRef.Value with
        | [] -> operations
        | command :: commands ->
            pRef.Value <- commands
            let env', imports = reduceCommand (env, operations, goals, parsedImports) command id
            let newProgram = List.concat [ imports; pRef.Value ]
            pRef.Value <- newProgram
            reduceProgramRec (env', operations, goals, parsedImports) pRef

    and reduceCommand (env, operations, (goals: HashSet<Goal>), parsedImports) command (cont) =
        match command with
        | Save (pos, filename, expr) -> // TODO: use pos
            reduceExpr [ ($"save {filename}", pos) ] (env, operations) expr
            <| fun operationId ->
                ignore
                <| goals.Add(GoalSave(filename, operationId))

                cont (env, [])
        | Declaration (ide, formalArgs, body) -> cont (env.Bind ide (Fun(env, formalArgs, body)), [])
        | Print (pos, str, expr) -> // TODO: use pos
            reduceExpr [ ($"print {str}", pos) ] (env, operations) expr
            <| fun operationId ->
                ignore <| goals.Add(GoalPrint(str, operationId))
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

    and reduceExpr (stack: ErrorMsg.Stack) (env: Environment, operations: Operations) (expr: Expression) (cont) =
        match expr with
        | ENumber f -> cont <| operations.FindOrCreate (Number f) []
        | EBool b -> cont <| operations.FindOrCreate (Bool b) []
        | EString s -> cont <| operations.FindOrCreate (String s) []
        | ECall (pos, ide, args) -> // TODO: use pos
            let stack' = (ide, pos) :: stack

            let rec reduceArgs args accum cont =
                match args with
                | [] -> cont (List.rev accum)
                | arg :: args' ->
                    reduceExpr stack' (env, operations) arg
                    <| fun operationId -> reduceArgs args' (operationId :: accum) cont

            reduceArgs args []
            <| fun actualArgs ->

                match operations.TryFind (Identifier ide) actualArgs with
                | Some operation'' ->
                    if memoize then
                        cont operation''
                    else
                        fail "Found operation in cache without memoization, which is impossible. Please report."
                | None ->
                    match env.TryFind ide with
                    | Some (Fun (denv, formalArgs, body)) ->
                        let callEnv =
                            if formalArgs.Length = args.Length then
                                denv.BindList formalArgs (List.map Operation actualArgs)
                            else
                                failWithStacktrace
                                    (sprintf "%s requires %d arguments, got %d" ide formalArgs.Length args.Length)
                                    stack'

                        reduceExpr stack' (callEnv, operations) body
                        <| fun operation'' ->
                            operations.Alias (Identifier ide) actualArgs operation''
                            cont operation''
                    | Some (Operation t) -> cont t
                    | None ->
                        cont
                        <| operations.Create (Identifier ide) actualArgs
// with
// | :? System.Collections.Generic.KeyNotFoundException ->
//
type WorkPlan =
    { operations: array<Operation>
      goals: array<Goal> }

    override this.ToString() =
        let t =
            String.concat "\n"
            <| Array.mapi (fun i el -> $"{i} -> {el}") this.operations

        let g =
            String.concat ","
            <| Array.map (fun x -> x.ToString()) this.goals

        $"goals: {g}\noperations:\n{t}"

    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.operations.Length - 1 do
            let operation = this.operations[i]

            str <-
                str
                + $"{i} [label=\"[{i}] {operation.ToString()}\"];\n"

            for argument in operation.arguments do
                str <- str + $"{i} -> {argument};\n"

        str + "\n}"

let reduceProgram (Parser.Program prog) =
    let goals = new HashSet<_>()

    let pRef = ref prog

    let operations =
        Internals.reduceProgramRec
            (Internals.emptyEnvironment, Internals.emptyOperations (), goals, new HashSet<_>())
            pRef

    { operations = Array.init operations.byId.Count (fun i -> operations.byId[i].operation)
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
