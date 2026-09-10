module VoxLogicA.Reducer


open System.Collections.Generic
open Parser

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
        | Number x -> numberToSyntax x
        | Bool x -> boolToSyntax x
        | String x -> x.ToString()

// A list, and not a sequence: the arguments are half of the key under which the
// reduction memoises an operation, so they have to compare and hash by what they
// contain. A sequence would compare by reference, every lookup would miss, and
// the sharing of the common subformulas -- the reason the reduction exists --
// would be gone with nothing to show for it.
type Arguments = list<OperationId>

type Operation =
    { operator: Operator
      arguments: Arguments }

    override this.ToString() =
        let sep = ","

        let args =
            if not (List.isEmpty this.arguments) then
                $"({String.concat sep (List.map (fun x -> x.ToString()) this.arguments)})"
            else
                ""

        $"{this.operator}{args}" // Uncomment to show the args


type Goal =
    | GoalSave of string * OperationId
    | GoalPrint of string * OperationId

type WorkPlan =
    { operations: array<Operation>
      goals: array<Goal> }

    override this.ToString() =
        let t =
            String.concat "\n" <| Array.mapi (fun i el -> $"{i} -> {el}") this.operations

        let g = String.concat "," <| Array.map (fun x -> x.ToString()) this.goals

        $"goals: {g}\noperations:\n{t}"


    member this.ToProgram(ctx: option<string>, numFrames: int) : Program =
        // Identifiers for the operations introduced by unrolling the temporal
        // operators. They start past the ids of the DAG, so that they can never
        // collide with the identifiers of the operations of the work plan.
        let mutable nextId = this.operations.Length

        let freshId () =
            let id = nextId
            nextId <- nextId + 1
            id

        // The frame reference, when there is one, travels as an actual argument and
        // is declared as a formal argument; it is never part of the name of an
        // operation.
        let ctxList =
            match ctx with
            | None -> []
            | Some c -> [ ECall("unknown", c, []) ]

        let ctxArgs =
            match ctx with
            | None -> []
            | Some c -> [ c ]

        let atFrame id = ECall("unknown", $"op{id}", ctxList)

        let atNextFrame id =
            ECall("unknown", $"op{id}", [ ECall("unknown", "inc", ctxList) ])

        // Maps the ids of the DAG to the ids of the emitted declarations. The two
        // differ for the operators that expand to more than one declaration; since
        // the arguments of an operation always have a smaller id than the operation
        // itself, env[arg] is always up to date when the operation is translated.
        let env = Array.init this.operations.Length id

        // A frame index that is a bare identifier can only be the frame reference
        // itself: any other name is a mistake that would otherwise travel through
        // the next two passes and surface at the end of the pipeline, far from the
        // specification that caused it.
        let checkFrameIndex index =
            let operand = this.operations[index]

            match ctx, operand.operator with
            | Some c, Identifier other when other <> c && Seq.isEmpty operand.arguments ->
                ErrorMsg.fail
                    $"'{other}' is used as a frame index, but the frame reference of this specification is '{c}'"
            | _ -> ()

        let sem opId (op: Operation) : seq<Command> * Expression * int =
            match op.operator with
            | Identifier "frame" ->
                match op.arguments with
                | [ _; index ] as args ->
                    checkFrameIndex index
                    Seq.empty, ECall("unknown", "frame", List.map (fun arg -> atFrame env[arg]) args), opId
                | _ -> failwith "frame must take two arguments"
            | Identifier "diamond" ->
                match op.arguments with
                | [ a ] -> Seq.empty, atNextFrame env[a], opId
                | _ -> failwith "Diamond must take one argument"
            | Identifier "until" ->
                match op.arguments with
                | [ a; b ] ->
                    // Bounded unrolling of phi U psi, with numFrames as the horizon:
                    //   U_numFrames = psi
                    //   U_i         = psi | (phi & X U_(i+1))
                    let declarations = ResizeArray<Command>()
                    let phi () = atFrame env[a]
                    let psi () = atFrame env[b]

                    let mutable inner = env[b] // id of U_(i+1)
                    let mutable result = psi () // U_0; just psi when there is nothing to unroll

                    for i = 1 to numFrames do
                        let idAnd = freshId ()

                        declarations.Add(
                            Declaration($"op{idAnd}", ctxArgs, ECall("unknown", "and", [ phi (); atNextFrame inner ]))
                        )

                        let orExpr = ECall("unknown", "or", [ psi (); atFrame idAnd ])

                        // The outermost disjunction is the operation itself, and is
                        // declared by the caller; the others need a declaration here.
                        if i < numFrames then
                            let idOr = freshId ()
                            declarations.Add(Declaration($"op{idOr}", ctxArgs, orExpr))
                            inner <- idOr
                        else
                            result <- orExpr

                    declarations :> seq<Command>, result, freshId ()
                | _ -> failwith "Until must take two arguments"
            | Identifier x ->
                Seq.empty, ECall("unknown", x, List.map (fun arg -> atFrame env[arg]) op.arguments), opId
            | Number x -> Seq.empty, ENumber x, opId
            | Bool x -> Seq.empty, EBool x, opId
            | String x -> Seq.empty, EString x, opId

        let declarations = ResizeArray<Command>()

        for i = 0 to this.operations.Length - 1 do
            let expansion, expr, outId = sem i this.operations[i]
            declarations.AddRange expansion
            env[i] <- outId
            declarations.Add(Declaration($"op{outId}", ctxArgs, expr))

        let goals =
            [ let param =
                match ctx with
                | None -> []
                | Some _ -> [ ENumber 0.0 ]

              for goal in this.goals do
                  match goal with
                  | GoalSave(x, y) -> yield Save("unknown", x, ECall("unknown", $"op{env[y]}", param))
                  | GoalPrint(x, y) -> yield Print("unknown", x, ECall("unknown", $"op{env[y]}", param)) ]

        Program [ yield! declarations; yield! goals ]

    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.operations.Length - 1 do
            let operation = this.operations[i]

            str <-
                str
                + $"{i} [label=\"[{i}] {operation.ToString()}\"];\n" // Uncomment to add [n] to each label
                //+ $"{i} [label=\"{operation.ToString()}\"];\n"

            for argument in operation.arguments do
                str <- str + $"{argument} -> {i};\n"

        str + "\n}"

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
                this.byTerm[(operator, arguments)] <- newOperation

            this.byId[newId] <- newOperation
            newId


        member this.Alias operator arguments (operationId: OperationId) =
            let operation = this.byId[operationId]
            this.byTerm[(operator, arguments)] <- operation


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
        | Save(pos, filename, expr) -> // TODO: use pos
            reduceExpr [ ($"save {filename}", pos) ] (env, operations) expr
            <| fun operationId ->
                ignore <| goals.Add(GoalSave(filename, operationId))

                cont (env, [])
        | Declaration(ide, formalArgs, body) -> cont (env.Bind ide (Fun(env, formalArgs, body)), [])
        | Print(pos, str, expr) -> // TODO: use pos
            reduceExpr [ ($"print {str}", pos) ] (env, operations) expr
            <| fun operationId ->
                ignore <| goals.Add(GoalPrint(str, operationId))
                cont (env, [])
        | Import filename ->
            let libdir =
                $"{System.IO.Path.GetDirectoryName(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName)}/imgql"

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
                ErrorMsg.Logger.Debug <| sprintf "Importing file \"%s\"" path

                let parsed = parseImport path
                cont (env, parsed)
            else
                cont (env, [])

    and reduceExpr (stack: ErrorMsg.Stack) (env: Environment, operations: Operations) (expr: Expression) (cont) =
        match expr with
        | ENumber f -> cont <| operations.FindOrCreate (Number f) []
        | EBool b -> cont <| operations.FindOrCreate (Bool b) []
        | EString s -> cont <| operations.FindOrCreate (String s) []
        | ECall(pos, ide, args) -> // TODO: use pos
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
                    | Some(Fun(denv, formalArgs, body)) ->
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
                    | Some(Operation t) -> cont t
                    | None -> cont <| operations.Create (Identifier ide) actualArgs

let reduceProgram (Parser.Program prog) =
    let goals = new HashSet<_>()

    let pRef = ref prog

    let operations =
        Internals.reduceProgramRec
            (Internals.emptyEnvironment, Internals.emptyOperations (), goals, new HashSet<_>())
            pRef

    { operations = Array.init operations.byId.Count (fun i -> operations.byId[i].operation)
      goals = Array.ofSeq goals }
