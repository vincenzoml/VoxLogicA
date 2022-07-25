module VoxLogicA.Main

open System.Reflection
open Argu

type LoadFlags = { fname: string; numCores: int }
// type JSonOutput = FSharp.Data.JsonProvider<"example.json">
type CmdLine =
    | [<UniqueAttribute>] Ops
    | [<UniqueAttribute>] JSon
    | [<UniqueAttribute>] Version
    | [<UniqueAttribute>] Sequential
    | [<UniqueAttribute>] PerformanceTest
    | [<UniqueAttribute>] SaveTaskGraphAsDot of string
    | [<UniqueAttribute>] SaveTaskGraph of option<string>
    | [<UniqueAttribute>] SaveSyntax of option<string>
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops -> "display a list of all the internal operators, with their types and a brief description"
            | JSon _ ->
                "saves auxiliary information on saved layers, printed values, and the log file to a structured json format instead than on standard output"
            | Sequential -> "wait for each thread to complete before starting a new one; useful for debugging"
            | Version -> "print the voxlogica version and exit"
            | PerformanceTest _ ->
                "do not load or save actual images; always use the given file instead; useful to measure raw speed"
            | SaveTaskGraph _ -> "save the task graph"
            | SaveTaskGraphAsDot _ -> "save the task graph in .dot format and exit"
            | SaveSyntax _ -> "save the AST in text format and exit"
            | Filename _ -> "VoxLogicA session file"

open VoxLogicA.Interpreter
open VoxLogicA.TestEngines

[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version

    let informationalVersion =
        ((Assembly
            .GetEntryAssembly()
            .GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0])
        :?> AssemblyInformationalVersionAttribute)
            .InformationalVersion

    let cmdLineParser =
        ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

    let parsed = cmdLineParser.Parse argv

    if Option.isSome (parsed.TryGetResult Version) then
        printfn "%s" informationalVersion
        exit 0
    // let finish =
    //     if Option.isSome (parsed.TryGetResult JSon) then
    //         let readLog = ErrorMsg.Logger.LogToMemory()
    //         fun oExn ->
    //             let (print,save) = ErrorMsg.Report.Get()
    //             let log = readLog()
    //             let json =
    //                 JSonOutput.Root(
    //                     print =
    //                         List.toArray (
    //                             List.map
    //                                 (fun (name, typ, res) -> JSonOutput.Print(name = name, vltype = typ, value = res))
    //                                 print
    //                         ),
    //                     layers =
    //                         List.toArray (
    //                             List.map
    //                                 (fun (name, vltype, min, max, path: string) ->
    //                                     let mutable ext = System.IO.Path.GetExtension path

    //                                     let mutable fname =
    //                                         System.IO.Path.GetFileNameWithoutExtension path

    //                                     if ext = ".gz" then
    //                                         let ext2 = System.IO.Path.GetExtension fname
    //                                         ext <- ext2 + ext
    //                                         fname <- System.IO.Path.GetFileNameWithoutExtension fname

    //                                     JSonOutput.Layer(
    //                                         vltype = vltype,
    //                                         min = min,
    //                                         max = max,
    //                                         name = fname,
    //                                         extension = ext
    //                                     ))
    //                                 save
    //                         ), //
    //                     error =
    //                         FSharp.Data.JsonValue.String(
    //                             match oExn with
    //                             | None -> ""
    //                             | Some exn -> exn.ToString()
    //                         ),
    //                     log = FSharp.Data.JsonValue.String log
    //                 )
    //             printf "%s" <| json.ToString()
    //     else
    //         ErrorMsg.Logger.LogToStdout ()
    //         ignore

    ErrorMsg.Logger.LogToStdout()

    if version.Revision <> 0 then
        ErrorMsg.Logger.Warning(
            sprintf
                "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d."
                name.Name
                version.Major
                version.Minor
                version.Build
        )

    try

        let filename =
            if parsed.Contains Filename then
                parsed.GetResult Filename
            else
                "src/test.imgql"
        // try (parsed.GetResult Filename)
        // with _ ->
        //     printfn "%s version: %s" name.Name informationalVersion
        //     printfn "%s\n" (cmdLineParser.PrintUsage())
        //     exit 0

        ErrorMsg.Logger.Debug $"{name.Name} version: {informationalVersion}"

        // let performance = parsed.Contains PerformanceTest

        let syntax = Parser.parseProgram filename
        ErrorMsg.Logger.Debug "Program parsed"

        if parsed.Contains SaveSyntax then
            let filenameOpt = parsed.GetResult SaveSyntax

            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the abstract syntax to {filename}"
                System.IO.File.WriteAllText(filename, $"{syntax}")
            | None -> ErrorMsg.Logger.Debug $"{syntax}"

        let program = Reducer.reduceProgram syntax

        ErrorMsg.Logger.Debug "Program reduced"
        ErrorMsg.Logger.Debug $"Number of tasks: {program.operations.Length}"

        if parsed.Contains SaveTaskGraph then
            let filenameOpt = parsed.GetResult SaveTaskGraph

            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the task graph to {filename}"
                System.IO.File.WriteAllText(filename, $"{program}")
            | None -> ErrorMsg.Logger.Debug $"{program}"

        if parsed.Contains SaveTaskGraphAsDot then
            let filename = parsed.GetResult SaveTaskGraphAsDot
            ErrorMsg.Logger.Debug $"Saving the task graph to {filename}"
            System.IO.File.WriteAllText(filename, program.ToDot())

        let engine = Arithmetics()
        let resourceManager = ArithmeticsResourceManager()

        let interpreter = Interpreter(engine, resourceManager)

        ErrorMsg.Logger.Debug "Preparing interpreter"
        interpreter.Prepare(program)
        ErrorMsg.Logger.Debug "Running interpreter"

        let tasks =
            Seq.map
                (fun goal ->
                    match goal with
                    | (Reducer.GoalSave (label, id)
                    | Reducer.GoalPrint (label, id)) ->
                        task {
                            let! result = interpreter.Query id
                            ErrorMsg.Logger.Result label result
                        }
                        :> System.Threading.Tasks.Task)
                program.goals
            |> Seq.toArray

        System.Threading.Tasks.Task.WaitAll tasks
        ErrorMsg.Logger.Debug "All done."

        //ErrorMsg.Logger.Debug $"{x}"

        // let model = GPUModel(performance) :> IModel // SITKModel() :> IModel
        // let checker = ModelChecker model

        // if parsed.Contains Ops then
        //     Seq.iter (fun (op: Operator) -> printfn "%s" <| op.Show()) checker.OperatorFactory.Operators
        //     exit 0

        // let run filename =
        //     let interpreter = Interpreter(model, checker)
        //     interpreter.Batch interpreter.DefaultLibDir filename

        // match (parsed.TryGetResult Filename, Util.isDebug ()) with
        // | Some filename, _ ->
        //     run filename
        //     finish None
        //     0
        // | None, _ ->
        //     run "test.imgql"

        0
    with
    | e ->
        ErrorMsg.Logger.DebugExn e
        raise e
        // finish (Some e)
        1
