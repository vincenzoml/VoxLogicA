module VoxLogicA.Main

open System.Reflection
open Argu
open VoxLogicA.Parser

type LoadFlags = { fname: string; numCores: int }
// type JSonOutput = FSharp.Data.JsonProvider<"example.json">
type CmdLine =
    | [<UniqueAttribute>] Version
    | [<UniqueAttribute>] SaveTaskGraphAsDot of string
    | [<UniqueAttribute>] SaveTaskGraph of option<string>
    | [<UniqueAttribute>] SaveTaskGraphAsAST of option<string>
    | [<UniqueAttribute>] SaveTaskGraphAsProgram of option<string>
    | [<UniqueAttribute>] NumFrames of int
    | [<UniqueAttribute>] MaxLabels of int
    | [<UniqueAttribute>] ProvideContext of option<string>
    | [<UniqueAttribute>] SaveSyntax of option<string>
    | [<UniqueAttribute>] SaveLabelling of option<string>
    | [<UniqueAttribute>] EvaluateSpatioTemporal of option<string>
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string

    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "print the voxlogica version and exit"
            | SaveTaskGraph _ -> "save the task graph"
            | SaveTaskGraphAsDot _ -> "save the task graph in .dot format and exit"
            | SaveTaskGraphAsAST _ -> "save the task graph in AST format and exit"
            | SaveTaskGraphAsProgram _ -> "save the task graph in VoxLogicA format and exit"
            | NumFrames _ -> "number of frames to process"
            | MaxLabels _ -> "bound on the labels an exists ranges over"
            | ProvideContext _ -> "provide the context"
            | SaveSyntax _ -> "save the AST in text format and exit"
            | SaveLabelling _ -> "save the labelling in text format and exit"
            | EvaluateSpatioTemporal _ -> "evaluate a flattened spatio temporal specification"
            | Filename _ -> "VoxLogicA session file"

[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version

    let informationalVersion =
        ((Assembly
            .GetEntryAssembly()
            .GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false)
            .[0])
        :?> AssemblyInformationalVersionAttribute)
            .InformationalVersion

    let cmdLineParser =
        ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

    let parsed = cmdLineParser.Parse argv

    if Option.isSome (parsed.TryGetResult Version) then
        printfn "%s" informationalVersion
        exit 0

    ErrorMsg.Logger.LogToStderr()
#if ! DEBUG
    ErrorMsg.Logger.SetLogLevel([ "user"; "info"; "warn"; "fail" ])
#else
    ()
#endif

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

        let filename: string =
            if parsed.Contains Filename then
                parsed.GetResult Filename
            else
#if DEBUG
                "test.imgql"
#else
                printfn "%s version: %s" name.Name informationalVersion
                printfn "%s\n" (cmdLineParser.PrintUsage())
                exit 0
#endif

        ErrorMsg.Logger.Info $"{name.Name} version: {informationalVersion}"

        // Each of these commands writes its result to the file it is given, and to
        // standard output when it is given none. The second case used to go to
        // Debug, which a release build compiles away: the command printed nothing
        // and exited successfully.
        let emit what (filenameOpt: option<string>) (text: string) =
            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving {what} to {filename}"
                System.IO.File.WriteAllText(filename, text)
            | None -> printfn "%s" (text.TrimEnd '\n')

        let syntax = Parser.parseProgram filename
        ErrorMsg.Logger.Debug "Program parsed"

        if parsed.Contains SaveSyntax then
            emit "the abstract syntax" (parsed.GetResult SaveSyntax) $"{syntax}"

        let program: Reducer.WorkPlan = Reducer.reduceProgram syntax

        ErrorMsg.Logger.Debug "Program reduced"
        ErrorMsg.Logger.Info $"Number of tasks: {program.operations.Length}"

        // The two dumps of the task graph are the same program in two formats, so
        // they read the frame reference the same way. Reading the number of frames
        // is left to the commands that need it: the other ones must keep working
        // without --numframes.
        let contextOpt =
            if parsed.Contains ProvideContext then
                parsed.GetResult ProvideContext
            else
                None

        // A function, not a value: the commands that unroll nothing have to keep
        // working without --numframes.
        let numFrames () =
            let n = parsed.GetResult NumFrames

            if n < 1 then
                ErrorMsg.fail $"--numframes is {n}: a specification cannot be unrolled over fewer than one frame"

            n

        // Also a function, and read only when the specification quantifies over
        // labels: the bound is not a fact about the data, as the number of frames
        // is, but a hypothesis of the run, and the specifications that make no
        // such hypothesis must not be asked for it.
        let maxLabels () =
            match parsed.TryGetResult MaxLabels with
            | None ->
                ErrorMsg.fail "the specification quantifies over labels: the bound on them has to be given with --maxlabels"
            | Some k when k < 1 -> ErrorMsg.fail $"--maxlabels is {k}: an exists cannot range over fewer than one label"
            | Some k -> k

        if parsed.Contains SaveTaskGraphAsAST then
            let voxlogicaProgram = program.ToProgram(contextOpt, numFrames (), maxLabels)

            emit "the task graph in AST syntax" (parsed.GetResult SaveTaskGraphAsAST) $"{voxlogicaProgram}"

        if parsed.Contains SaveTaskGraphAsProgram then
            let voxlogicaProgram = program.ToProgram(contextOpt, numFrames (), maxLabels)

            emit
                "the task graph in VoxLogicA syntax"
                (parsed.GetResult SaveTaskGraphAsProgram)
                (voxlogicaProgram.ToSyntax())

        if parsed.Contains SaveTaskGraph then
            emit "the task graph" (parsed.GetResult SaveTaskGraph) $"{program}"


        if parsed.Contains SaveTaskGraphAsDot then
            let filename = parsed.GetResult SaveTaskGraphAsDot
            ErrorMsg.Logger.Debug $"Saving the task graph to {filename}"
            System.IO.File.WriteAllText(filename, program.ToDot())

        if parsed.Contains EvaluateSpatioTemporal then
            let partEval = PartialEvaluation.evaluateProgram program (numFrames ())

            // The lines used to be appended one at a time to a file deleted
            // beforehand, and printed by way of ToString() on the collection
            // itself, which names its type rather than its contents.
            emit
                "the partial evaluation"
                (parsed.GetResult EvaluateSpatioTemporal)
                (String.concat "\n" partEval.program + "\n")

        ErrorMsg.Logger.Info "All done."
        0
    with e ->
        // The message has already been through the logger; raising again would
        // print it a second time, as an unhandled exception, and abort instead of
        // returning a status. In a debug build DebugExn carries the stack trace.
        ErrorMsg.Logger.DebugExn e
        1
