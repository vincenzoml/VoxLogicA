module VoxLogicA.Main

open System.Reflection
open Argu
open VoxLogicA.Parser

type LoadFlags = { fname: string; numCores: int }
// type JSonOutput = FSharp.Data.JsonProvider<"example.json">
type CmdLine =
    | [<UniqueAttribute>] Version
    | [<UniqueAttribute>] FlattenSpatioTemporal of string
    | [<UniqueAttribute>] SaveTaskGraphAsDot of string
    | [<UniqueAttribute>] SaveTaskGraph of option<string>
    | [<UniqueAttribute>] SaveTaskGraphAsAST of option<string>
    | [<UniqueAttribute>] SaveTaskGraphAsProgram of option<string>
    | [<UniqueAttribute>] NumFrames of int
    | [<UniqueAttribute>] ProvideContext of option<string>
    | [<UniqueAttribute>] SaveSyntax of option<string>
    | [<UniqueAttribute>] SaveLabelling of option<string>
    | [<UniqueAttribute>] EvaluateSpatioTemporal of option<string>
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "print the voxlogica version and exit"
            | FlattenSpatioTemporal _ -> "save spatial specification from spatio temporal"
            | SaveTaskGraph _ -> "save the task graph"
            | SaveTaskGraphAsDot _ -> "save the task graph in .dot format and exit"
            | SaveTaskGraphAsAST _ -> "save the task graph in AST format and exit"
            | SaveTaskGraphAsProgram _ -> "save the task graph in VoxLogicA format and exit"
            | NumFrames _ -> "number of frames to process"
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
            .GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0])
        :?> AssemblyInformationalVersionAttribute)
            .InformationalVersion

    let cmdLineParser =
        ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

    let parsed = cmdLineParser.Parse argv

    if Option.isSome (parsed.TryGetResult Version) then
        printfn "%s" informationalVersion
        exit 0

    ErrorMsg.Logger.LogToStdout()
#if ! DEBUG
    ErrorMsg.Logger.SetLogLevel([ "user"; "info" ])
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

        let syntax = Parser.parseProgram filename
        ErrorMsg.Logger.Debug "Program parsed"

        if parsed.Contains SaveSyntax then
            let filenameOpt = parsed.GetResult SaveSyntax

            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the abstract syntax to {filename}"
                System.IO.File.WriteAllText(filename, $"{syntax}")
            | None -> ErrorMsg.Logger.Debug $"{syntax}"

        let program: Reducer.WorkPlan = Reducer.reduceProgram syntax

        ErrorMsg.Logger.Debug "Program reduced"
        ErrorMsg.Logger.Info $"Number of tasks: {program.operations.Length}"

        //if parsed.Contains FlattenSpatioTemporal then
        //    let filenameOpt = parsed.GetResult FlattenSpatioTemporal

        //    let commands =
        //        match syntax with
        //        | Program p -> p

        //    match filenameOpt with
        //    | filename ->
        //        ErrorMsg.Logger.Debug $"Saving spatio-temporal flattening to {filename}"
        //        let spatioTemporalProgram, venv, _, _ = SpatioTemporal.flattenSpatioTemporal commands (Env []) (Env []) (Env [">",EFun(ECall("", "x", []),Env[])]) 
        //        System.IO.File.Delete(filename)
        //        for command in spatioTemporalProgram do
        //            System.IO.File.AppendAllText(filename, $"{command}")
        //        ErrorMsg.Logger.Debug $"{venv}"


        if parsed.Contains SaveTaskGraphAsAST then
            let filenameOpt = parsed.GetResult SaveTaskGraphAsAST
            let numFrames = argv[2]

            let voxlogicaProgram = program.ToProgram(None, int numFrames)
            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the task graph in AST syntax to {filename}"
                System.IO.File.WriteAllText(filename, $"{voxlogicaProgram}")
            | None -> ErrorMsg.Logger.Debug $"{voxlogicaProgram}"

        if parsed.Contains SaveTaskGraphAsProgram then
            let filenameOpt = parsed.GetResult SaveTaskGraphAsProgram
            let contextOpt = if parsed.Contains ProvideContext then parsed.GetResult ProvideContext else None
            let numFrames = parsed.GetResult NumFrames

            let voxlogicaProgram = program.ToProgram(contextOpt, int numFrames)
            let voxlogicaSyntax = voxlogicaProgram.ToSyntax()
            match filenameOpt with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the task graph in VoxLogicA syntax to {filename}"
                System.IO.File.WriteAllText(filename, $"{voxlogicaSyntax}")
            | None -> ErrorMsg.Logger.Debug $"{voxlogicaSyntax}"

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

        if parsed.Contains EvaluateSpatioTemporal then
            let filenameOpt = parsed.GetResult EvaluateSpatioTemporal
            let numFrames = parsed.GetResult NumFrames
            let partEval = PartialEvaluation.evaluateProgram program (int numFrames)
            let evaluatedProgram = partEval.program

            match filenameOpt with
            | Some filename ->
                if System.IO.File.Exists(filename) then
                    System.IO.File.Delete(filename)
                ErrorMsg.Logger.Debug $"Saving the partial evaluation to {filename}"
                for str in evaluatedProgram do
                    System.IO.File.AppendAllText(filename, str + "\n")
            | None -> ErrorMsg.Logger.Debug $"{(evaluatedProgram).ToString()}"
        
        if parsed.Contains SaveLabelling then            
            let labelling = Labelling.label(program)
            let labellingString = 
                labelling
                |> Array.mapi (fun i x -> $"{i}: {x}")
                |> String.concat "\n"                
            let x = parsed.GetResult SaveLabelling
            match x with
            | Some filename ->
                ErrorMsg.Logger.Debug $"Saving the labelling to {filename}"            
                System.IO.File.WriteAllText(filename, labellingString)
            | None -> ErrorMsg.Logger.Debug labellingString

        ErrorMsg.Logger.Info "All done."
        0
    with
    | e ->
        ErrorMsg.Logger.DebugExn e
        raise e
        1
