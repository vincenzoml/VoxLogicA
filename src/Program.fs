module VoxLogicA.Main

open System.Reflection
open Argu

type LoadFlags = { fname: string; numCores: int }
// type JSonOutput = FSharp.Data.JsonProvider<"example.json">
type CmdLine =
    | [<UniqueAttribute>] Version
    | [<UniqueAttribute>] SaveTaskGraphAsDot of string
    | [<UniqueAttribute>] SaveTaskGraph of option<string>
    | [<UniqueAttribute>] SaveSyntax of option<string>
    | [<UniqueAttribute>] SaveLabelling of option<string>
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "print the voxlogica version and exit"
            | SaveTaskGraph _ -> "save the task graph"
            | SaveTaskGraphAsDot _ -> "save the task graph in .dot format and exit"
            | SaveSyntax _ -> "save the AST in text format and exit"
            | SaveLabelling _ -> "save the labelling in text format and exit"       
            | Filename _ -> "VoxLogicA session file"

[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
<<<<<<< HEAD
    let version = name.Version 
    let informationalVersion = ((Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0]) :?> AssemblyInformationalVersionAttribute).InformationalVersion
    ErrorMsg.Logger.Debug (sprintf "%s %s" name.Name informationalVersion)
    let model = PosetModel() :> IModel   
    let checker = ModelChecker(model)          
    if version.Revision <> 0 then ErrorMsg.Logger.Warning (sprintf "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d." name.Name version.Major version.Minor version.Build)                        
=======
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

>>>>>>> 17c31905644b20952e5086389a070bcf0ce1e558
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
        
<<<<<<< HEAD
        if parsed.Contains Ops
        then 
            Seq.iter (fun (op : Operator) -> printfn "%s" <| op.Show()) checker.OperatorFactory.Operators
            exit 0
        let sequential = parsed.Contains Sequential        
        // if sequential
        // then 
        //     let proc = System.Diagnostics.Process.GetCurrentProcess()
        //     proc.ProcessorAffinity <- nativeint 0x1          
        let run filename =
            let interpreter = Interpreter(model,checker)
            interpreter.Batch sequential interpreter.DefaultLibDir filename
            model.OnExit()
        match (parsed.TryGetResult Filename,ErrorMsg.isDebug()) with 
            | None,false ->                                      
                printfn "%s\n" (cmdLineParser.PrintUsage ())
                0
            | Some filename,_ -> 
                run filename 
                0
            | None,true ->
                run "test.imgql"
                0
    with e ->        
            ErrorMsg.Logger.DebugExn e
            ErrorMsg.Logger.Failure "exiting."
            1
=======
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
>>>>>>> 17c31905644b20952e5086389a070bcf0ce1e558
