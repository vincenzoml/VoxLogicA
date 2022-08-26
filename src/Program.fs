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
open VoxLogicA.CPUEngine
open itk.simple

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

    if Option.isSome (parsed.TryGetResult CmdLine.Version) then
       printfn "%s" informationalVersion
       exit 0

    ErrorMsg.Logger.LogToStdout()
#if DEBUG
    //ErrorMsg.Logger.SetLogLevel(["user"; "info"; "debug"])
#else
    ErrorMsg.Logger.SetLogLevel([ "user"; "info" ])    
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

        let filename =
            if parsed.Contains Filename then
                parsed.GetResult Filename
            else
#if DEBUG
                "src/test.imgql"
                //"test.imgql"
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

        let program = Reducer.reduceProgram syntax

        ErrorMsg.Logger.Debug "Program reduced"
        ErrorMsg.Logger.Info $"Number of tasks: {program.operations.Length}"

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

        // let fibEngine = Fib()
        let cpuEngine = CPUEngine()

        // let resourceManager = Resources.ResourceManager<_, _>(FibResource.Allocator)
        let CPUResourceManager = Resources.ResourceManager<_,_>(CPUResource.Allocator)

        // let interpreter = Interpreter(engine, resourceManager)
        let CPUInterpreter = Interpreter(cpuEngine, CPUResourceManager)

        ErrorMsg.Logger.Debug "Preparing interpreter"
        CPUInterpreter.Prepare(program)
        ErrorMsg.Logger.Debug "Running interpreter"

        let mutable n = program.goals.Length
        let cts = new System.Threading.CancellationTokenSource()

        ignore <| System.Threading.Tasks.Task.Run(
            fun () -> ignore <| CPUInterpreter.Session()) // TODO not clear; there should be no Task.Run here

        //let tasks =
        //    [| for goal in program.goals do
        //           match goal with // DUE MATCH IN UNO
        //            | (Reducer.GoalSave (label, id)) ->
        //                (task {
        //                    System.Threading.SynchronizationContext.SetSynchronizationContext
        //                        interpreter.SynchronizationContext
//
        //                    let! result = interpreter.QueryAsync id
        //                    ErrorMsg.Logger.Result label result.Value // SOSTITUISCO CON SALVATAGGIO, DOVE LABEL È IL NOME DEL FILE
        //                    result.Reclaim(interpreter)
        //                    n <- n - 1
        //                    if n = 0 then 
        //                        ErrorMsg.Logger.Test "cts" "Requesting cancellation"
        //                        cts.Cancel()
        //                        ErrorMsg.Logger.Test "test" "exiting, remove the exit and make cancellation work"
        //                        exit 0
        //               }) :> System.Threading.Tasks.Task 
        //            | (Reducer.GoalPrint (label, id)) -> 
        //                (task {
        //                    System.Threading.SynchronizationContext.SetSynchronizationContext
        //                        interpreter.SynchronizationContext
    //
        //                    let! result = interpreter.QueryAsync id
        //                    ErrorMsg.Logger.Result label result.Value 
        //                    result.Reclaim(interpreter)
        //                    n <- n - 1
        //                    if n = 0 then 
        //                        ErrorMsg.Logger.Test "cts" "Requesting cancellation"
        //                        cts.Cancel()
        //                        ErrorMsg.Logger.Test "test" "exiting, remove the exit and make cancellation work"
        //                        exit 0
        //                })
        //                :> System.Threading.Tasks.Task |]

        let CPUtasks = 
            [| for goal in program.goals do
                match goal with
                | (Reducer.GoalSave(label, id)) ->
                    (task {
                        System.Threading.SynchronizationContext.SetSynchronizationContext
                            CPUInterpreter.SynchronizationContext
                        let! result = CPUInterpreter.QueryAsync id
                        let img = 
                            match result.Value with
                            | CPUImg img' -> img'
                        SimpleITK.WriteImage(img,label) // SOSTITUISCO CON SALVATAGGIO, DOVE LABEL È IL NOME DEL FILE
                        ErrorMsg.Logger.Test "test" "Image saved"
                        result.Reclaim(CPUInterpreter)
                        n <- n - 1
                        if n = 0 then 
                            ErrorMsg.Logger.Test "cts" "Requesting cancellation"
                            cts.Cancel()
                            ErrorMsg.Logger.Test "test" "exiting, remove the exit and make cancellation work"
                            exit 0
                    }) :> System.Threading.Tasks.Task
                | (Reducer.GoalPrint(label, id )) ->
                    (task {
                        System.Threading.SynchronizationContext.SetSynchronizationContext
                            CPUInterpreter.SynchronizationContext
                        let! result = CPUInterpreter.QueryAsync id
                        ErrorMsg.Logger.Result label (result.Value.ToString())                            
                        n <- n - 1
                        if n = 0 then 
                            ErrorMsg.Logger.Test "cts" "Requesting cancellation"
                            cts.Cancel()
                            ErrorMsg.Logger.Test "test" "exiting, remove the exit and make cancellation work"
                            exit 0
                    }) :> System.Threading.Tasks.Task
            |]

        ErrorMsg.Logger.Info "tasks launched, waiting..."
        
        CPUInterpreter.Run(cts.Token)

        ErrorMsg.Logger.Info "All done."

        0
    with
    | e ->
        ErrorMsg.Logger.DebugExn e
        raise e
        1
