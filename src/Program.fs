module VoxLogicA.Main

open System.Reflection
open Argu

type LoadFlags = { fname: string; numCores: int }
type JSonOutput = FSharp.Data.JsonProvider<"example.json">
type CmdLine =
    | [<UniqueAttribute>] Ops
    | [<UniqueAttribute>] JSon
    | [<UniqueAttribute>] Version
    | [<UniqueAttribute>] Sequential
    | [<UniqueAttribute>] PerformanceTest
    | [<UniqueAttribute>] SaveTaskGraph of string
    | [<UniqueAttribute>] SaveSyntax of string
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops -> "display a list of all the internal operators, with their types and a brief description"
            | JSon _ ->
                "saves auxiliary information on saved layers, printed values, and the log file to a structured json format instead than on standard output"
            | Sequential ->
                "wait for each thread to complete before starting a new one; useful for debugging"
            | Version -> 
                "print the voxlogica version and exit"
            | PerformanceTest _ ->
                "do not load or save actual images; always use the given file instead; useful to measure raw speed" 
            | SaveTaskGraph _ -> 
                "save the task graph in .dot format and exit"
            | SaveSyntax _ -> 
                "save the AST in text format and exit"
            | Filename _ -> "VoxLogicA session file"

[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version

    let informationalVersion =
        ((Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0]) :?> AssemblyInformationalVersionAttribute).InformationalVersion
    let cmdLineParser =
        ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

    let parsed = cmdLineParser.Parse argv
    if Option.isSome (parsed.TryGetResult Version) then 
        printfn "%s" informationalVersion
        exit 0
    let finish = 
        if Option.isSome (parsed.TryGetResult JSon) then 
            let readLog = ErrorMsg.Logger.LogToMemory()
            fun oExn ->
                let (print,save) = ErrorMsg.Report.Get()
                let log = readLog()
                let json =
                    JSonOutput.Root(
                        print =
                            List.toArray (
                                List.map
                                    (fun (name, typ, res) -> JSonOutput.Print(name = name, vltype = typ, value = res))
                                    print
                            ),
                        layers =
                            List.toArray (
                                List.map
                                    (fun (name, vltype, min, max, path: string) ->
                                        let mutable ext = System.IO.Path.GetExtension path

                                        let mutable fname =
                                            System.IO.Path.GetFileNameWithoutExtension path

                                        if ext = ".gz" then
                                            let ext2 = System.IO.Path.GetExtension fname
                                            ext <- ext2 + ext
                                            fname <- System.IO.Path.GetFileNameWithoutExtension fname

                                        JSonOutput.Layer(
                                            vltype = vltype,
                                            min = min,
                                            max = max,
                                            name = fname,
                                            extension = ext
                                        ))
                                    save
                            ), //
                        error =
                            FSharp.Data.JsonValue.String(
                                match oExn with
                                | None -> ""
                                | Some exn -> exn.ToString()
                            ),
                        log = FSharp.Data.JsonValue.String log
                    )
                printf "%s" <| json.ToString()                 
        else 
            ErrorMsg.Logger.LogToStdout ()
            ignore    
    if version.Revision <> 0 then
        ErrorMsg.Logger.Warning
            (sprintf
                "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d."
                 name.Name
                 version.Major
                 version.Minor
                 version.Build)
    try

        // if sequential
        // then
        //     let proc = System.Diagnostics.Process.GetCurrentProcess()
        //     proc.ProcessorAffinity <- nativeint 0x1                

        let filename =
            try (parsed.GetResult Filename) 
            with _ ->  
                printfn "%s version: %s" name.Name informationalVersion
                printfn "%s\n" (cmdLineParser.PrintUsage())
                exit 0

        ErrorMsg.Logger.Debug $"{name.Name} version: {informationalVersion}"
                
        // let performance = parsed.Contains PerformanceTest        


        let syntax = Parser.parseProgram filename
        if parsed.Contains SaveSyntax then
            System.IO.File.WriteAllText(parsed.GetResult SaveSyntax,$"{syntax}")

        let (Reducer.Tasks x) = Reducer.reduceProgram syntax        

        ErrorMsg.Logger.Debug $"Number of tasks: {x.Length}"
        
        0
    with e ->
        ErrorMsg.Logger.DebugExn e
        finish (Some e)
        1
