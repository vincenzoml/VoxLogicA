// Copyright 2018 Vincenzo Ciancia.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// A copy of the license is available in the file "Apache_License.txt".
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module VoxLogicA.Main

open System.Reflection
open Argu

type LoadFlags = { fname: string; numCores: int }

type CmdLine =
    | [<UniqueAttribute>] Ops
    | [<UniqueAttribute>] JSon
    | [<UniqueAttribute>] Sequential
    | [<UniqueAttribute>] PerformanceTest
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops -> "display a list of all the internal operators, with their types and a brief description"
            | JSon _ ->
                "saves auxiliary information on saved layers, printed values, and the log file to a structured json format instead than on standard output"
            | Sequential ->
                "wait for each thread to complete before starting a new one; useful for debugging"
            | PerformanceTest _ ->
                "do not load or save actual images; always use the given file instead; useful to measure raw speed" 

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
    ErrorMsg.Logger.Debug(sprintf "%s %s" name.Name informationalVersion)
    let performance = parsed.Contains PerformanceTest        
    let model = SITKModel(performance) :> IModel
    let checker = ModelChecker model

    let finish =
        if Option.isSome (parsed.TryGetResult JSon) then
            let readLog = ErrorMsg.Logger.LogToMemory()

            fun oExn ->
                let (print, save) = ErrorMsg.Report.Get()
                let log = readLog ()

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
            ErrorMsg.Logger.LogToStdout()
            ignore

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
        if parsed.Contains Ops then
            Seq.iter (fun (op: Operator) -> printfn "%s" <| op.Show()) checker.OperatorFactory.Operators
            exit 0
        // if sequential
        // then
        //     let proc = System.Diagnostics.Process.GetCurrentProcess()
        //     proc.ProcessorAffinity <- nativeint 0x1


        let run filename =
            let interpreter = Interpreter(model, checker)
            interpreter.Batch interpreter.DefaultLibDir filename

        match (parsed.TryGetResult Filename, ErrorMsg.isDebug ()) with
        | None, false ->
            printfn "%s\n" (cmdLineParser.PrintUsage())
            0
        | Some filename, _ ->
            run filename
            finish None
            0
        | None, true ->
            run "test.imgql"
            0
    with
    | e ->
        ErrorMsg.Logger.DebugExn e
        ErrorMsg.Logger.Failure "exiting."
        finish (Some e)
        1
