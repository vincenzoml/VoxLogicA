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
    | [<MainCommandAttribute; UniqueAttribute>] Filename of string
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops -> "display a list of all the internal operators, with their types and a brief description"
            | JSon _ ->
                "saves auxiliary information on saved layers, printed values, and the log file to a structured json format instead than on standard output"
            | Filename _ -> "VoxLogicA session file"

#if FALSE
[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version

    let informationalVersion =
        ((Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0]) :?> AssemblyInformationalVersionAttribute).InformationalVersion
    let cmdLineParser =
        ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

    let parsed = cmdLineParser.Parse argv
    ErrorMsg.Logger.Debug(sprintf "%s %s" name.Name informationalVersion)
    let model = SITKModel() :> IModel
    let checker = ModelChecker model
    let finish = 
        if Option.isSome (parsed.TryGetResult JSon) then 
            let readLog = ErrorMsg.Logger.LogToMemory()
            fun oExn ->
                let (print,save) = ErrorMsg.Report.Get()
                let log = readLog()
                let json = JSonOutput.Root(                                
                            print = List.toArray (List.map (fun (name,typ,res) -> JSonOutput.Print(name = name, typ = typ, value = res)) print),
                            layers = List.toArray (List.map (fun (name,typ,info,path) -> JSonOutput.Layer(name = name, typ = typ, info = info, path = path)) save), // 
                            error = FSharp.Data.JsonValue.String (match oExn with None -> "" | Some exn -> exn.ToString()),  
                            log = FSharp.Data.JsonValue.String log)
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
    with e ->
        ErrorMsg.Logger.DebugExn e
        ErrorMsg.Logger.Failure "exiting."
        finish (Some e)
        1
#else

open VoxLogicA

[<EntryPoint>]
let main (argv: string array) =
    ErrorMsg.Logger.LogToStdout()
    ErrorMsg.Logger.Debug "Starting"
    
    let kernelFile = System.IO.Path.Combine [|System.IO.Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location); "kernel.cl"|]
    
    let gpu = GPU.GPU(kernelFile)    
    
    let img = new SITKUtil.VoxImage "./three_coloured_items_RGBA.png"        
    
    let input = gpu.CopyImageToDevice img
    let output = gpu.NewImageOnDevice img 
    let output2 = gpu.NewImageOnDevice img 
    
    printfn "input: %A output: %A" input output
    
    let e1 = gpu.Run("swapRG",[||],input,output,img.Size,None)
    let e2 = gpu.Run("swapRG",[|e1|],output,output2,img.Size,None)   
    
    ignore <| gpu.Run("swapRG",[|e2|],output2,input,img.Size,None)    
    gpu.Finish() 

    let img1 = input.Get()
    let img2 = output.Get()
    let img3 = output2.Get()
    img1.Save("output1.png")  
    img2.Save("output2.png")  
    img3.Save("output3.png")  
    
    0
#endif