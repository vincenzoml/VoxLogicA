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

exception CommandLineException 
    with override __.Message = "Invalid arguments. Usage:\nVoxLogicA <FILENAME>"

type LoadFlags = {
    fname : string;
    numCores : int
}

type CmdLine = 
    | Ops 
    | Sequential
    | [<MainCommandAttribute;UniqueAttribute>] Filename of string    
with
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops ->  "display a list of all the internal operators, with their types and a brief description"
            | Filename _ -> "VoxLogicA session file"
            | Sequential ->  "Run on one CPU only"
    
[<EntryPoint>]
let main (argv : string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version 
    let informationalVersion = ((Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0]) :?> AssemblyInformationalVersionAttribute).InformationalVersion
    ErrorMsg.Logger.Debug (sprintf "%s %s" name.Name informationalVersion)
    let model = SITKModel() :> IModel   
    let checker = ModelChecker(model)          
    if version.Revision <> 0 then ErrorMsg.Logger.Warning (sprintf "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d." name.Name version.Major version.Minor version.Build)                        
    try
        let cmdLineParser = ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())     
        let parsed = cmdLineParser.Parse argv  
        
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
