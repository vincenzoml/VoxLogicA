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

exception CommandLineException 
    with override __.Message = "Invalid arguments. Usage:\nVoxLogicA <FILENAME>"

type CmdLine = Help | Ops | Filename of string

let parseCmdLine (argv : array<string>) =
    match argv with
    | [|"--help"|] -> Help 
    | [|"--ops"|] -> Ops
    | ([|filename|] | [|"--";filename|]) -> Filename filename
    | _ -> raise CommandLineException 
    
[<EntryPoint>]
let main (argv : string array) = 
    let logger = ErrorMsg.Logger()        
    try 
        let model = SITKModel() :> IModel   
        let checker = ModelChecker(model)                         
        match parseCmdLine argv with
        | Filename filename ->             
            let interpreter = Interpreter(model,checker,logger)
            interpreter.Batch 
                interpreter.DefaultLibDir 
                (System.IO.Path.GetFullPath ".") 
                (System.IO.Path.GetFullPath ".") 
                filename    
        | Help -> 
            printfn "%s" <| // TODO: get the executable name from the environment
                (     "Usage:\n"
                    + "VoxLogicA filename\t# starts an analysis described in filename\n"
                    + "VoxLogicA --help\t# shows this text\n"
                    + "VoxLogicA --ops \t# describes all defined operators\n"
                    + "VoxLogicA -- filename\t# if you need to use a filename like '--help'"  )
        | Ops -> Seq.iter (fun (op : Operator) -> printfn "%s" <| op.Show()) checker.OperatorFactory.Operators
        0
    with 
        | CommandLineException ->
            printfn "Command line error. Try the \"--help\" command line switch."
            1
        | e ->        
            logger.DebugExn e
            logger.Failure "exiting."
            1
