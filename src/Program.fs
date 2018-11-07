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


let parseCmdLine (argv : array<string>) =
    match argv.Length with
    | 1 -> argv.[0] 
    | _ -> raise CommandLineException 
    
[<EntryPoint>]
let main (argv : string array) = 
    let logger = ErrorMsg.Logger()        
    try
        let filename = parseCmdLine argv
        let model = SITKModel() :> IModel
        let checker = ModelChecker(model)                
        let interpreter = Interpreter(model,checker,logger)
        interpreter.Batch interpreter.DefaultLibDir (System.IO.Path.GetFullPath ".") (System.IO.Path.GetFullPath ".") filename                      
        0
    with e ->
        logger.DebugExn e
        logger.Failure "exiting."
        1
