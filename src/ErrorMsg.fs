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

module VoxLogicA.ErrorMsg 
open System.IO
// TODO: convert into a new type called Logger 

let mutable private debugFlag = false
#if DEBUG
do debugFlag <- true
#endif
let isDebug() = true

type Logger private () =        
        static let streamWriter = new StreamWriter(System.Console.OpenStandardOutput())
        static let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        static let mutable destinations = [streamWriter] // this is a placeholder for permitting more simultaneous destinations.         
        
        static let print prefix (string : string) =            
            let printer destination = 
                lock destination 
                    (fun () ->
                        fprintfn destination "[%10dms] [%s] %s" 
                            stopWatch.ElapsedMilliseconds 
                            prefix 
                            (string.Replace("\n","\n                      "))
                        destination.Flush())
            List.iter printer destinations               

        // private new() = Logger(new StreamWriter(System.Console.OpenStandardOutput()))
        // private new(filename : string) = Logger(new StreamWriter(filename))

        static member Debug s =  print "info" s
        static member DebugOnly s = if isDebug() then print "dbug" s
        static member Warning s = print "warn" s
        static member Failure s = print "fail" s
        static member Result name value = print "user" (sprintf "%s=%A" name value)
        static member DebugExn (exn : exn) = Logger.Debug <| if isDebug() then exn.ToString() else exn.Message
        