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

namespace VoxLogicA

open Hopac  
open System
type ModelChecker(model : IModel) =
    let operatorFactory = OperatorFactory(model)
    let formulaFactory = FormulaFactory()       
    let cache = System.Collections.Generic.Dictionary<int,Job<obj>>()            
    let mutable alreadyChecked = 0
    let startChecker i (referenceCount : array<ref<int>>) = 
        job {   let iv = new IVar<_>()
                let f = formulaFactory.[i]
                let op = f.Operator                
                do! Job.queue <|
                        Job.tryWith                                                  
                            (job {  // cache.[f'.Uid] below never fails !
                                    // because formula uids give a topological sort of the dependency graph
                                    let! arguments = Job.seqCollect (Array.map (fun (f' : Formula) -> cache.[f'.Uid]) f.Arguments)
                                    // ErrorMsg.Logger.DebugOnly (sprintf "About to execute: %s (id: %d)" f.Operator.Name f.Uid)
                                    // ErrorMsg.Logger.DebugOnly (sprintf "Arguments: %A" (Array.map (fun x -> x.GetHashCode()) (Array.ofSeq arguments)))
                                    

                                    /// A GLOBAL ARRAY OF LOCKS AND A GLOBAL ARRAY OF REFERENCE COUNTS
                                    /// SEE ALSO: https://stackoverflow.com/questions/41652195/dispose-pattern-in-f
                                    printfn "about to compute %d" i
                                    let! x = op.Eval (Array.ofSeq arguments)  
                                    printfn "computed %d" i
                                    for arg in formulaFactory.[i].Arguments do
                                        let (oldrefc,newrefc) = 
                                            lock 
                                                referenceCount.[arg.Uid] 
                                                (fun () -> 
                                                    let r = referenceCount.[arg.Uid]
                                                    let o = !r 
                                                    let n = !r - 1
                                                    r := n
                                                    (o,n))
                                        printfn "  arg: %d oldrefs: %d newrefs: %d" arg.Uid oldrefc newrefc
                                        if newrefc = 0 then 
                                            printfn "  disposing %d" arg.Uid
                                            let dispose = 
                                                try (x :?> IDisposableJob).Dispose
                                                with :? InvalidCastException -> job { return () }
                                            do! (Job.start dispose)

                                    /// after this, lock, reference counts of arguments - 1, GC eventually, unlock
                                                                                    
                                    // ErrorMsg.Logger.DebugOnly (sprintf "Finished: %s (id: %d)" f.Operator.Name f.Uid)
                                    // ErrorMsg.Logger.DebugOnly (sprintf "Result: %A" <| x.GetHashCode())                                               
                                    do! IVar.fill iv x } )
                            (fun exn -> ErrorMsg.Logger.DebugOnly (exn.ToString()); IVar.FillFailure (iv,exn))  
                cache.[i] <- IVar.read iv }
                    
    member __.OperatorFactory = operatorFactory    
    member __.FormulaFactory = formulaFactory
    member __.Check =
        // this method must be called before Get(f) for f added after the previous invocation of Check()
        // corollary: this method must be called at least once before any invocation of Get        
        // It is important that the ordering of formulas is a topological sort of the dependency graph
        // this method should not be invoked concurrently from different threads or concurrently with get
        ErrorMsg.Logger.Debug (sprintf "Running %d tasks" (formulaFactory.Count - alreadyChecked))
        let referenceCount = Array.init formulaFactory.Count (fun i -> ref 0)
        for i = 0 to formulaFactory.Count - 1 do            
            for x in formulaFactory.[i].Arguments do
                referenceCount.[x.Uid] := !referenceCount.[x.Uid] + 1
        for i = 0 to formulaFactory.Count - 1 do
            let f = formulaFactory.[i]
            printfn "formula: %d operator: %A args: %A refcount: %d" i f.Operator.Name (Array.map (fun (arg : Formula) -> arg.Uid) f.Arguments) !referenceCount.[i]
        job {   for i = alreadyChecked to formulaFactory.Count - 1 do                                           
                    //ErrorMsg.Logger.Debug (sprintf "Starting task %d" i)
                    do! startChecker i referenceCount                   
                alreadyChecked <- formulaFactory.Count                  }
    member __.Get (f : Formula) = cache.[f.Uid]   
        


