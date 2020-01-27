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

type ModelChecker(model : IModel) =
    let operatorFactory = OperatorFactory(model)
    let formulaFactory = FormulaFactory()       
    let cache = System.Collections.Generic.Dictionary<int,Job<obj>>()            
    let mutable alreadyChecked = 0
    let startChecker i = 
        job {   let iv = new IVar<_>()
                let f = formulaFactory.[i]
                let op = f.Operator                
                do! Job.queue <|
                        Job.tryWith                                                  
                            (job {  // cache.[f'.Uid] below never fails !
                                    // because formula uids give a topological sort of the dependency graph
                                    let! arguments = Job.seqCollect (Array.map (fun (f' : Formula) -> cache.[f'.Uid]) f.Arguments)
                                    let! x = op.Eval (Array.ofSeq arguments)                                                                                                
                                    do! IVar.fill iv x } )
                            (fun exn -> IVar.FillFailure (iv,exn))  
                cache.[i] <- IVar.read iv }
                    
    member __.OperatorFactory = operatorFactory    
    member __.FormulaFactory = formulaFactory
    member __.Check =
        // this method must be called before Get(f) for f added after the previous invocation of Check()
        // corollary: this method must be called at least once before any invocation of Get        
        // It is important that the ordering of formulas is a topological sort of the dependency graph
        // this method should not be invoked concurrently from different threads or concurrently with get
        ErrorMsg.Logger.Debug (sprintf "executing %d tasks" (formulaFactory.Count - alreadyChecked))
        job {   for i = alreadyChecked to formulaFactory.Count - 1 do   
                    do! startChecker i
                alreadyChecked <- formulaFactory.Count                  }
    member __.Get (f : Formula) = cache.[f.Uid]   
        


