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

exception RefCountException of r : int
    with override this.Message = sprintf "Value referenced, with reference count already %d, which is less or equal than 0. This should never happen, please report it as a bug." this.r

type RefCount() =
    let refcount = ref 1
    interface System.IDisposable with
        member this.Dispose() =
            ErrorMsg.Logger.Debug <| sprint "Stub: Called Dispose of %A : RefCount with reference count %d" this !refcount
    
    member this.Delete() = 
        ErrorMsg.Logger.Debug <| sprintf "Stub: Called Delete on object %A." this
    abstract member Reference : unit -> unit
    default this.Reference () =
        lock refcount (fun () -> 
            // ErrorMsg.Logger.Debug <| sprintf "reference value %d->%d %A" !refcount (!refcount+1) this
            if !refcount > 0 then refcount := !refcount + 1
            else raise <| RefCountException !refcount)
    abstract member Dereference : unit -> unit
    default this.Dereference() =         
        lock refcount (fun () ->
            // ErrorMsg.Logger.Debug <| sprintf"dereference value %d->%d %A" !refcount (!refcount-1) this
            refcount := !refcount - 1
            if !refcount = 0 then 
                this.Delete())

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
                                    // ErrorMsg.Logger.DebugOnly (sprintf "About to execute: %s (id: %d)" f.Operator.Name f.Uid)
                                    // ErrorMsg.Logger.DebugOnly (sprintf "Arguments: %A" (Array.map (fun x -> x.GetHashCode()) (Array.ofSeq arguments)))
                                    let! x = op.Eval (Array.ofSeq arguments)                                                 
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
        job {   for i = alreadyChecked to formulaFactory.Count - 1 do   
                    // ErrorMsg.Logger.DebugOnly (sprintf "Starting task %d" i)
                    do! startChecker i                    
                alreadyChecked <- formulaFactory.Count                  }
    member __.Get (f : Formula) = cache.[f.Uid]   
        


