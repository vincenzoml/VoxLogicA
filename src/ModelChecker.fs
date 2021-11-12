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

type IWait =  
    abstract member Wait : Job<unit>
type ModelChecker(model : IModel) =
    let operatorFactory = OperatorFactory(model)
    let formulaFactory = FormulaFactory()       
    let cache = System.Collections.Generic.Dictionary<int,Job<obj>>()            
    let mutable alreadyChecked = 0
    let mutable referenceCount = Array.init 0 (fun i -> ref 0)
    let deref i = job {
        let (oldrefc,newrefc) = 
            lock 
                referenceCount.[i] 
                (fun () -> 
                    let r = referenceCount.[i]
                    let o = r.Value 
                    let n = r.Value - 1
                    r.Value <- n
                    (o,n))
        ErrorMsg.Logger.DebugOnly <| sprintf "Model checker deref: %d oldrefs: %d newrefs: %d" i oldrefc newrefc
        if newrefc <= 0 then 
            ErrorMsg.Logger.Debug <| sprintf "Model checker disposing %d" i
            let! y = cache.[i]
            ErrorMsg.Logger.DebugOnly <| sprintf "Model checker read from cache: %d=%A" i (y.GetHashCode())
            let dispose = 
                try (y :?> IDisposableJob).Dispose
                with :? System.InvalidCastException -> job { return () }
            do! (Job.start dispose)            
    }
    let startChecker i (referenceCount : array<ref<int>>) = 
        job {   
            let iv = new IVar<_>()
            let f = formulaFactory.[i]
            let op = f.Operator                
            do! Job.start <|
                    Job.tryWith                                                  
                        (job {  // cache.[f'.Uid] below never fails !
                                // because formula uids give a topological sort of the dependency graph
                            let! arguments = Job.seqCollect (Array.map (fun (f' : Formula) -> cache.[f'.Uid]) f.Arguments)
                            ErrorMsg.Logger.DebugOnly (sprintf "Model checker running: %s (id: %d)\nArguments: %A\nHash codes: %A" 
                                                            f.Operator.Name f.Uid 
                                                            (Array.map (fun (f : Formula) -> f.Uid) f.Arguments) 
                                                            (Array.map (fun x -> x.GetHashCode()) (Array.ofSeq arguments)))
                            

                            /// A GLOBAL ARRAY OF LOCKS AND A GLOBAL ARRAY OF REFERENCE COUNTS
                            /// SEE ALSO: https://stackoverflow.com/questions/41652195/dispose-pattern-in-f
                            // printfn "about to compute %d" i
                            let! x = op.Eval (Array.ofSeq arguments)  
                                                                            
                            ErrorMsg.Logger.DebugOnly (sprintf "Model checker finished: %s (id: %d)\nresult: %A" f.Operator.Name f.Uid (x.GetHashCode()))                                                                    
                            do! IVar.fill iv x 
                            for uid in Seq.distinct (Seq.map (fun (x : Formula) -> x.Uid) formulaFactory.[i].Arguments) do
                                do! deref uid
                                } )
                        (fun exn -> ErrorMsg.Logger.DebugOnly (exn.ToString()); IVar.FillFailure (iv,exn))  
            cache.[i] <- IVar.read iv }
                    
    member __.OperatorFactory = operatorFactory    
    member __.FormulaFactory = formulaFactory
    member this.Check =
        // this method must be called before Get(f) for f added after the previous invocation of Check()
        // corollary: this method must be called at least once before any invocation of Get        
        // It is important that the ordering of formulas is a topological sort of the dependency graph
        // this method should not be invoked concurrently from different threads or concurrently with get
        ErrorMsg.Logger.Debug (sprintf "Running %d tasks" (formulaFactory.Count - alreadyChecked))
        // if ErrorMsg.isDebug() then
        System.IO.File.WriteAllText("DebugFormulas.dot",this.FormulaFactory.AsDot)
        referenceCount <- Array.init formulaFactory.Count (fun i -> ref 0)
        for i = 0 to formulaFactory.Count - 1 do            
            for x in formulaFactory.[i].Arguments do
                referenceCount.[x.Uid].Value <- referenceCount.[x.Uid].Value + 1
        // for i = 0 to formulaFactory.Count - 1 do
            // let f = formulaFactory.[i]
            // printfn "formula: %d operator: %A args: %A refcount: %d" i f.Operator.Name (Array.map (fun (arg : Formula) -> arg.Uid) f.Arguments) !referenceCount.[i]
        job {   
                for i = alreadyChecked to formulaFactory.Count - 1 do                                           
                    ErrorMsg.Logger.Debug (sprintf "Starting task %d" i)
                    do! startChecker i referenceCount      
                    if i % 1 = 0 then // TODO: URGENT: study this
                            ErrorMsg.Logger.DebugOnly <| sprintf "Model checker: Attempting to wait for Uid %A" i
                            let! x = cache.[i]
                            ErrorMsg.Logger.DebugOnly <| sprintf "Model checker: Starting to wait for Uid %A" i
                            do! job {                   
                                        do! 
                                            try (x :?> IWait).Wait
                                            with _ -> Job.result ()
                                           
                                        ErrorMsg.Logger.DebugOnly <| sprintf "Model checker: Finished waiting for Uid %A" i                        
                                    }                                
                                                           
                alreadyChecked <- formulaFactory.Count                  
            }

    member __.Get (f : Formula) =  
        let r = referenceCount.[f.Uid]
        lock r (fun () -> r.Value <- r.Value + 1) 
        cache.[f.Uid]   

    member __.Unref (f : Formula) =
        deref f.Uid
        
