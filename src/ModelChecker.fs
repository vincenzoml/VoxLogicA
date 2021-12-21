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

// TODO: URGENT: when saving the same formula multiple times, task count is wrong?

open Hopac  

type IWait =  
    abstract member Wait : Job<unit>
type ModelChecker(model : IModel) =
    let globalLock = Lock()
    let operatorFactory = OperatorFactory(model)
    let formulaFactory = FormulaFactory()       
    let cache = System.Collections.Generic.Dictionary<int,IVar<_>>()    
    let inProgress = System.Collections.Generic.Dictionary<int,unit>() 

    let incNumThreads,decNumThreads,setNumThreads = 
        let threadCountLck = Lock()
        let mutable threadCount = 0
        (
            (Lock.duringFun threadCountLck (fun () -> threadCount <- threadCount + 1; threadCount)),
            (Lock.duringFun threadCountLck (fun () -> threadCount <- threadCount - 1; threadCount)),
            (fun n -> Lock.duringFun threadCountLck (fun () -> threadCount <- n))
        )
        
    let mutable dotFileName = ""
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
            ErrorMsg.Logger.DebugOnly <| sprintf "Model checker disposing %d" i
            let! (y : obj) = IVar.read cache.[i]
            ErrorMsg.Logger.DebugOnly <| sprintf "Model checker read from cache for disposal: %d=%A" i (y.GetHashCode())
            let dispose = 
                try (y :?> IDisposableJob).Dispose
                with :? System.InvalidCastException -> job { return () }
            do! (Job.start dispose)            
    }

    // WHERE IN THE WORLD IS CARMEN SANDIEGO? let l = Lock()
    // WHERE IN THE WORLD IS CARMEN SANDIEGO? let mutable v = 0

    let startChecker i (referenceCount : array<ref<int>>) = 
        job {
            let iv = cache.[i]
            let f = formulaFactory.[i]
            let op = f.Operator           
            do! Job.start <| job { 
                    do! Job.tryWith
                            (job {  // cache.[f'.Uid] below never fails !
                                    // because formula uids give a topological sort of the dependency graph                                
                                let! arguments = Job.seqCollect (Array.map (fun (f' : Formula) -> cache.[f'.Uid]) f.Arguments)     
                                let! t = incNumThreads
                                // WHERE IN THE WORLD IS CARMEN SANDIEGO? do! Lock.duringFun l (fun () -> (v <- v+1; printfn "V: %A" v))
                                do! model.SignalNumThreads t
                                ErrorMsg.Logger.DebugOnly (sprintf "Model checker running: %s (id: %d)\nArguments: %A\nHash codes: %A" 
                                                                f.Operator.Name f.Uid 
                                                                (Array.map (fun (f : Formula) -> f.Uid) f.Arguments) 
                                                                (Array.map (fun x -> x.GetHashCode()) (Array.ofSeq arguments)))
                                

                                /// A GLOBAL ARRAY OF LOCKS AND A GLOBAL ARRAY OF REFERENCE COUNTS
                                /// SEE ALSO: https://stackoverflow.com/questions/41652195/dispose-pattern-in-f
                                // printfn "about to compute %d" i
                                
                                
                                // WHERE IN THE WORLD IS CARMEN SANDIEGO? do! Lock.duringFun l (fun () -> (v <- v-1; printfn "V': %A" v))
                                
                                let! x = op.Eval (Array.ofSeq arguments)  

                                ErrorMsg.Logger.DebugOnly (sprintf "Model checker finished: %s (id: %d)\nresult: %A" f.Operator.Name f.Uid (x.GetHashCode()))                                                                    
                                do! IVar.fill iv x 
                                for uid in Seq.distinct (Seq.map (fun (x : Formula) -> x.Uid) formulaFactory.[i].Arguments) do
                                    do! deref uid                              
                            } )
                            (fun exn -> job {
                                ErrorMsg.Logger.DebugOnly (exn.ToString())
                                do! IVar.FillFailure (iv,exn)                        
                            })  
                    let! t = decNumThreads                    
                    do! model.SignalNumThreads t      
            }
            cache.[i] <- iv 
        }
                    
    member __.OperatorFactory = operatorFactory    
    member __.FormulaFactory = formulaFactory
    member __.WriteOnlyDot filename =
        dotFileName <- filename
    member this.Check = job {
        // this method must be called before Get(f) for f added after the previous invocation of Check()
        // corollary: this method must be called at least once before any invocation of Get        
        // It is important that the ordering of formulas is a topological sort of the dependency graph
        // this method should not be invoked concurrently from different threads or concurrently with get
        
        if dotFileName <> "" then 
            System.IO.File.WriteAllText(dotFileName,this.FormulaFactory.AsDot)
            exit 0
        ErrorMsg.Logger.Debug (sprintf "Running %d tasks" formulaFactory.Count)        
        referenceCount <- Array.init formulaFactory.Count (fun i -> ref 0)
        for i = 0 to formulaFactory.Count - 1 do            
            cache.[i] <- new IVar<_>()
            for x in formulaFactory.[i].Arguments do
                referenceCount.[x.Uid].Value <- referenceCount.[x.Uid].Value + 1

        // for i = 0 to formulaFactory.Count - 1 do
            // let f = formulaFactory.[i]
            // printfn "formula: %d operator: %A args: %A refcount: %d" i f.Operator.Name (Array.map (fun (arg : Formula) -> arg.Uid) f.Arguments) !referenceCount.[i]
        //    
        //         for i = 0 to formulaFactory.Count - 1 do                                           
        //             ErrorMsg.Logger.DebugOnly (sprintf "Starting task %d" i)
        //             do! startChecker i referenceCount               
        //
    }     

    member __.Get (f : Formula) =  job {
        ErrorMsg.Logger.DebugOnly <| sprintf "GET %A" f.Uid
        let r = referenceCount.[f.Uid]
        lock r (fun () -> r.Value <- r.Value + 1)
        return! Lock.duringJob globalLock <| job {
            let mutable frontier = [f.Uid]
            let mutable visit = []
            while frontier <> [] do
                let hd = List.head frontier   
                frontier <- List.tail frontier             
                if not (inProgress.ContainsKey hd) then 
                    inProgress.[hd] <- ()
                    do! startChecker hd referenceCount
                for d in Array.map (fun (x : Formula) -> x.Uid) (formulaFactory.[hd].Arguments) do
                    frontier <- d::frontier        
            let! result = IVar.read cache.[f.Uid]
            let! t = incNumThreads
            do! model.SignalNumThreads t
            return result
        }
    }

    member __.Unref (f : Formula) = job {
        do! deref f.Uid
        let! t = decNumThreads
        do! model.SignalNumThreads t   
    }

        
