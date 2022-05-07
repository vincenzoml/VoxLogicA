namespace VoxLogicA

// TODO: URGENT: when saving the same formula multiple times, task count is wrong?

open Hopac  

type IWait =  
    abstract member Wait : Job<unit>
type ModelChecker(model : IModel) =
    let mb = BoundedMb<_>(32)
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
    let mutable referenceCount = new IVar<array<ref<int>>>()
    let deref i = job {
        let! rc = IVar.read referenceCount
        let (oldrefc,newrefc) = 
            lock 
                rc.[i] 
                (fun () -> 
                    let r = rc.[i]
                    let o = r.Value 
                    let n = r.Value - 1
                    r.Value <- n
                    (o,n))

        if newrefc <= 0 then 

            let! (y : obj) = IVar.read cache.[i]

            let dispose = 
                try (y :?> IDisposableJob).Dispose
                with :? System.InvalidCastException -> job { return () }
            do! (Job.start dispose)            
    }

    let startChecker i moreWaits = 
        job {
            let iv = cache.[i]
            let f = formulaFactory.[i]
            let op = f.Operator           
            let tokens = new BoundedMb<_>(1000000)
            for i = 0 to 100 do
                do! BoundedMb.put tokens i
            do! Job.start <| job { 
                    let! token = BoundedMb.take tokens
                    do! Job.tryWith
                            (job {  // cache.[f'.Uid] below never fails !
                                    // because formula uids give a topological sort of the dependency graph                                
                                let! arguments' = Job.seqCollect (Array.append (Array.map (fun (f' : Formula) -> cache.[f'.Uid]) f.Arguments) (Array.map (fun i -> cache.[i]) moreWaits) )                                
                                let arguments = Array.sub (Array.ofSeq arguments') 0 (Array.length f.Arguments)
                                let! t = incNumThreads
                                do! model.SignalNumThreads t

                                //                                 f.Operator.Name f.Uid 
                                //                                 (Array.map (fun (f : Formula) -> f.Uid) f.Arguments) 
                                //                                 (Array.map (fun x -> x.GetHashCode()) (Array.ofSeq arguments)))
                                                                
                                let! x = op.Eval (Array.ofSeq arguments)  

                                // ErrorMsg.Logger.Debug (sprintf "Model checker finished: %s (id: %d)" f.Operator.Name f.Uid)
                                // ErrorMsg.Logger.Debug (sprintf "result: %A %A" f.Uid (x.GetHashCode()))                                                                    
                                
                                do! IVar.fill iv x 
                                for uid in Seq.distinct (Seq.map (fun (x : Formula) -> x.Uid) formulaFactory.[i].Arguments) do
                                    do! deref uid            
                            } )
                            (fun exn -> job {

                                do! IVar.FillFailure (iv,exn)                        
                            })  
                    let! t = decNumThreads 
                    do! model.SignalNumThreads t      
                    do! BoundedMb.put tokens token     
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
        let rc = Array.init formulaFactory.Count (fun i -> ref 0)
        do! IVar.fill referenceCount rc        
        for i = 0 to formulaFactory.Count - 1 do            
            cache.[i] <- new IVar<_>()
            for x in formulaFactory.[i].Arguments do            
                rc.[x.Uid].Value <- rc.[x.Uid].Value + 1

        // for i = 0 to formulaFactory.Count - 1 do
            // let f = formulaFactory.[i]
            // printfn "formula: %d operator: %A args: %A refcount: %d" i f.Operator.Name (Array.map (fun (arg : Formula) -> arg.Uid) f.Arguments) !referenceCount.[i]
        //    
        //         for i = 0 to formulaFactory.Count - 1 do                                           

        //             do! startChecker i referenceCount               
        //
        do! Job.start <| job {
            let mutable exit = false            
            while not exit do
                let! r = BoundedMb.take mb
                if r >= 0 then                        
                    do! startChecker r [||] // TODO: URGENT: (if tmp >= 0 then [|tmp|] else [||])
                else exit <- true
        }
    }

    member __.Finish = BoundedMb.put mb -1    

    member __.Get (f : Formula) =  job {
        let! rc = IVar.read referenceCount

        let r = rc.[f.Uid]
        lock r (fun () -> r.Value <- r.Value + 1)

        let mutable frontier = [f.Uid]
        let mutable res = Set.empty
        while frontier <> [] do            
            let hd = List.head frontier   
            frontier <- List.tail frontier                                 
            res <- Set.add hd res // BoundedMb.put mb hd
            for d in Array.map (fun (x : Formula) -> x.Uid) (formulaFactory.[hd].Arguments) do
                frontier <- d::frontier      
        
        do! Lock.duringJob globalLock <| job {
            for task in List.sort (Set.toList res) do
                if (not (inProgress.ContainsKey task)) &&  (not (IVar.Now.isFull cache.[task]))
                then 
                    do! BoundedMb.put mb task
                    let! t = incNumThreads
                    do! model.SignalNumThreads t              
        }
        let! result = IVar.read cache.[f.Uid]
        let! t = decNumThreads
        do! model.SignalNumThreads t
        return result        
    }

    member __.Unref (f : Formula) = job {
        do! deref f.Uid
    }

        
