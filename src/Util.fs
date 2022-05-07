namespace VoxLogicA
open System

type JSonOutput = FSharp.Data.JsonProvider<"example.json">



module Util =
    let mutable private debugFlag = false
    #if DEBUG
    do debugFlag <- true
    #endif
    let isDebug () = debugFlag
    module Concurrent =
        open Hopac

        let conIgnore (s : array<#Job<unit>>) = // The implementation of the same function in Hopac ("conIgnore") does not queue all jobs immediately; we want this one.
            job {
                let x = Array.init (Array.length s) (fun _ -> new IVar<_>()) // Use Latch instead?
                for i = 0 to s.Length - 1 do
                    queue <| 
                        Job.tryWith 
                            (job {  do! (s.[i] :> Job<unit>)
                                    do! (IVar.fill x.[i] None)  } )
                            (fun exn -> IVar.fill x.[i] (Some exn))
                for i = 0 to s.Length - 1 do
                    match! IVar.read x.[i] with
                    | None -> () 
                    | Some exn -> 
                        if isDebug() then
                            printfn "\n\n\n%A\n\n\n" <| exn.ToString()
                        raise exn
            }

        open System.Reflection

        type Converter =
            static member private fn<'a> (x : Hopac.Job<'a>) = 
                job {   let! y = x
                        return y :> obj }

        let CastMemberToJob(object : obj, method : System.Reflection.MethodInfo) =   
                // TODO: the following line gets the 'a in Job<'a> but doesn't check the "Job" part.    
                let methodRT = method.ReturnType.GetGenericArguments().[0] 
                let m = typeof<Converter>.GetMethod("fn",BindingFlags.Static|||BindingFlags.NonPublic)
                let gm = m.MakeGenericMethod(methodRT)     
                fun parameters ->
                    job {                        
                            let jobobj = method.Invoke(object,parameters) // jobobj is a Job<'a>
                            let res = gm.Invoke(null,[|jobobj|])
                            return! (res :?> Hopac.Job<obj>)
                    }


        let CastPropertyToJob(object : obj, property : System.Reflection.PropertyInfo) =   
                // TODO: the following line gets the 'a in Job<'a> but doesn't check the "Job" part.
                let propertyRT = property.PropertyType.GetGenericArguments().[0] 
                let m = typeof<Converter>.GetMethod("fn",BindingFlags.Static|||BindingFlags.NonPublic)
                let gm = m.MakeGenericMethod(propertyRT)     
                fun (parameters : array<obj>) ->
                    job {
                        if parameters.Length > 0 then raise (new ArgumentException())                    
                        let jobobj = property.GetValue(object) // jobobj is a Job<'a>
                        let res = gm.Invoke(null,[|jobobj|])
                        return! (res :?> Hopac.Job<obj>)
                    }                

