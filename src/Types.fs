namespace VoxLogicA

open Hopac

open FParsec


exception UnsupportedTypeException of t : System.Type
    with override this.Message = sprintf "System type '%s' cannot be converted to VoxLogicA type (you should improve Types.fs to change this)" this.t.Name


exception ParserError of s : string
    with override this.Message = sprintf "Parser error:"

type Type = 
    TModel | TNumber |TBool | TString | TValuation of Type
    static member OfSystemType t =
        if t = typeof<float32> then TNumber
        else if t = typeof<bool> then TBool
        else if t = typeof<string> then TString
        else raise (UnsupportedTypeException t)
    static member Parse(s : string) = 
        let parser,parserImpl = createParserForwardedToRef()
        parserImpl :=           
                choice [
                    skipString "number" >>% TNumber
                    skipString "bool" >>% TBool
                    skipString "string" >>% TString
                    skipString "model" >>% TModel
                    skipString "valuation" >>. between (skipString "(") (skipString ")") parser |>> TValuation
                ]            
        match (run parser s) with  
            | Success (t,_,_) -> t
            | Failure _ as f -> 
                let xn = ParserError(sprintf "parse error in type declaration %s; the parser reported %A" s f)
                xn.Data.[0] <- f
                raise xn
                
    override this.ToString() =
        match this with
            | TModel -> "model"
            | TNumber -> "number"
            | TBool -> "bool"
            | TString -> "string"
            | TValuation(t)-> sprintf "valuation(%s)" (t.ToString())
    static member StringOfArgumentsStringType (ta : array<string>) =      // TODO: can this be generalised and moved to Util?  
        if ta.Length = 0 then ""
        else if ta.Length = 1 then sprintf "%s" (ta.[0].ToString())
        else 
            let mutable res = sprintf "("
            for i = 0 to ta.Length - 2 do res <- res + (ta.[i].ToString()) + ","
            res <- res + ta.[ta.Length - 1].ToString() + ")"
            res
                        
type IDisposableJob =
    abstract member Dispose : Hopac.Job<unit>

exception RefCountException of r : int
    with override this.Message = sprintf "Value referenced, with reference count already %d, which is less than 0. This should never happen, please report it as a bug." this.r
type RefCount() =
    let refcount = ref 0
    let mutable toDispose = false
    let mutable disposed = false
    interface IDisposableJob with
        member this.Dispose = 
            lock refcount (fun () -> job {
                if not disposed then
                    if !refcount = 0 then
                        disposed <- true
                        do! this.Delete
                    else
                        toDispose <- true
            })        
    
    abstract member Delete : Job<unit>
    default this.Delete =         
        job { ErrorMsg.Logger.Debug <| sprintf "Stub: Called Delete on object %A." this }

    abstract member Reference : unit -> Job<unit>
    default this.Reference () = job {
        lock refcount (fun () -> 
            // ErrorMsg.Logger.Debug <| sprintf "reference value %d->%d %A" !refcount (!refcount+1) (this.GetHashCode())
            if refcount.Value >= 0 then refcount.Value <- refcount.Value + 1
            else raise <| RefCountException !refcount)
    }
    abstract member Dereference : unit -> Job<unit>
    default this.Dereference() = job {         
        do! lock refcount (fun () ->
            // ErrorMsg.Logger.Debug <| sprintf"dereference value %d->%d %A" !refcount (!refcount-1) (this.GetHashCode())
            refcount.Value <- refcount.Value - 1            
            if refcount.Value = 0 && toDispose && not disposed then 
                disposed <- true
                this.Delete
            else
                job { return () }
        )
    }
