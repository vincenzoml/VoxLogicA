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

open FParsec

type UnsupportedTypeException(t : System.Type) =
    inherit BugException(sprintf "System type '%s' cannot be converted to VoxLogicA type (you should improve Types.fs to change this)" t.Name)

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
                let xn = BugException (sprintf "parse error in type declaration %s; the parser reported %A" s f)
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
                    if ! refcount = 0 then
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
            if !refcount >= 0 then refcount := !refcount + 1
            else raise <| RefCountException !refcount)
    }
    abstract member Dereference : unit -> Job<unit>
    default this.Dereference() = job {         
        do! lock refcount (fun () ->
            // ErrorMsg.Logger.Debug <| sprintf"dereference value %d->%d %A" !refcount (!refcount-1) (this.GetHashCode())
            refcount := !refcount - 1            
            if !refcount = 0 && toDispose && not disposed then 
                this.Delete
            else
                job { return () }
        )
    }
