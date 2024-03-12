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
open System.Collections.Generic
open FSharp.Json

exception NoModelLoadedException 
    with override __.Message = "No model loaded"


exception MoreThanOneModelUnsupportedException
    with override __.Message = "Loading more than one graph is not yet supported"

open Hopac

module Lifts =
    let lift = Job.lift
    let lift2 = fun fn x y  -> job { return fn x y }

open Lifts

open Poset

open Truth

type SITKModel() =    
    inherit IModel()
    let mutable basePoset : option<Poset> = None
    let getBasePoset() = match basePoset with None -> raise NoModelLoadedException | Some poset -> poset
    let atoms = Dictionary<_,_>()
        
    let supportedExtensions = [".json"] // TODO: make this list exhaustive
    
    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.        
        // TODO: changed save policy with OnExit
        true
        // match t with 
        // | (TValuation(TBool)) when List.exists (f.EndsWith : string -> bool) supportedExtensions -> true 
        // | _ -> false

    override __.Save atomName v =
        let t = v :?> Truth
        atoms.Add(atomName,t)
            
    override __.Load s =
        let poset = loadPoset s
        let res = 
            match basePoset with
            | None -> 
                basePoset <- Some poset
                poset
            | Some _ ->                
                raise MoreThanOneModelUnsupportedException
        res :> obj

    override __.OnExit () =
        let atomsToPrint =
            (atoms :> seq<_>)
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
        System.IO.File.WriteAllText("result.json", Json.serialize atomsToPrint)

    interface IAtomicModel<Truth> with
        member __.Ap s = job { 
                let poset = getBasePoset()
                let ap = getPosetAp poset s
                let res = Array.create poset.size false 
                Set.iter (fun idx -> res.[idx] <- true) ap
                return res
            }

    // interface IBooleanModel<Truth> with
    //     member __.TT = job { return TT(getBaseTriaGraph().NumSimplexes) }
    //     member __.FF = job { return FF(getBaseTriaGraph().NumSimplexes) }
    //     member __.BConst v = lift (BConst (getBaseTriaGraph().NumSimplexes)) v  
    //     member __.And v1 v2 = lift2 And v1 v2
    //     member __.Or v1 v2 = lift2 Or v1 v2
    //     member __.Not v = lift Not v
 
    // interface ISpatialModel<Truth> with
    //     member __.Near v = job { return (downClosure (getBaseTriaGraph()) v) } 
    //     member __.Interior v = job { return (interior (getBaseTriaGraph()) v) }
    //     member __.Through v1 v2 = job {return (reach (getBaseTriaGraph()) v1 v2)}           
   