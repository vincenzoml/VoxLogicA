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

exception NoModelLoadedException with
    override __.Message = "No model loaded"


exception MoreThanOneModelUnsupportedException with
    override __.Message = "Loading more than one graph is not yet supported"

open Hopac

module Lifts =
    let lift = Job.lift
    let lift2 = fun fn x y -> job { return fn x y }

open Lifts

open Poset

open Truth



type InternalData = 
    {
        poset: Poset;
        props: Dictionary<string,Truth>;
        ups: array<list<int>>;
        downs: array<list<int>>;
        boths: array<Set<int>>
    }

type PosetModel() =
    inherit IModel()
    let mutable internalData: option<InternalData> = None

    let getBasePoset () =
        match internalData with
        | None -> raise NoModelLoadedException
        | Some x -> x.poset

    let toSave: Dictionary<string, Truth>= Dictionary()

    let closure v =
        let poset = getBasePoset()
        let mutable idxs = []
        let rec findIndexes arr el acc =
            if arr <> Array.empty then
                if el = Array.head arr then
                    //printfn "append"
                    idxs <- List.append idxs [(acc)]
                    findIndexes (Array.tail arr) el (acc + 1)
                else
                    findIndexes (Array.tail arr) el (acc + 1)
        findIndexes v true 0
        //printfn "idxs: %A" idxs
        let myModel = match internalData with
                        | None -> raise NoModelLoadedException
                        | Some x -> x
        let mutable localTruths = [for _ in 1..poset.points.Length -> (FF poset.points.Length)]
        for elem in idxs do
            //printfn "ciao"
            //printfn "%A" elem
            for i in myModel.downs[elem] do
                localTruths[elem][i] <- true
        let mutable result = FF poset.points.Length
        for elem in localTruths do
            result <- Or result elem
        Or result v
        
    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.
        true

    override __.Save name v =
        let t = v :?> Truth        
        try 
            //printfn "try to save"
            toSave.Add(name, t)
        with e -> 
            printfn "error in adding formula %A to the save list" name
            raise e


    override __.Load s =
        let poset = loadPoset s

        let res: Poset =
            match internalData with
            | None ->  
                let props = Dictionary()                                                          
                let ups = Array.create poset.points.Length []
                let downs = Array.create poset.points.Length []
                let boths = Array.create poset.points.Length Set.empty 
                List.iteri
                    (fun idx (point:Point) ->
                        //printfn "try to add props"
                        assert ((int point.id) = idx)
                        ups[idx] <- List.map int point.up
                        List.iter
                            (fun atom ->
                                if not (props.ContainsKey atom) then
                                    try 
                                        props.Add(atom, FF poset.points.Length)
                                    with e -> 
                                        printfn "error in adding atom %A to the atoms list" atom
                                        raise e                                    
                                let v : Truth = props[atom]
                                v[idx] <- true)
                            point.atoms
                        List.iter
                            (fun target_idx -> 
                                //printfn "try to add downs"
                                downs[target_idx] <- (idx)::downs[target_idx]
                                //
                            )
                            ups[idx]
                        //printfn "downs idx: %A %A" idx downs[idx]
                        List.iter
                            (fun target_idx ->
                                if List.contains idx ups[target_idx] && List.contains target_idx ups[idx] then
                                    boths[idx] <- Set.add target_idx boths[idx]
                                //else
                                //    printfn "crepa maledetto"
                            )
                            ups[idx]
                        internalData <- Some {
                            poset = poset
                            props = props
                            ups = ups
                            downs = downs
                            boths = boths
                        }    
                        // TODO: inizializzare boths[idx] come unione di ups e downs
                    )
                    poset.points
                poset
            | Some _ -> raise MoreThanOneModelUnsupportedException

        res :> obj

    override __.OnExit() =
        let atomsToPrint = (toSave :> seq<_>) |> Seq.map (|KeyValue|) |> Map.ofSeq
        System.IO.File.WriteAllText("result.json", Json.serialize atomsToPrint)

    interface IAtomicModel<Truth> with
        member __.Ap s =
            //printfn "try to save prop"
            job { 
                match internalData with
                | Some x -> return x.props[s]
                | None -> return FF(getBasePoset().points.Length)
            }

    interface IBooleanModel<Truth> with
        member __.TT = job { return TT(getBasePoset().points.Length) }
        member __.FF = job { return FF(getBasePoset().points.Length) }
        member __.BConst v = lift (BConst (getBasePoset().points.Length)) v
        member __.And v1 v2 = lift2 And v1 v2
        member __.Or v1 v2 = lift2 Or v1 v2
        member __.Not v = lift Not v

    interface ISpatialModel<Truth> with
        member __.Near v = job {
           return closure v
         }
        member __.Interior v = job { 
           let compV = Not v
           let closCompV = closure compV
           return Not closCompV
         }
        member __.Through v1 v2 = job {return (v2)}