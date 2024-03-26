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
    }

type PosetModel() =
    inherit IModel()
    let mutable internalData: option<InternalData> = None

    let getBasePoset () =
        match internalData with
        | None -> raise NoModelLoadedException
        | Some x -> x.poset

    let toSave: Dictionary<string, Truth>= Dictionary()

    let rec findAllIndexes (arr) (b) (acc) : int list =
            //printfn "%A" arr
            let index = 
                match Array.tryFindIndex (fun x -> x = b) arr with
                | None -> -1
                | Some x -> x
            let indexes = 
                if index <> -1 then
                    let newArray = Array.removeAt index arr 
                    (index + acc)::findAllIndexes newArray b (acc + 1)
                else
                    []
            indexes

    let rec connComponent explored currComponent next toVisit =
        match toVisit with
        | [] -> (explored,currComponent)
        | current::rest ->
            let newExplored = Set.add current explored
            if not (Set.contains current explored) then
                let newOnes = Set.difference (next current) newExplored
                connComponent newExplored (Set.add current currComponent) next (Set.toList (Set.union newOnes (Set rest)))
            else
                connComponent newExplored currComponent next rest
    
    let connComponents (v : Truth) =
        let mutable explored = Set.empty
        let mutable components = Set.empty
        let model = match internalData with
                    | None -> raise NoModelLoadedException
                    | Some x -> x
        let next idx = 
            assert v[idx]
            Set.filter (Array.get v) (Set.ofList (List.append model.ups[idx] model.downs[idx]))
        for idx in 0 .. model.poset.points.Length - 1 do
            if (v[idx]) then 
                let (newExplored,currentComponent) = connComponent explored Set.empty next [idx]
                if not (Set.isEmpty currentComponent) 
                then components <- Set.add currentComponent components
                explored <- newExplored
            
        components

    let rec createComponentsTruthValues ccs acc (v : Truth) =
            match ccs with
            | [] -> acc
            | current::rest -> 
                let tmp = FF v.Length
                for idx in current do tmp[idx] <- true
                let newAcc = tmp::acc
                createComponentsTruthValues rest newAcc v
    let closure v =
        let poset = getBasePoset()
        let idxs = findAllIndexes v true 0
        let myModel = match internalData with
                        | None -> raise NoModelLoadedException
                        | Some x -> x
        let mutable localTruths = [for _ in 1..poset.points.Length -> (FF poset.points.Length)]
        for elem in idxs do
            for i in myModel.downs[elem] do
                localTruths[elem][i] <- true
        let mutable result = FF poset.points.Length
        for elem in localTruths do
            result <- Or result elem
        Or result v

    let gamma (v1 : Truth) (v2 : Truth) =
        let components = connComponents v1
        let truthComponentValues = createComponentsTruthValues (Set.toList components) [] v1
        let rec computeClosures components acc1 =
            match components with
            | [] -> acc1
            | current::rest -> computeClosures rest ((closure current)::acc1)
        let closures = computeClosures truthComponentValues []
        let rec computeGamma (closures : list<Truth>) acc =
            match closures with
            | [] -> acc
            | current::rest ->
                let zip = Array.zip [|for i in 0..current.Length-1 -> i|] current
                let check = Array.filter (fun (i, tv) -> tv = true && v2[i] = tv) zip
                if check <> [||] then
                    computeGamma rest (Or current acc)
                else
                    computeGamma rest acc
        computeGamma closures (FF v1.Length)

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
        ErrorMsg.Logger.DebugOnly (sprintf "Loading %A" s)
        let poset = loadPoset s

        let res: Poset =
            match internalData with
            | None ->  
                let props = Dictionary()                                                          
                let ups = Array.create poset.points.Length []
                let downs = Array.create poset.points.Length []
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
                        internalData <- Some {
                            poset = poset
                            props = props
                            ups = ups
                            downs = downs
                        }    
                    )
                    poset.points
                ErrorMsg.Logger.DebugOnly (sprintf "Loaded %A" s)    
                poset
            | Some _ -> raise MoreThanOneModelUnsupportedException

        res :> obj

    override __.OnExit() =
        let ts = (toSave :> seq<_>)
        let atomsToPrint = ts |> Seq.map (|KeyValue|) |> Map.ofSeq
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
        member __.Through v1 v2 = job {
            let ccs = connComponents v1
            let comps = createComponentsTruthValues (Set.toList ccs) [] v1 
            let mutable result = FF v1.Length
            let mutable intermediates = []
            for comp in comps do
                for i in 0..comps.Length-1 do
                    if v2[i] && comp[i] then
                        intermediates <- comp::intermediates
            for intr in intermediates do
                result <- Or result intr
            return result
        }

        member __.Gamma v1 v2 = job {            
            return gamma v1 v2
        }

        member __.Eta v1 v2 = job {
            return And v1 (gamma v1 v2)            
        }