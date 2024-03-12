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

type PosetModel() =
    inherit IModel()
    let mutable basePoset: option<Poset> = None

    let getBasePoset () =
        match basePoset with
        | None -> raise NoModelLoadedException
        | Some poset -> poset

    let atoms = Dictionary<_, _>()

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.
        true

    override __.Save atomName v =
        let t = v :?> Truth
        atoms.Add(atomName, t)

    override __.Load s =
        let poset = loadPoset s

        let res =
            match basePoset with
            | None ->
                basePoset <- Some poset

                List.iteri
                    (fun idx (point:Point) ->
                        assert ((int point.id) = idx)
                        List.iter
                            (fun atom ->
                                printfn "%A" atom
                                if not (atoms.ContainsKey atom) then
                                    atoms[atom] <- FF poset.points.Length
                                let v = atoms[atom]
                                v[idx] <- true)
                            point.atoms)
                    poset.points

                poset
            | Some _ -> raise MoreThanOneModelUnsupportedException

        res :> obj

    override __.OnExit() =
        let atomsToPrint = (atoms :> seq<_>) |> Seq.map (|KeyValue|) |> Map.ofSeq
        System.IO.File.WriteAllText("result.json", Json.serialize atomsToPrint)

    interface IAtomicModel<Truth> with
        member __.Ap s =
            job {
                // let poset = getBasePoset() TODO: this is redundant because we can only load one model (see the code of "__.Load") but when one can load more posets, the table "atoms" is part of *each* model
                return atoms[s]
            }

    interface IBooleanModel<Truth> with
        member __.TT = job { return TT(getBasePoset().points.Length) }
        member __.FF = job { return FF(getBasePoset().points.Length) }
        member __.BConst v = lift (BConst (getBasePoset().points.Length)) v
        member __.And v1 v2 = lift2 And v1 v2
        member __.Or v1 v2 = lift2 Or v1 v2
        member __.Not v = lift Not v

// interface ISpatialModel<Truth> with
//     member __.Near v = job { return (downClosure (getBasePoset()) v) }
//     member __.Interior v = job { return (interior (getBasePoset()) v) }
//     member __.Through v1 v2 = job {return (reach (getBasePoset()) v1 v2)}
