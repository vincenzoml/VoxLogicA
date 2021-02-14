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

open TriaGraph

open Truth

type SITKModel() =    
    inherit IModel()
    let mutable baseTriaGraph : option<TriaGraph> = None
    let getBaseTriaGraph() = match baseTriaGraph with None -> raise NoModelLoadedException | Some triaGraph -> triaGraph
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
        // printfn "save to '%A': %A" atomName t
        // saveTriaGraph (getBaseTriaGraph ()) filename "result" t
        atoms.Add(atomName,t)
            
    override __.Load s =
        let triaGraph = loadTriaGraph s
        let res = 
            match baseTriaGraph with
            | None -> 
                baseTriaGraph <- Some triaGraph
                triaGraph
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
                let triaGraph = getBaseTriaGraph()
                let ap = getTriaGraphAp triaGraph s
                let res = Array.create triaGraph.NumSimplexes false 
                Set.iter (fun idx -> res.[idx] <- true) ap
                return res
            }

    // interface IBoundedModel<VoxImage> with
    //     member __.Border = job { return VoxImage.Border (getBaseImg()) }
    
    // interface IImageModel<VoxImage> with
    //     member __.Intensity (img : VoxImage) = lift VoxImage.Intensity img
        
    //     member __.Red (img : VoxImage) = lift VoxImage.Red img
    //     member __.Green (img : VoxImage) = lift VoxImage.Green img
    //     member __.Blue (img : VoxImage) = lift VoxImage.Blue img
    //     member __.Alpha (img : VoxImage) = lift VoxImage.Alpha img
    //     member __.RGB (imgr : VoxImage) (imgg : VoxImage) (imgb : VoxImage) = job { return VoxImage.RGB imgr imgg imgb }
    //     member __.RGBA (imgr : VoxImage) (imgg : VoxImage) (imgb : VoxImage) (imga : VoxImage) = job { return VoxImage.RGBA imgr imgg imgb imga }
    //     member __.Volume img = lift VoxImage.Volume img
    //     member __.MaxVol img = lift VoxImage.MaxVol img
    //     member __.Percentiles img mask correction = job { return VoxImage.Percentiles img mask correction }
    //     member __.LCC img = job { return VoxImage.Lcc img }

    interface IBooleanModel<Truth> with
        member __.TT = job { return TT(getBaseTriaGraph().NumSimplexes) }
        member __.FF = job { return FF(getBaseTriaGraph().NumSimplexes) }
        member __.BConst v = lift (BConst (getBaseTriaGraph().NumSimplexes)) v  
        member __.And v1 v2 = lift2 And v1 v2
        member __.Or v1 v2 = lift2 Or v1 v2
        member __.Not v = lift Not v
 
    interface ISpatialModel<Truth> with
        member __.Near v = job { return (upClosure (getBaseTriaGraph()) v) }
        member __.Interior v = job { return (interior (getBaseTriaGraph()) v) }
        member __.Through v1 v2 = job {return (reach (getBaseTriaGraph()) v1 v2)}           
   
    // interface IDirectedSpatialModel<Truth> with
    //     member __.BNear v = job { return (bdilate (getBaseGraph()) v) }
    //     member __.BInterior v = job { return (berode (getBaseGraph()) v) }
    //     member __.BThrough v1 v2 = job {return (btrough (getBaseGraph()) v1 v2)}           

    // interface IDistanceModel<VoxImage> with
    //     member __.DT img = lift VoxImage.Dt img
        
    // interface IQuantitativeModel<VoxImage> with    
    //     member __.Const value = job { return VoxImage.CreateFloat (getBaseImg(),float32 value) }
    //     member __.EqSV value img = lift2 VoxImage.Eq value img
            
    //     member __.GeqSV value img = lift2 VoxImage.Geq value img
    //     member __.LeqSV value img = lift2 VoxImage.Leq value img
    //     member __.Between value1 value2 img = job { return VoxImage.Between value1 value2 img }        
    //     member __.Abs img = job { return VoxImage.Abs img }
    //     member __.Max img = lift VoxImage.Max img
    //     member __.Min img = lift VoxImage.Min img
    //     member __.SubtractVV img1 img2 = job { return VoxImage.Subtract(img1,img2) }
    //     member __.AddVV img1 img2 = job {return VoxImage.Add(img1,img2) }
    //     member __.MultiplyVV img1 img2 = job { return VoxImage.Mult(img1,img2) }
    //     member __.Mask (img : VoxImage) (maskImg : VoxImage) = job { return VoxImage.Mask img maskImg 0.0 }
    //     member __.Avg (img : VoxImage) (maskImg : VoxImage)  = lift2 VoxImage.Avg img maskImg
    //     member __.AddVS (img : VoxImage) k = job { return VoxImage.Add(img,k) }
    //     member __.MulVS (img : VoxImage) k = job { return VoxImage.Mult(img,k) }
    //     member __.SubVS (img : VoxImage) k = job { return VoxImage.Subtract(img,k) }
    //     member __.DivVS (img : VoxImage) k = job { return VoxImage.Mult(img,1.0/k) }
    //     member __.SubSV k (img : VoxImage) = job { return VoxImage.Subtract(k,img) }
    //     member __.DivSV k (img : VoxImage) = job { return VoxImage.Div(k,img) }
        
    // interface IStatisticalModel<VoxImage> with 
    //     member __.CrossCorrelation rho a b fb m1 m2 k = VoxImage.Crosscorrelation rho a b fb m1 m2 k

