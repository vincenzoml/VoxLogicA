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

exception NoModelLoadedException 
    with override __.Message = "No model loaded"

open SITKUtil
open itk.simple
open Hopac

module Lifts =
    let lift = Job.lift
    let lift2 = fun fn x y  -> job { return fn x y }

open Lifts

type SITKModel() =    
    inherit IModel()
    let mutable baseImg : option<Image> = None
    let getBaseImg() = match baseImg with None -> raise NoModelLoadedException | Some img -> img
        
    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.
        match (t,System.IO.Path.GetExtension f) with 
        | TValuation(_),(".nii"|".nii.gz"|".png"|".jpg") -> true // TODO: make this list exhaustive 
        | _ -> false


    override __.Save (logger : ErrorMsg.Logger) filename v =
        saveImage filename (v :?> Image) logger
          
            
    override __.Load (logger : ErrorMsg.Logger) s =
        let img = loadImage s logger
        match baseImg with
            | None -> baseImg <- Some img
            | Some _ -> logger.Warning "multiple loaded images should share the same voxel and physical space. Check not yet implemented."
        img :> obj

    interface IBoundedModel<Image> with
        member __.Border = job { return border (getBaseImg()) }
    
    interface IImageModel<Image> with
        member __.Intensity (img : Image) = lift intensity img
        
        member __.Red (img : Image) = job  {return (SimpleITK.VectorIndexSelectionCast(img,0ul)) }
        member __.Green (img : Image) = job { return SimpleITK.VectorIndexSelectionCast(img,1ul) }
        member __.Blue (img : Image) = job { return SimpleITK.VectorIndexSelectionCast(img,2ul) }

        member __.Volume img = lift volume img
        member __.MaxVol img = lift maxvol img
        member __.Percentiles img mask = lift2 percentiles img mask

    interface IBooleanModel<Image> with
        member __.And img1 img2 = lift2 logand img1 img2
        member __.Or img1 img2 = lift2 logor img1 img2
        member __.Not img = lift lognot img
        member __.TT = job { return tt (getBaseImg()) }
        member __.FF = job { return ff (getBaseImg()) }

    interface ISpatialModel<Image> with
        member __.Near img = lift near img
        member __.Interior img = lift interior img
        member __.Flood img1 img2 = lift2 flood img1 img2   
        member __.Reaches img1 img2 = lift2 reaches img1 img2           
   
    
    interface IDistanceModel<Image> with
        member __.DT img = lift dt img
        
    interface IQuantitativeModel<Image> with    
        member __.Eq value img = lift2 eq value img
            
        member __.Geq value img = lift2 geq value img
        member __.Leq value img = lift2 leq value img
        member __.Between value1 value2 img = job { return between value1 value2 img }
        member __.Max img = lift maxImg img
        member __.Min img = lift minImg img
        member __.Subtract img1 img2 = lift2 subtract img1 img2
        member __.Mask (img : Image) (maskImg : Image) = lift2 mask img maskImg
        member __.Avg (img : Image) (maskImg : Image)  = lift2 avg img maskImg
        member __.Sdiv (img : Image) k = job { return SimpleITK.Divide(img,k) }

    interface IStatisticalModel<Image> with 
        member __.CrossCorrelation rho a b fb m1 m2 k = crosscorrelation rho a b fb m1 m2 k

