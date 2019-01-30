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
        
    let supported_extensions = [".nii";".nii.gz";".png";".jpg";"bmp"] // TODO: make this list exhaustive

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.        
        match t with 
        | (TValuation(_)|TModel) when List.exists (f.EndsWith : string -> bool) supported_extensions -> true 
        | _ -> false        
    override __.Save filename v =
        saveImage filename (v :?> Image)                      
    override __.Load s =
        let img = loadImage s 
        match baseImg with
            | None -> 
                baseImg <- Some img
                img :> obj
            | Some img1 ->
                try
                    use x = SimpleITK.Add(img1,img) 
                    img :> obj
                with _ -> // if And fails, the two images don't have the same physical structure
                    if img.GetNumberOfPixels() = img1.GetNumberOfPixels() 
                        //&& img.GetNumberOfComponentsPerPixel() = img1.GetNumberOfComponentsPerPixel() 
                        //&& img.GetPixelID() = img1.GetPixelID()
                        && img.GetDimension() = img1.GetDimension()
                    then 
                        if img.GetNumberOfComponentsPerPixel() = img1.GetNumberOfComponentsPerPixel() then 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\" has different physical space, but same logical structure than previously loaded images; physical space corrected." s)
                            changePhysicalSpace(img,img1) :> obj 
                        else 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\"correcting physical space with different number of components is not currently supported; going to exit." s)                        
                            raise (DifferentPhysicalAndLogicalSpaceException s) //TODO: fix this, converting when possible.
                    else raise (DifferentPhysicalAndLogicalSpaceException s)

    interface IBoundedModel<Image> with
        member __.Border = job { return border (getBaseImg()) }
    
    interface IImageModel<Image> with
        member __.Intensity (img : Image) = lift intensity img
        
        member __.Red (img : Image) = lift red img
        member __.Green (img : Image) = lift green img
        member __.Blue (img : Image) = lift blue img
        member __.Alpha (img : Image) = lift alpha img
        member __.RGB (imgr : Image) (imgg : Image) (imgb : Image) = job { return rgb imgr imgg imgb }
        member __.RGBA (imgr : Image) (imgg : Image) (imgb : Image) (imga : Image) = job { return rgba imgr imgg imgb imga }
        member __.Volume img = lift volume img
        member __.MaxVol img = lift maxvol img
        member __.Percentiles img mask correction = job { return percentiles img mask correction }

    interface IBooleanModel<Image> with
        member __.And img1 img2 = lift2 logand img1 img2
        member __.Or img1 img2 = lift2 logor img1 img2
        member __.Not img = lift lognot img
        member __.TT = job { return tt (getBaseImg()) }
        member __.FF = job { return ff (getBaseImg()) }

    interface ISpatialModel<Image> with
        member __.Near img = lift near img
        member __.Interior img = lift interior img
        member __.Through img1 img2 = lift2 through img1 img2           
   
    
    interface IDistanceModel<Image> with
        member __.DT img = lift dt img
        
    interface IQuantitativeModel<Image> with    
        member __.Const value = job { return mkConst (float32 value) (getBaseImg()) }
        member __.EqSV value img = lift2 eq value img
            
        member __.GeqSV value img = lift2 geq value img
        member __.LeqSV value img = lift2 leq value img
        member __.Between value1 value2 img = job { return between value1 value2 img }
        member __.Max img = lift maxImg img
        member __.Min img = lift minImg img
        member __.SubtractVV img1 img2 = lift2 subtract img1 img2
        member __.AddVV img1 img2 = lift2 add img1 img2
        member __.MultiplyVV img1 img2 = lift2 mult img1 img2
        member __.Mask (img : Image) (maskImg : Image) = lift2 mask img maskImg
        member __.Avg (img : Image) (maskImg : Image)  = lift2 avg img maskImg
        member __.AddVS (img : Image) k = job { return SimpleITK.Add(img,k) }
        member __.MulVS (img : Image) k = job { return SimpleITK.Multiply(img,k) }
        member __.SubVS (img : Image) k = job { return SimpleITK.Subtract(img,k) }
        member __.DivVS (img : Image) k = job { return SimpleITK.Divide(img,k) }
        member __.SubSV k (img : Image) = job { return SimpleITK.Subtract(k,img) }
        member __.DivSV k (img : Image) = job { return SimpleITK.Divide(k,img) }
        
    interface IStatisticalModel<Image> with 
        member __.CrossCorrelation rho a b fb m1 m2 k = crosscorrelation rho a b fb m1 m2 k

