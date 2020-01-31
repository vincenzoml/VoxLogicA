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
open Hopac

module Lifts =
    let lift = Job.lift
    let lift2 = fun fn x y  -> job { return fn x y }

open Lifts


type SITKModel() =    
    inherit IModel()
    let mutable baseImg : option<VoxImage> = None
    let getBaseImg() = match baseImg with None -> raise NoModelLoadedException | Some img -> img
        
    let supportedExtensions = [".nii";".nii.gz";".png";".jpg";"bmp"] // TODO: make this list exhaustive
    let itkM = itk.simple.Version.ITKMajorVersion().ToString() // TODO: move these to an auxiliary function in SITKUtil 
    let itkm = itk.simple.Version.ITKMinorVersion().ToString()
    let sitkM = itk.simple.Version.MajorVersion().ToString()
    let sitkm = itk.simple.Version.MinorVersion().ToString()
    let _ = ErrorMsg.Logger.Debug(sprintf "ITK Version: %s.%s" itkM itkm)
    let _ = ErrorMsg.Logger.Debug(sprintf "SimpleITK Version: %s.%s" sitkM sitkm)

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.        
        match t with 
        | (TValuation(_)|TModel) when List.exists (f.EndsWith : string -> bool) supportedExtensions -> true 
        | _ -> false        

    override __.Save filename v =
        (v :?> VoxImage).Save(filename)                  
            
    override __.Load s =
        let img = new VoxImage(s) 
        let res = 
            match baseImg with
            | None -> 
                baseImg <- Some img
                img 
            | Some img1 ->
                if VoxImage.SamePhysicalSpace img1 img
                then img
                else 
                    if img.NPixels = img1.NPixels
                        && img.Dimension = img1.Dimension
                    then 
                        if img.NComponents = img1.NComponents then 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\" has different physical space, but same logical structure than previously loaded images; physical space corrected." s)
                            img.ChangePhysicalSpace img1                             
                        else 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\"correcting physical space with different number of components is not currently supported; going to exit." s)                        
                            raise (DifferentPhysicalAndLogicalSpaceException s) 
                    else raise (DifferentPhysicalAndLogicalSpaceException s)
        ErrorMsg.Logger.DebugOnly (sprintf "loaded image: %A" <| res.GetHashCode())
        res :> obj     

    interface IBoundedModel<VoxImage> with
        member __.Border = job { return VoxImage.Border (getBaseImg()) }
    
    interface IImageModel<VoxImage> with
        member __.Intensity (img : VoxImage) = lift VoxImage.Intensity img
        
        member __.Red (img : VoxImage) = lift VoxImage.Red img
        member __.Green (img : VoxImage) = lift VoxImage.Green img
        member __.Blue (img : VoxImage) = lift VoxImage.Blue img
        member __.Alpha (img : VoxImage) = lift VoxImage.Alpha img
        member __.RGB (imgr : VoxImage) (imgg : VoxImage) (imgb : VoxImage) = job { return VoxImage.RGB imgr imgg imgb }
        member __.RGBA (imgr : VoxImage) (imgg : VoxImage) (imgb : VoxImage) (imga : VoxImage) = job { return VoxImage.RGBA imgr imgg imgb imga }
        member __.Volume img = lift VoxImage.Volume img
        member __.MaxVol img = lift VoxImage.MaxVol img
        member __.Percentiles img mask correction = job { return VoxImage.Percentiles img mask correction }

    interface IBooleanModel<VoxImage> with
        member __.TT = job { return VoxImage.TT (getBaseImg()) }
        member __.FF = job { return VoxImage.FF (getBaseImg()) }
        member __.BConst value = job { if value then return VoxImage.TT (getBaseImg()) else return VoxImage.FF (getBaseImg()) }
        member __.And img1 img2 = lift2 VoxImage.Logand img1 img2
        member __.Or img1 img2 = lift2 VoxImage.Logor img1 img2
        member __.Not img = lift VoxImage.Lognot img
 
    interface ISpatialModel<VoxImage> with
        member __.Near img = lift VoxImage.Near img
        member __.Interior img = lift VoxImage.Interior img
        member __.Through img1 img2 = lift2 VoxImage.Through img1 img2           
   
    interface IDistanceModel<VoxImage> with
        member __.DT img = lift VoxImage.Dt img
        
    interface IQuantitativeModel<VoxImage> with    
        member __.Const value = job { return VoxImage.CreateFloat (getBaseImg(),float32 value) }
        member __.EqSV value img = lift2 VoxImage.Eq value img
            
        member __.GeqSV value img = lift2 VoxImage.Geq value img
        member __.LeqSV value img = lift2 VoxImage.Leq value img
        member __.Between value1 value2 img = job { return VoxImage.Between value1 value2 img }
        member __.Max img = lift VoxImage.Max img
        member __.Min img = lift VoxImage.Min img
        member __.SubtractVV img1 img2 = job { return VoxImage.Subtract(img1,img2) }
        member __.AddVV img1 img2 = job {return VoxImage.Add(img1,img2) }
        member __.MultiplyVV img1 img2 = job { return VoxImage.Mult(img1,img2) }
        member __.Mask (img : VoxImage) (maskImg : VoxImage) = job { return VoxImage.Mask img maskImg 0.0 }
        member __.Avg (img : VoxImage) (maskImg : VoxImage)  = lift2 VoxImage.Avg img maskImg
        member __.AddVS (img : VoxImage) k = job { return VoxImage.Add(img,k) }
        member __.MulVS (img : VoxImage) k = job { return VoxImage.Mult(img,k) }
        member __.SubVS (img : VoxImage) k = job { return VoxImage.Subtract(img,k) }
        member __.DivVS (img : VoxImage) k = job { return VoxImage.Mult(img,1.0/k) }
        member __.SubSV k (img : VoxImage) = job { return VoxImage.Subtract(k,img) }
        member __.DivSV k (img : VoxImage) = job { return VoxImage.Div(k,img) }
        
    interface IStatisticalModel<VoxImage> with 
        member __.CrossCorrelation rho a b fb m1 m2 k = VoxImage.Crosscorrelation rho a b fb m1 m2 k

