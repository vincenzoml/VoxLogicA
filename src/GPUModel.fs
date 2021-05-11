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

exception NoModelLoadedException with
    override __.Message = "No model loaded"

open Hopac
open VoxLogicA.GPU
open SITKUtil

type GPUModelValue =
    { gVal: GPUValue<VoxImage>
      gEvt: array<Event> }

type GPUModel() =
    inherit IModel()

    let kernelFile =
        System.IO.Path.Combine [| System.IO.Path.GetDirectoryName(
                                      System
                                          .Reflection
                                          .Assembly
                                          .GetExecutingAssembly()
                                          .Location
                                  )
                                  "kernel.cl" |]

    let gpu = GPU(kernelFile)

    let mutable baseImg : option<VoxImage> = None

    let getBaseImg () =
        match baseImg with
        | None -> raise NoModelLoadedException
        | Some img -> img

    let supportedExtensions =
        [ ".nii"
          ".nii.gz"
          ".png"
          ".jpg"
          "bmp" ] // TODO: make this list exhaustive

    let itkM =
        itk.simple.Version.ITKMajorVersion().ToString() // TODO: move these to an auxiliary function in SITKUtil

    let itkm =
        itk.simple.Version.ITKMinorVersion().ToString()

    let sitkM =
        itk.simple.Version.MajorVersion().ToString()

    let sitkm =
        itk.simple.Version.MinorVersion().ToString()

    let _ =
        ErrorMsg.Logger.Debug(sprintf "ITK Version: %s.%s" itkM itkm)

    let _ =
        ErrorMsg.Logger.Debug(sprintf "SimpleITK Version: %s.%s" sitkM sitkm)

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.
        match t with
        | (TValuation (_)
        | TModel) when List.exists (f.EndsWith: string -> bool) supportedExtensions -> true
        | _ -> false

    override __.Save filename v =
        let gmv = (v :?> GPUModelValue)
        gpu.Wait <| gmv.gEvt
        let img = gmv.gVal.Get()
        ErrorMsg.Logger.DebugOnly(sprintf "saving image: %A" <| img.GetHashCode())
        img.Save(filename)
        JSonOutput.Info(min = VoxImage.Min(VoxImage.Intensity img), max = VoxImage.Max(VoxImage.Intensity img))

    override __.Load s =
        let img = new VoxImage(s)

        let res =
            match baseImg with
            | None ->
                baseImg <- Some img
                gpu.SetDimensionIndex <| img.Dimension
                img
            | Some img1 ->
                if VoxImage.SamePhysicalSpace img1 img then
                    img
                else if img.NPixels = img1.NPixels
                        && img.Dimension = img1.Dimension then
                    if img.NComponents = img1.NComponents then
                        ErrorMsg.Logger.Warning(
                            sprintf
                                "Image \"%s\" has different physical space, but same logical structure than previously loaded images; physical space corrected."
                                s
                        )

                        img.ChangePhysicalSpace img1
                    else
                        ErrorMsg.Logger.Warning(
                            sprintf
                                "Image \"%s\"correcting physical space with different number of components is not currently supported; going to exit."
                                s
                        )

                        raise (DifferentPhysicalAndLogicalSpaceException s)
                else
                    raise (DifferentPhysicalAndLogicalSpaceException s)

        ErrorMsg.Logger.DebugOnly(sprintf "loaded image: %A" <| res.GetHashCode())

        { gVal = (gpu.CopyImageToDevice res)
          gEvt = [||] }
        :> obj

    interface IBoundedModel<GPUModelValue> with
        member __.Border =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run("test", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

    interface IImageModel<GPUModelValue> with
        member __.Intensity(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run(
                        "intensity",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Red(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run(
                        "getComponent",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(1f) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Green(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run(
                        "getComponent",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(2f) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Blue(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run(
                        "getComponent",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(3f) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Alpha(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run(
                        "getComponent",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(4f) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.RGB (imgr: GPUModelValue) (imgg: GPUModelValue) (imgb: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,4,Float32)
                //which event??
                let event =
                    gpu.Run(
                        "rgbComps",
                        [||],
                        seq {
                            imgr.gVal
                            imgg.gVal
                            imgb.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.RGBA (imgr: GPUModelValue) (imgg: GPUModelValue) (imgb: GPUModelValue) (imga: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,4,Float32)

                let event =
                    gpu.Run(
                        "rgbaComps",
                        [||],
                        seq {
                            imgr.gVal
                            imgg.gVal
                            imgb.gVal
                            imga.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Volume img = job { return 0.5 }

        member __.MaxVol img =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run("test", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Percentiles img mask correction =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32)

                let event =
                    gpu.Run("test", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

        member __.LCC img =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,Float32) // TODO: this could be UInt32

                let event =
                    gpu.Run("test", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

    interface IBooleanModel<GPUModelValue> with
        member __.TT = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run(
                        "booleanImg", 
                        [||], 
                        seq { 
                            output :> KernelArg
                            gpu.Float32(1f) :> KernelArg
                        }, 
                        img.Size, 
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }
        member __.FF = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run(
                        "booleanImg", 
                        [||], 
                        seq { 
                            output :> KernelArg
                            gpu.Float32(0f) :> KernelArg
                        }, 
                        img.Size, 
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }
        member __.BConst value = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)
                let v = if value then 1f else 0f

                let event =
                    gpu.Run(
                        "booleanImg", 
                        [||], 
                        seq { 
                            output :> KernelArg
                            gpu.Float32(v) :> KernelArg
                        }, 
                        img.Size, 
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }
        member __.And img1 img2 = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run("trueImg", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }
        member __.Or img1 img2 = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run("trueImg", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }
        member __.Not img = job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img,1,UInt8)

                let event =
                    gpu.Run("trueImg", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

// interface ISpatialModel<VoxImage> with
//     member __.Near img = lift VoxImage.Near img
//     member __.Interior img = lift VoxImage.Interior img
//     member __.Through img1 img2 = lift2 VoxImage.Through img1 img2

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
//     member __.DivideVV img1 img2 = job { return VoxImage.Div(img1,img2) }
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

// // IMAGING
// [<OperatorAttribute("otsu",[|"valuation(number)";"valuation(bool)";"number"|],"valuation(bool)","otsu threshold (image, mask,number of bins)")>]
// member __.Otsu (img : VoxImage, mask : VoxImage, nbins : float) = job { return VoxImage.Otsu(img,mask,nbins) }
