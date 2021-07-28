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
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run("border", [||], seq { output }, img.Size, None)

                return { gVal = output; gEvt = [| event |] }
            }

    interface IImageModel<GPUModelValue> with
        member __.Intensity(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                if img.NComponents > 1 then
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
                else
                    return imgIn
            }

        member __.Red(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

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
                let output = gpu.NewImageOnDevice(img, 1, Float32)

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
                let output = gpu.NewImageOnDevice(img, 1, Float32)

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
                let output = gpu.NewImageOnDevice(img, 1, Float32)

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
                let output = gpu.NewImageOnDevice(img, 4, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append (Array.append imgr.gEvt imgg.gEvt) imgb.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "rgbComps",
                        newEvents,
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
                let output = gpu.NewImageOnDevice(img, 4, Float32)
                let evt1 = Array.append imgr.gEvt imgg.gEvt
                let evt2 = Array.append evt1 imgb.gEvt

                let tmpEvents =
                    Seq.distinct (Array.append evt2 imga.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "rgbaComps",
                        newEvents,
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

        member __.Volume img =
            job {
                gpu.Wait img.gEvt

                let cpuImg = img.gVal.Get()

                let result = VoxImage.Volume cpuImg

                return result
            }

        //member __.MaxVol img =
        //    job {
        //        let img = getBaseImg ()
        //        let output = gpu.NewImageOnDevice(img,1,UInt8)

        //        let event =
        //            gpu.Run("test", [||], seq { output }, img.Size, None)

        //        return { gVal = output; gEvt = [| event |] }
        //    }

        member __.Percentiles imgIn mask correction = // IN CPU
            job {
                let evt = Array.append imgIn.gEvt mask.gEvt
                gpu.Wait evt

                let cpuImg = imgIn.gVal.Get()
                let cpuMask = mask.gVal.Get()

                let result =
                    VoxImage.Percentiles cpuImg cpuMask correction

                let output = gpu.CopyImageToDevice result

                return { gVal = output; gEvt = [||] }
            }

        member __.LCC img =
            job {
                let bimg = getBaseImg ()
                
                let mutable flag = gpu.CopyArrayToDevice([|1uy|]) : GPUValue<array<uint8>>
                let mutable output = gpu.NewImageOnDevice(bimg, 4, Float32)
                let mutable tmp = gpu.NewImageOnDevice(bimg, 4, Float32)
                // let comp = gpu.Float32(0f)
                //let mutable event = [||]

                let swap () =
                    let temp = tmp
                    tmp <- output
                    output <- temp
                
                let evt = gpu.Run("initCCL", img.gEvt, seq { img.gVal :> KernelArg; tmp :> KernelArg }, bimg.Size, None)               

                gpu.Wait([|evt|])
                let prova = tmp.Get()
                printfn "%A" <| prova.BufferType
                prova.Save("output/init.png")

                let mutable retval = evt   
                for _ = 1 to 2 do                 
                    let step evt = 
                        gpu.Run(
                                "iterateCCL",
                                [| evt |],
                                seq {
                                    tmp :> KernelArg
                                    output :> KernelArg
                                    flag :> KernelArg
                                },
                                bimg.Size,
                                None
                            )

                    let rec iterate n evt =
                        if n <= 0 then evt
                        else
                            let evt' = step evt
                            swap() 
                            iterate (n-1) evt'

                    retval <- iterate 32 evt

                    gpu.Wait([|evt|])
                    let prova = tmp.Get()
                    prova.Save("output/provainit.png")
                    //failwith "Exit here" 

                    let evt =
                        gpu.Run(
                               "reconnectCCL",
                               [|evt|],
                               seq {
                                   tmp :> KernelArg
                                   output :> KernelArg
                                   flag :> KernelArg
                               },
                               bimg.Size,
                               None
                        )
                    retval <- evt
                    swap()

                gpu.Wait([|retval|])
                let prova = output.Get()
                prova.Save("output/provarec.png")
                    //failwith "Exit here" 
                //gpu.Wait([|evt|])
                //let newFlag = gpu.Float32(flag.Get())
                //flag <- newFlag
                //printfn "flag is %A" flag.Value
                //printfn "comp is %A" comp.Value

                return { gVal = output; gEvt = [|retval|] }
            }


    interface IBooleanModel<GPUModelValue> with
        member __.TT =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

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

        member __.FF =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

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

        member __.BConst value =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)
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

        member __.And img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "logand",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Or img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "logor",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Not imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "lognot",
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

    interface ISpatialModel<GPUModelValue> with
        member __.Near imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "dilate",
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

        member __.Through img img2 = job { return (failwith "through: stub" : GPUModelValue) }

        member __.Interior imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "erode",
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

    interface IDistanceModel<GPUModelValue> with
        member __.DT img =
            job {
                gpu.Wait img.gEvt

                let cpuImg = img.gVal.Get()

                let result = VoxImage.Dt cpuImg

                let output = gpu.CopyImageToDevice result

                return { gVal = output; gEvt = [||] }
            }

    interface IQuantitativeModel<GPUModelValue> with
        member __.Const value =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "constImg",
                        [||],
                        seq {
                            output :> KernelArg
                            gpu.Float32(float32 value) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.EqSV value imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "eq",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 value) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.GeqSV value imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "geq",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 value) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.LeqSV value imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "leq",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 value) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Between value1 value2 imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, UInt8)

                let event =
                    gpu.Run(
                        "between",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 value1) :> KernelArg
                            gpu.Float32(float32 value2) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Abs imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "abs",
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
        //     member __.Max img = lift VoxImage.Max img

        // member __.Min img =

        //     job {
        //         gpu.Wait img.gEvt

        //         let cpuImg = img.gVal.Get()

        //         let result = VoxImage.Min cpuImg

        //         let output = gpu.CopyImageToDevice result

        //         return { gVal = output; gEvt = [||] }
        //     }



        member __.SubtractVV img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "sub",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.AddVV img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "add",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.MultiplyVV img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "mul",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.DivideVV img1 img2 =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.gEvt img2.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "div",
                        newEvents,
                        seq {
                            img1.gVal
                            img2.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.Mask imgIn maskImg =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append imgIn.gEvt maskImg.gEvt)

                let newEvents = Seq.toArray tmpEvents

                let event =
                    gpu.Run(
                        "mask",
                        newEvents,
                        seq {
                            imgIn.gVal
                            maskImg.gVal
                            output
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }
        //     member __.Avg (img : VoxImage) (maskImg : VoxImage)  = lift2 VoxImage.Avg img maskImg
        member __.AddVS imgIn k =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "addVS",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.MulVS imgIn k =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "mulVS",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.SubVS imgIn k =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "subVS",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.DivVS imgIn k =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "divVS",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.SubSV k imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "subSV",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

        member __.DivSV k imgIn =
            job {
                let img = getBaseImg ()
                let output = gpu.NewImageOnDevice(img, 1, Float32)

                let event =
                    gpu.Run(
                        "divSV",
                        imgIn.gEvt,
                        seq {
                            imgIn.gVal :> KernelArg
                            output :> KernelArg
                            gpu.Float32(float32 k) :> KernelArg
                        },
                        img.Size,
                        None
                    )

                return { gVal = output; gEvt = [| event |] }
            }

// interface IStatisticalModel<VoxImage> with
//     member __.CrossCorrelation rho a b fb m1 m2 k = VoxImage.Crosscorrelation rho a b fb m1 m2 k

// // IMAGING
// [<OperatorAttribute("otsu",[|"valuation(number)";"valuation(bool)";"number"|],"valuation(bool)","otsu threshold (image, mask,number of bins)")>]
// member __.Otsu (img : VoxImage, mask : VoxImage, nbins : float) = job { return VoxImage.Otsu(img,mask,nbins) }
