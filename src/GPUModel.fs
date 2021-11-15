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
open System
open VoxLogicA.GPU
open SITKUtil

type GPUModelValue(gVal: GPUValue<VoxImage>, gEvt: array<Event>, gpu : GPU) =
    interface IDisposableJob with
        member __.Dispose = (gVal :> IDisposableJob).Dispose
    
    interface IWait with
        member __.Wait = Job.result <| gpu.Wait(gEvt)

    member __.GVal = gVal
    member __.GEvt = gEvt

type GPUModel(performanceTest) =
    inherit IModel()

    let kernelFile =
        System.IO.Path.Combine [| AppContext.BaseDirectory
                                  "kernel.cl" |]

    let mutable gpuval = None
    let mutable dim = 0

    let mutable baseImg: option<VoxImage> = None

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

    let gpu () =
        match gpuval with
        | None -> raise NoModelLoadedException
        | Some (x: GPU) -> x

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.
        match t with
        | (TValuation (_)
        | TModel) when List.exists (f.EndsWith: string -> bool) supportedExtensions -> true
        | _ -> false

    override __.Save filename v =
        ErrorMsg.Logger.DebugOnly(sprintf "Retrieving from GPU: %A" filename)
        let gmv = (v :?> GPUModelValue)
        gpu().Wait <| gmv.GEvt
        let img = gmv.GVal.Get()
        ErrorMsg.Logger.DebugOnly(sprintf "saving image: %A" <| img.GetHashCode())
        if performanceTest then
            JSonOutput.Info(min = 0.0, max = 0.0)            
        else
            img.Save(filename)
            JSonOutput.Info(min = VoxImage.Min(VoxImage.Intensity img), max = VoxImage.Max(VoxImage.Intensity img))            

    override __.Load s = 
        job {
            let res = // TODO: URGENT: does this require locking?
                match (baseImg,performanceTest) with
                | None,_ ->
                    let img = new VoxImage(s)                    
                    dim <- img.Dimension
                    baseImg <- Some img
                    gpuval <-
                        match gpuval with
                        | None -> Some(GPU(kernelFile, dim))
                        | Some (_) as x -> x
                    if performanceTest then ErrorMsg.Logger.Debug "Start measuring performance from here"
                    img
                | Some img1,true ->
                    img1
                | Some img1,false ->
                    let img = new VoxImage(s)
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

            let! img = (gpu().CopyImageToDevice res)         

            return GPUModelValue(img, [| |],gpu()) :> obj
        }

    interface IBoundedModel<GPUModelValue> with
        member __.Border =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let kernelName = if dim = 2 then "border" else "border3D"

                let! event =
                    gpu()
                        .Run(kernelName, [||], seq { output }, img.Size, None)

                return GPUModelValue(output, [| event |],gpu())
            }

    interface IImageModel<GPUModelValue> with
        member __.Intensity(imgIn: GPUModelValue) =
            // ErrorMsg.Logger.Warning
            //     "NOTE: Intensity must be changed in order to accommodate the special case in which it is the identity"            

            job {
                let img = getBaseImg ()
                
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let! event =
                    gpu()
                        .Run(
                            "copy",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Red(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(1f)

                let! event =
                    gpu()
                        .Run(
                            "getComponent",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Green(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(1f)

                let! event =
                    gpu()
                        .Run(
                            "getComponent",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Blue(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(3f)

                let! event =
                    gpu()
                        .Run(
                            "getComponent",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Alpha(imgIn: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(4f)

                let! event =
                    gpu()
                        .Run(
                            "getComponent",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.RGBA (imgr: GPUModelValue) (imgg: GPUModelValue) (imgb: GPUModelValue) (imga: GPUModelValue) =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 4, Float32)
                let evt1 = Array.append imgr.GEvt imgg.GEvt
                let evt2 = Array.append evt1 imgb.GEvt

                let tmpEvents =
                    Seq.distinct (Array.append evt2 imga.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "rgbaComps",
                            newEvents,
                            seq {
                                imgr.GVal
                                imgg.GVal
                                imgb.GVal
                                imga.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Volume(img: GPUModelValue) =
            job {
                let img1 = getBaseImg ()
                let! output' = gpu().NewImageOnDevice(img1, 1, Float32)
                let mutable output = output'
                let! tmp' = gpu().NewImageOnDevice(img1, 1, Float32)
                let mutable tmp = tmp'

                let kernelVolume =
                    if dim = 3 then
                        "volume3D"
                    else
                        "volume2D"

                let kernelRead =
                    if dim = 3 then
                        "readFirstPixel3D"
                    else
                        "readFirstPixel2D"

                let! evt' =
                    gpu()
                        .Run(
                            "castUInt8ToFloat32",
                            img.GEvt,
                            seq {
                                img.GVal :> KernelArg
                                tmp :> KernelArg
                            },
                            img1.Size,
                            None
                        )

                let mutable newEvent = [| evt' |]

                let iterations =
                    int (ceil (Math.Log2(float img1.Size.[0])))

                let swap () =
                    let temp = tmp
                    tmp <- output
                    output <- temp

                let mutable currentSize = img1.Size

                // ARRAY DELLE DIMENSIONI, COPIATO COME VARIABILE
                for i = 0 to iterations - 1 do // WHILE UNA DELLE DIMENSIONI E' MAGGIORE DI 1
                    let! event =
                        gpu()
                            .Run(
                                kernelVolume,
                                newEvent,
                                seq {
                                    tmp
                                    output
                                },
                                currentSize, // VETTORE DELLE DIMENSIONI
                                None
                            )

                    // gpu().Wait([|event|])
                    // let x = (VoxImage.Mult (output.Get(),float 256))
                    // x.Save(sprintf "output/iteration-%02d.png" i)

                    // AGGIUNGERE UNO ALLE DIMENSIONI DISPARI E DIMEZZARLE
                    newEvent <- [| event |]
                    swap ()
                    for i in 0..currentSize.Length - 1 do
                        if currentSize.[i] % 2 = 1 then
                            currentSize.[i] <- currentSize.[i] + 1
                            currentSize.[i] <- currentSize.[i] / 2

                let mutable res: GPUValue<array<float32>> = gpu().CopyArrayToDevice([| 0f |])
                // gpu().Wait(newEvent)
                // let x = (VoxImage.Mult (tmp.Get(),float 256))
                // x.Save("output/tmp.png")

                let! ev =
                    (gpu()
                        .Run(
                            kernelRead,
                            newEvent,
                            seq {
                                tmp
                                res
                            },
                            [| 1 |],
                            None
                        ))

                gpu().Wait([| ev |])
                let result = res.Get()
                return float result.[0]
            }

        // ON CPU
        member __.MaxVol img =
            job {

                gpu().Wait img.GEvt

                let cpuImg = img.GVal.Get()

                let result = VoxImage.MaxVol cpuImg

                let! output = gpu().CopyImageToDevice result

                return GPUModelValue(output, [||],gpu())
            }

        // ON CPU
        member __.Percentiles imgIn mask correction =
            job {
                let evt = Array.append imgIn.GEvt mask.GEvt
                gpu().Wait evt

                let cpuImg = imgIn.GVal.Get()
                let cpuMask = mask.GVal.Get()

                let result =
                    VoxImage.Percentiles cpuImg cpuMask correction

                let! output = gpu().CopyImageToDevice result

                return GPUModelValue(output, [||],gpu())
            }

        member __.LCC img = // TODO: URGENT: see the changes to this in through ; use this instead of the copy in through
            job {
                let bimg = getBaseImg ()

                let mutable flag: GPUValue<array<uint8>> = gpu().CopyArrayToDevice([| 0uy |])
                let! output' = gpu().NewImageOnDevice(bimg, 4, Float32)
                let mutable output = output'
                let! tmp' = gpu().NewImageOnDevice(bimg, 4, Float32)
                let mutable tmp = tmp'

                let kernelInit =
                    if dim = 2 then
                        "initCCL"
                    else
                        "initCCL3D"

                let kernelIterate =
                    if dim = 2 then
                        "iterateCCL"
                    else
                        "iterateCCL3D"

                let kernelReconnect =
                    if dim = 2 then
                        "reconnectCCL"
                    else
                        "reconnectCCL3D"

                let swap () =
                    let temp = tmp
                    tmp <- output
                    output <- temp

                let! evt0 =
                    gpu()
                        .Run(
                            kernelInit,
                            img.GEvt,
                            seq {
                                img.GVal :> KernelArg
                                tmp :> KernelArg
                            },
                            bimg.Size,
                            None
                        )

                //gpu().Wait([|evt0|])
                //tmp.Get().Save("output/init.nii.gz")

                let rec iterate n iterations evt =
                    job {
                        if n >= iterations then
                            return evt
                        else
                            let! evt' =
                                gpu()
                                    .Run(
                                        kernelIterate,
                                        [| evt |],
                                        seq {
                                            tmp :> KernelArg
                                            output :> KernelArg
                                        },
                                        bimg.Size,
                                        None
                                    )

                            //gpu().Wait([|evt'|])
                            //output.Get().Save(sprintf "output/iteration-%02d.nii.gz" n)

                            swap ()
                            return! iterate (n + 1) iterations evt'
                    }

                let mutable terminated = false

                let mutable whileEvt = evt0

                let mutable nsteps = 0
                let mutable nrecs = 0

                while not terminated do

                    let k = 8

                    let! evt1 = iterate 0 k whileEvt

                    let! evt2 =
                        gpu()
                            .Run(
                                kernelReconnect,
                                [| evt1 |],
                                seq {
                                    tmp :> KernelArg
                                    output :> KernelArg
                                    flag :> KernelArg
                                },
                                bimg.Size,
                                None
                            )

                    nsteps <- nsteps + k
                    nrecs <- nrecs + 1

                    gpu().Wait([| evt2 |]) // DO NOT REMOVE THIS

                    if flag.Get().[0] > 0uy then
                        swap ()

                        let! whileEvtTmp =
                            gpu()
                                .Run("resetFlag", [||], seq { flag }, [| 1 |], None)

                        whileEvt <- whileEvtTmp
                    else
                        // ErrorMsg.Logger.Debug(
                        //     sprintf "LCC terminated after %d steps (%d reconnects)" (nsteps + nrecs) nrecs
                        // )
                        terminated <- true

                // ONLY TO DEBUG A SINGE ITERATION WITH RECONNECT SET THIS AND COMMENT THE IF ABOVE: terminated <- true

                return GPUModelValue(output, [||],gpu()) // No event returned as we waited for the event already to read the flag
            }


    interface IBooleanModel<GPUModelValue> with
        member __.TT =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f = gpu().Float32(1f)

                let! event =
                    gpu()
                        .Run(
                            "booleanImg",
                            [||],
                            seq {
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.FF =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f = gpu().Float32(0f)

                let! event =
                    gpu()
                        .Run(
                            "booleanImg",
                            [||],
                            seq {
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.BConst value =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let v = if value then 1f else 0f
                let f = gpu().Float32(v)

                let! event =
                    gpu()
                        .Run(
                            "booleanImg",
                            [||],
                            seq {
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.And img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "logand",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Or img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "logor",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Not imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)

                let! event =
                    gpu()
                        .Run(
                            "lognot",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

    interface ISpatialModel<GPUModelValue> with
        member __.Near imgIn =
            job {
                let img = getBaseImg ()
                let kernelName = if dim = 2 then "dilate" else "dilate3D"
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)

                let! event =
                    gpu()
                        .Run(
                            kernelName,
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member this.Through (img: GPUModelValue) (img2: GPUModelValue) = // TODO: URGENT: make multiple allocation transactional
            let lcc (img: GPUModelValue) =
                job {
                    let bimg = getBaseImg ()

                    let mutable flag: GPUValue<array<uint8>> = gpu().CopyArrayToDevice([| 0uy |]) // TODO: URGENT: leak?
                    let! temporary' = gpu().NewImageOnDevice(bimg, 4, Float32)
                    let mutable temporary = temporary'
                    let! meaningful' = gpu().NewImageOnDevice(bimg, 4, Float32)
                    let mutable meaningful = meaningful'

                    let kernelInit =
                        if dim = 2 then
                            "initCCL"
                        else
                            "initCCL3D"

                    let kernelIterate =
                        if dim = 2 then
                            "iterateCCL"
                        else
                            "iterateCCL3D"

                    let kernelReconnect =
                        if dim = 2 then
                            "reconnectCCL"
                        else
                            "reconnectCCL3D"

                    let! evt0 =
                        gpu()
                            .Run(
                                kernelInit,
                                img.GEvt,
                                seq {
                                    img.GVal 
                                    meaningful
                                },
                                bimg.Size,
                                None
                            )

                    // let! evt0 =
                    //     gpu()
                    //         .Run(
                    //             "copy",
                    //             [|evt0|],
                    //             seq {
                    //                 img.GVal 
                    //                 temporary
                    //             },
                    //             bimg.Size,
                    //             None
                    //         )

                    let rec iterate n evt meaningful temporary = // follows a double-buffering scheme on inp and out
                        job {
                            if n <= 0 then
                                return (meaningful,temporary,evt) 
                            else
                                let! evt' =
                                    gpu()
                                        .Run(
                                            "iterateCCL3D", // kernelIterate,
                                            [| evt |],
                                            seq {
                                                meaningful :> KernelArg
                                                temporary :> KernelArg
                                            },
                                            bimg.Size,
                                            None
                                        )

                                return! iterate (n - 1) evt' temporary meaningful // double buffering; after the call to .Run, temporary is now "meaningful"
                        }

                    let mutable terminated = false

                    let mutable whileEvt = evt0

                    while not terminated do                        
                        let! (m,t,evt1) = iterate 8 whileEvt meaningful temporary 
                        let! evt2 = 
                            gpu()
                                .Run(
                                    kernelReconnect,
                                    [| evt1 |],
                                    seq {
                                        m :> KernelArg
                                        t :> KernelArg
                                        flag :> KernelArg
                                    },
                                    bimg.Size,
                                    None
                                )                                   

                        gpu().Wait([| evt2 |]) // DO NOT REMOVE THIS
                        meaningful <- t // double buffering again
                        temporary <- m // double buffering again 
                        
                        if flag.Get().[0] > 0uy then     
                           let! whileEvtTmp =
                               gpu()
                                   .Run("resetFlag", [||], seq { flag }, [| 1 |], None)
                           whileEvt <- whileEvtTmp
                        else
                           terminated <- true                  

                    do! (temporary :> IDisposableJob).Dispose // can be disposed here, as we waited for the event already to read the flag TODO: URGENT: restore this line
                    return GPUModelValue(meaningful, [| |],gpu()) 
                }


            //Lock.duringJob shamefulLock <| // TODO change this to a multi-image allocation function on the GPU (this is here to avoid starvation)
            job {
                let baseImg = getBaseImg ()

                // let! tmp = gpu().NewImageOnDevice(baseImg, 1, UInt8)

                let! tmpResult = (this :> IBooleanModel<_>).BConst false
                let tmp = tmpResult.GVal
                let! outputResult = (this :> IBooleanModel<_>).BConst false
                let output = outputResult.GVal
            
                let! lccResult = lcc img2
                let lccImg = lccResult.GVal                

                let newEvents =
                    Array.distinct (Array.concat [|img.GEvt; lccResult.GEvt; tmpResult.GEvt; outputResult.GEvt|])

                let kernelInit =
                    if dim = 2 then
                        "initThrough"
                    else
                        "initThrough3D"

                let kernelFinalize =
                    if dim = 2 then
                        "finalizeThrough"
                    else
                        "finalizeThrough3D"

                let! event =
                    gpu()
                        .Run(
                            kernelInit,
                            newEvents,
                            seq {
                                img.GVal
                                lccImg
                                tmp
                            },
                            baseImg.Size,
                            None
                        )

                let! resultEvent =
                    gpu()
                        .Run(
                            kernelFinalize,
                            [| event |],
                            seq {
                                tmp
                                lccImg
                                output
                            },
                            baseImg.Size,
                            None
                        )

                let! _ = 
                    Job.queue
                    <| job {
                        gpu().Wait [| resultEvent |]
                        do! (tmp :> IDisposableJob).Dispose 
                        do! (lccResult :> IDisposableJob).Dispose
                        }

                return GPUModelValue(output, [| resultEvent |],gpu())
            }    

        member __.Interior imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let kernelName = if dim = 2 then "erode" else "erode3D"

                let! event =
                    gpu()
                        .Run(
                            kernelName,
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

    interface IDistanceModel<GPUModelValue> with
        member __.DT img =
            job {
                gpu().Wait img.GEvt

                let cpuImg = img.GVal.Get()

                let result = VoxImage.Dt cpuImg

                let! output = gpu().CopyImageToDevice result

                return GPUModelValue(output, [||],gpu())
            }

    interface IQuantitativeModel<GPUModelValue> with
        member __.Const value =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 value)

                let! event =
                    gpu()
                        .Run(
                            "constImg",
                            [||],
                            seq {
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.EqSV value imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f = gpu().Float32(float32 value)

                let! event =
                    gpu()
                        .Run(
                            "eq",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.GeqSV value imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f = gpu().Float32(float32 value)

                let! event =
                    gpu()
                        .Run(
                            "geqSV",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.LeqSV value imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f = gpu().Float32(float32 value)

                let! event =
                    gpu()
                        .Run(
                            "leq",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Between value1 value2 imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, UInt8)
                let f1 = gpu().Float32(float32 value1)
                let f2 = gpu().Float32(float32 value2)

                let! event =
                    gpu()
                        .Run(
                            "between",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f1 :> KernelArg
                                f2 :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Abs imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let! event =
                    gpu()
                        .Run(
                            "abs",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }
        //     member __.Max img = lift VoxImage.Max img

        // member __.Min img =

        //     job {
        //         gpu().Wait img.gEvt

        //         let cpuImg = img.gVal.Get()

        //         let result = VoxImage.Min cpuImg

        //         let output = gpu().CopyImageToDevice result

        //         return GPUModelValue(output, [||] ,gpu())
        //     }



        member __.SubtractVV img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "sub",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.AddVV img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "add",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.MultiplyVV img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "mul",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.DivideVV img1 img2 =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append img1.GEvt img2.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "div",
                            newEvents,
                            seq {
                                img1.GVal
                                img2.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.Mask imgIn maskImg =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)

                let tmpEvents =
                    Seq.distinct (Array.append imgIn.GEvt maskImg.GEvt)

                let newEvents = Seq.toArray tmpEvents

                let! event =
                    gpu()
                        .Run(
                            "mask",
                            newEvents,
                            seq {
                                imgIn.GVal
                                maskImg.GVal
                                output
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }
        //     member __.Avg (img : VoxImage) (maskImg : VoxImage)  = lift2 VoxImage.Avg img maskImg
        member __.AddVS imgIn k =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "addVS",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.MulVS imgIn k =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "mulVS",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.SubVS imgIn k =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "subVS",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.DivVS imgIn k =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "divVS",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.SubSV k imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "subSV",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

        member __.DivSV k imgIn =
            job {
                let img = getBaseImg ()
                let! output = gpu().NewImageOnDevice(img, 1, Float32)
                let f = gpu().Float32(float32 k)

                let! event =
                    gpu()
                        .Run(
                            "divSV",
                            imgIn.GEvt,
                            seq {
                                imgIn.GVal :> KernelArg
                                output :> KernelArg
                                f :> KernelArg
                            },
                            img.Size,
                            None
                        )

                return GPUModelValue(output, [| event |],gpu())
            }

// member __.Min imgIn =
//     job {
//         let evt = Array.append imgIn.gEvt mask.gEvt
//         gpu().Wait evt

//         let cpuImg = imgIn.gVal.Get()

//         let result = VoxImage.Min cpuImg

//         let output = gpu().Float32 (float32 result)

//         return GPUModelValue(output, [||] ,gpu())
//     }

// interface IStatisticalModel<VoxImage> with
//     member __.CrossCorrelation rho a b fb m1 m2 k = VoxImage.Crosscorrelation rho a b fb m1 m2 k

// // IMAGING
// [<OperatorAttribute("otsu",[|"valuation(number)";"valuation(bool)";"number"|],"valuation(bool)","otsu threshold (image, mask,number of bins)")>]
// member __.Otsu (img : VoxImage, mask : VoxImage, nbins : float) = job { return VoxImage.Otsu(img,mask,nbins) }
