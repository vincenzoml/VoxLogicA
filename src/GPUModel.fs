namespace VoxLogicA

open Hopac
open System.Collections.Generic
open Cloo
open System
open System.IO
open itk.simple

#nowarn "9"

type GPUModel() =
    inherit IModel()    
    // Find a context
    let mutable context = null    
    let _ = 
        printfn "%A" ComputePlatform.Platforms
        let mutable i = 0
        while isNull context && i < ComputePlatform.Platforms.Count do        
            let platform = ComputePlatform.Platforms.[i]    
            i <- i + 1
            printfn "1"
            try
                context <- new ComputeContext(ComputeDeviceTypes.Gpu,ComputeContextPropertyList(platform), null, IntPtr.Zero)                 
            with _ -> ()            
        if isNull context then
            i <- 0
            printfn "2"
            while isNull context && i < ComputePlatform.Platforms.Count do        
            let platform = ComputePlatform.Platforms.[i]    
            i <- i + 1
            try
                context <- new ComputeContext(ComputeDeviceTypes.Cpu,ComputeContextPropertyList(platform), null, IntPtr.Zero)                
            with _ -> ()                        
        if isNull context then 
            failwith "No GPU found, exiting."   
    let _ = printfn "%A" context; exit 0 
    let streamReader = new StreamReader(System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location) + "/kernel.cl")
    let source = streamReader.ReadToEnd()
    let _ = streamReader.Close()    
    let mutable baseImg : option<GPUImage> = None
    let mutable events : List<ComputeEventBase> = new List<ComputeEventBase>()
    let mutable queue : ComputeCommandQueue = new ComputeCommandQueue(context, context.Devices.[0], ComputeCommandQueueFlags.None)
    let program = new ComputeProgram(context, source)
    let build = 
        try
            program.Build(null, null, null, IntPtr.Zero)
        with e as BuildProgramFailureComputeException-> 
            let log = program.GetBuildLog (context.Devices.[0])
            printfn "%A" log
            exit -1
    let genKernels = program.CreateAllKernels()
    let kernels = genKernels 
               |> Seq.cast
               |> List.ofSeq
    let supportedExtensions = [".nii";".nii.gz";".png";".jpg";"bmp"]
    let mutable sitkImg : option<Image> = None
    let handler = GPUHandler(context)
    let mutable comps = 0ul

    let allocate (img : Image, pixeltype : PixelIDValueEnum) = // Does not guarantee the memory is cleared.
        match (img.GetNumberOfComponentsPerPixel(),img.GetPixelID() = pixeltype) with
            | (1ul,true) -> new Image(img)
            | (_,true) -> SimpleITK.VectorIndexSelectionCast(img,0ul)            
            | (1ul,false) -> SimpleITK.Cast(img,pixeltype)    
            | (_,_) -> SimpleITK.VectorIndexSelectionCast(img,0ul,pixeltype)

    let getBaseBuffer() = match baseImg with None -> raise NoGPUModelLoadedException | Some img -> img

    override __.CanSave t f = // TODO: check also if file can be written to, and delete it afterwards.        
        match t with 
        | (TValuation(_)|TModel) when List.exists (f.EndsWith : string -> bool) supportedExtensions -> true 
        | _ -> false        

    override __.Save filename v =
        let newimg = v :?> GPUImage
        ErrorMsg.Logger.Debug "Finalising the event queue"
        queue.Finish()
        ErrorMsg.Logger.DebugOnly (sprintf "saving image: %A" <| newimg.GetHashCode())
        handler.Save (filename, newimg, events, queue, sitkImg.Value)

    override __.Load s =
        let mutable img = SimpleITK.ReadImage(s)
        //printfn "%s" s
        let pixFormat = img.GetPixelIDTypeAsString()
        //printfn "%A" pixFormat
        comps <- img.GetNumberOfComponentsPerPixel()
        //printfn "comps %A" comps
        let channels = 
            match comps with
            | 1ul -> ComputeImageChannelOrder.R
            | 3u -> ComputeImageChannelOrder.Rgb //uint8 + 3u not supported in OpenCL
            | 4u -> ComputeImageChannelOrder.Rgba
            | _ -> raise (UnsupportedImageTypeException s)
        let format = 
            match pixFormat with
            | "8-bit unsigned integer" -> ComputeImageChannelType.UnsignedInt8
            | "16-bit unsigned integer" -> ComputeImageChannelType.UnsignedInt16
            | "32-bit unsigned integer" -> ComputeImageChannelType.UnsignedInt32
            //| "vector of 8-bit unsigned integer" -> ComputeImageChannelType.UnsignedInt8
            | "32-bit float" -> ComputeImageChannelType.Float
            | _ -> raise (UnsupportedImageTypeException s)
        //printfn "assigned format"
        let bytes = 
            match pixFormat with
            | "8-bit unsigned integer" -> img.GetBufferAsUInt8()
            | "16-bit unsigned integer" -> img.GetBufferAsUInt16()
            | "32-bit unsigned integer" -> img.GetBufferAsUInt32()
            | "vector of 8-bit unsigned integer" -> img.GetBufferAsUInt8()
            | _ -> img.GetBufferAsFloat()
        //printfn "got bytes"
        let informat = ComputeImageFormat(channels, format)
        let dims = img.GetSize()
        let width,height = int dims.[0], int dims.[1]
        let gpuImg = new ComputeImage2D(context,ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.UseHostPointer, informat, width, height, 0L, bytes)
        let inImg = GPUImage(gpuImg, channels, format)
        //printfn "create CI"
        let res =
            match baseImg with
            | None -> 
                baseImg <- Some inImg
                sitkImg <- Some img 
                inImg
            | Some img1 ->
                if GPUHandler.SamePhysicalSpace sitkImg.Value img
                then inImg
                else 
                    if img.GetNumberOfPixels() = sitkImg.Value.GetNumberOfPixels()
                        && img.GetDimension() = sitkImg.Value.GetDimension()
                    then 
                        if img.GetNumberOfComponentsPerPixel() = sitkImg.Value.GetNumberOfComponentsPerPixel() then 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\" has different physical space, but same logical structure than previously loaded images; physical space corrected." s)
                            let vect = allocate(img, PixelIDValueEnum.sitkFloat32)
                            let newImg = new Image(vect)
                            let newBytes = newImg.GetBufferAsFloat()
                            let buf = new ComputeImage2D(context,ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.UseHostPointer, informat, width, height, 0L, newBytes) :> ComputeImage               
                            GPUImage(buf, channels, format)
                        else 
                            ErrorMsg.Logger.Warning (sprintf "Image \"%s\"correcting physical space with different number of components is not currently supported; going to exit." s)                        
                            raise (DifferentPhysicalAndLogicalSpaceException s) 
                    else raise (DifferentPhysicalAndLogicalSpaceException s)
        ErrorMsg.Logger.Debug (sprintf "loaded image: %A" <| res.GetHashCode())
        //printfn "loaded"
        res :> obj    

    member this.Events
        with get () = events 
        and set (evts) = events <- evts  
    
    member this.Queue
        with get () = queue
        and set (q) = queue <- q

    interface IBoundedModel<GPUImage> with
        member __.Border = job { 
            return handler.UOp (getBaseBuffer(), events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "copyImg") kernels)) 
        }

    interface IImageModel<GPUImage> with
        member __.Intensity (img : GPUImage) = job {
            //printfn "Intensity"
            if img.BaseComps = ComputeImageChannelOrder.R then
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "copyImg") kernels))
            else
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "intensity") kernels))
        }
        //member __.Red (img : GPUImage) = job {
        //    //printfn "getting red"
        //    return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "getComponent") kernels),  0)
        //}
        //member __.Green (img : GPUImage) = job {
        //    //printfn "getting green"
        //    return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "getComponent") kernels), 1)
        //}
        //member __.Blue (img : GPUImage) = job {
        //    //printfn "getting blue"
        //    return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "getComponent") kernels), 2)
        //}
        member __.Alpha (img : GPUImage) = job {
            return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "getComponent") kernels), 3)
        }
        //member __.RGB (img : GPUImage) (img1 : GPUImage) (img2 : GPUImage) = job {
        //    return handler.RGB (img, img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "rgbComps") kernels))
        //}
        //member __.RGBA (img : GPUImage) (img1 : GPUImage) (img2 : GPUImage) (img3 : GPUImage) = job {
        //    return handler.RGBA (img, img1, img2, img3, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "rgbaComps") kernels))
        //}
        //member __.Volume (img : GPUImage) = job {
        //    //printfn "Volume"
        //    return handler.Volume (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "volume") kernels))
        //}
        //// ############# TODO #############
        //member __.MaxVol (img : GPUImage) = job {
        //    return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "redComponent") kernels), 0)
        //}
        //member __.Percentiles  (img : GPUImage) (img1 : GPUImage) (v : float) = job {
        //    return handler.GetComponent (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "redComponent") kernels), 0)
        //}
        //// ################################
        member __.LCC (img : GPUImage) = job {
            let ker1 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "initCCL") kernels)
            let ker2 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "iterateCCL") kernels)
            let ker3 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "termination") kernels)
            let ker4 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "reconnectCCL") kernels)
            let ker5 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "preTermination") kernels)
            let kers = [ker1.[0]; ker2.[0]; ker3.[0]; ker4.[0]; ker5.[0]]
            return handler.LabelComponents (img, events, queue, kers)
        }

    interface IBooleanModel<GPUImage> with
        member __.TT = job { 
                return handler.Const (getBaseBuffer(), events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "trueImg") kernels))
            }
        member __.FF = job { return handler.Const (getBaseBuffer(), events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "falseImg") kernels)) }

        member __.BConst value = job { 
            if value then
                return handler.Const (getBaseBuffer(), events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "trueImg") kernels))
            else
                return handler.Const (getBaseBuffer(), events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "falseImg") kernels))
            }

        member __.Not (img : GPUImage) = job { 
                //printfn "not"
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "not") kernels)) 
            }

        member __.And (img1 : GPUImage) (img2 : GPUImage) = job { 
                //printfn "and"
                return handler.BOp (img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "and") kernels)) 
            }

        member __.Or (img1 : GPUImage) (img2 : GPUImage) = job { 
                //printfn "or"
                return handler.BOp (img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "or") kernels)) 
            }

    interface IQuantitativeModel<GPUImage> with    
        member __.Const value = job { 
                //printfn "const"
                return handler.Const (getBaseBuffer(), value, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "constImg") kernels)) 
            }
        member __.EqSV value img = job { 
                //printfn "eq"
                return handler.BOpSV (value, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "eq") kernels)) 
            } 
        member __.GeqSV value img = job { 
                //printfn "geq"
                return handler.BOpSV (value, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "geq") kernels)) 
            }
        member __.LeqSV value img = job { 
                //printfn "leq"
                return handler.BOpSV (value, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "leq") kernels)) 
            }
        member __.Between value1 value2 img = job { 
                return handler.Between (value1, value2, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "between") kernels))
            }       
        member __.Abs img = job { 
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "absImg") kernels))
            }
        //member __.Max img = job { 
        //        return handler.MaxMin (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName  = "max") kernels)) 
        //    }
        //member __.Min img = job { 
        //        return handler.MaxMin (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName  = "min") kernels)) 
        //    }
        member __.SubtractVV img1 img2 = job { 
                return handler.BOp(img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "sub") kernels)) 
            }
        member __.AddVV img1 img2 = job { 
                return handler.BOp(img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "add") kernels)) 
            }
        member __.MultiplyVV img1 img2 = job { 
                return handler.BOp(img1, img2, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "mul") kernels)) 
            }
        member __.Mask (img : GPUImage) (maskImg : GPUImage) = job { 
                return handler.BOp(img, maskImg, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "mask") kernels)) 
            }
        //member __.Avg (img : GPUImage) (maskImg : GPUImage)  = job { 
        //        return handler.Avg (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "volume") kernels)) 
        //    }
        member __.AddVS (img : GPUImage) k = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "addVS") kernels))  
            }
        member __.MulVS (img : GPUImage) k = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "mulVS") kernels))  
            }
        member __.SubVS (img : GPUImage) k = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "subVS") kernels))  
            }
        member __.DivVS (img : GPUImage) k = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "divVS") kernels))  
            }
        member __.SubSV k (img : GPUImage) = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "subSV") kernels))  
            }
        member __.DivSV k (img : GPUImage) = job { 
                return handler.BOpSV (k, img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "divSV") kernels))  
            }

    // ############ TODO ############
    //interface IStatisticalModel<GPUImage> with
    //    member __.CrossCorrelation rho a b fb m1 m2 k = job { return getBaseBuffer() }

    interface ISpatialModel<GPUImage> with
        member __.Near img = job { 
                //printfn "Near"
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "dilate") kernels)) 
            }
        member __.Interior img = job { 
                //printfn "Interior"
                return handler.UOp (img, events, queue, (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "erode") kernels))
            }
        member __.Through img1 img2 = job { 
                //printfn "Through"
                let ker1 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "initCCL") kernels)
                let ker2 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "iterateCCL") kernels)
                let ker3 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "termination") kernels)
                let ker4 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "maskT") kernels)
                let ker5 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "through") kernels)
                let ker6 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "reconnectCCL") kernels)
                let ker7 = (List.filter (fun (x : ComputeKernel) -> x.FunctionName = "preTermination") kernels)
                let kers = [ker1.[0]; ker2.[0]; ker3.[0]; ker4.[0]; ker5.[0]; ker6.[0]; ker7.[0]]
                return handler.Through (img1, img2, events, queue, kers)
            }           
    // ############ TODO ############
    //interface IDistanceModel<GPUImage> with
    //    member __.DT img = job { return getBaseBuffer() }