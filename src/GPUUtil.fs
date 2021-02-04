namespace VoxLogicA

exception NoGPUModelLoadedException 
    with override __.Message = "No model loaded"

exception UnsupportedImageSizeException of s : string
    with override this.Message = sprintf "More than 2 dimensions are not supported with type: %s" this.s

exception UnsupportedImageTypeException of s : string
    with override this.Message = sprintf "Unsupported image type: %s" this.s

exception DifferentPhysicalAndLogicalSpaceException of s : string
    with override this.Message = sprintf "Image %s\ndiffers both in physical space and logical structure (number of voxels and dimensions) from previously loaded images." this.s

open System.Collections.Generic
open Cloo
open System
open System.IO
open itk.simple
open ErrorMsg
open FSharp.NativeInterop
open FSharp.Core.Operators

#nowarn "9"
open ErrorMsg
type GPUImage (img : ComputeImage, comps : ComputeImageChannelOrder, imgtype : ComputeImageChannelType) =
    let mutable baseImg = img
    let mutable baseComps = comps
    let mutable baseType = imgtype
    member this.BaseImg 
        with get() = baseImg
        and set(i) = baseImg <- i
    member this.BaseComps
        with get() = baseComps
        and set(c) = baseComps <- c
    member this.BaseType
        with get() = baseType
        and set(t) = baseType <- t
    interface IDisposable with
        member this.Dispose () =
            Logger.Debug (sprintf "called dispose of GPUImage with buffer %A" baseImg)
            baseImg.Dispose()

type GPUHandler (ctx : ComputeContext) =
    //GPU computation and buffers handling
    let context = ctx
    let tiledim = 4

    static member SamePhysicalSpace (img1 : Image) (img2 : Image) =
        try
            use x = SimpleITK.Add(img1,img2) 
            true
        with _ -> 
            false

    member __.Save (filename : string, tosave : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, baseImg : Image) =
        let fname = System.IO.Path.GetFileName(filename)
        //let format = baseImg.GetPixelIDTypeAsString()
        let buf = tosave.BaseImg
        let img = 
            match tosave.BaseType with
            | ComputeImageChannelType.UnsignedInt8 -> new Image(new VectorUInt32 [|uint32 (baseImg.GetWidth());uint32 (baseImg.GetHeight())|],PixelIDValueEnum.sitkUInt8)
            | ComputeImageChannelType.UnsignedInt16 -> new Image(new VectorUInt32 [|uint32 (baseImg.GetWidth());uint32 (baseImg.GetHeight())|],PixelIDValueEnum.sitkUInt16)
            | _ -> new Image(new VectorUInt32 [|uint32 (baseImg.GetWidth());uint32 (baseImg.GetHeight())|],PixelIDValueEnum.sitkFloat32)
        let bytes =
            match tosave.BaseType with
            | ComputeImageChannelType.UnsignedInt8 -> img.GetBufferAsUInt8()
            | ComputeImageChannelType.UnsignedInt16 -> img.GetBufferAsUInt16()
            //| "vector of 8-bit unsigned integer" -> img.GetBufferAsUInt8()
            | _ -> img.GetBufferAsFloat()
        queue.ReadFromImage(buf, bytes, true, events)
        if fname.EndsWith(".jpg") && img.GetNumberOfComponentsPerPixel() > 3ul then
            Logger.Warning <| sprintf "Saving to %s\nusing 4 components per pixel. The resulting image will be CMYK; only proceed if you know what you are doing." fname
        let tmp = 
            if filename.EndsWith(".nii") || filename.EndsWith(".nii.gz") then img
            else 
                if filename.EndsWith(".png") || filename.EndsWith(".jpg") || filename.EndsWith ("bmp") then
                    if img.GetSize().Count = 2 then 
                        if img.GetPixelID() <> PixelIDValueEnum.sitkUInt8 then
                            if img.GetPixelID() = PixelIDValueEnum.sitkUInt16 then
                                SimpleITK.Multiply(img, 65535.0)
                            else
                                // TODO: double-check that "nearest integer" in the message below is correct
                                Logger.Warning (sprintf "saving to %s\nrequires cast to uint8. For each component, only values between 0 and 255 are preserved, rounded to the nearest integer; the behaviour on values outside this range is unspecified." fname)                    
                                let ncomp = img.GetNumberOfComponentsPerPixel()
                                if ncomp = 1ul
                                then SimpleITK.Cast(img,PixelIDValueEnum.sitkUInt8)
                                else // TODO: why simply casting the image doesn't work here? It works in load
                                    let comps = Array.init (int ncomp) (fun i -> SimpleITK.VectorIndexSelectionCast(img,uint32 i,PixelIDValueEnum.sitkUInt8))
                                    use flt = new ComposeImageFilter()
                                    use v = new VectorOfImage(comps)
                                    flt.Execute(v)                            
                        else
                            Logger.Warning (sprintf "saving boolean image to %s; value 'true' is set to 255, not 1"  fname)
                            SimpleITK.Multiply(img,255.0)                            
                    else raise <| UnsupportedImageSizeException (Path.GetExtension filename)
                else raise <| UnsupportedImageTypeException (Path.GetExtension filename)
        Logger.Debug <| sprintf "Saving file %s" filename        
        SimpleITK.WriteImage(tmp, filename)

    member this.GetComponent (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>, com : int) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, img.BaseType)        
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        let mutable comp = 
            match img.BaseComps with
            | ComputeImageChannelOrder.R -> Array.create 1 0
            | _ -> Array.create 1 com
        let compPtr : nativeint = NativePtr.toNativeInt<int> &&comp.[0]
        let arg = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, compPtr)
        kernel.[0].SetMemoryArgument(2, arg)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)        
        GPUImage(obuf, ComputeImageChannelOrder.R, img.BaseType)

    //member this.RGB (img1 : GPUImage, img2 : GPUImage, img3 : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
    //    let outformat = ComputeImageFormat(ComputeImageChannelOrder.Rgbx, img1.BaseType)
    //    let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
    //    kernel.[0].SetMemoryArgument(0, img1.BaseImg)
    //    kernel.[0].SetMemoryArgument(1, img2.BaseImg)
    //    kernel.[0].SetMemoryArgument(2, img3.BaseImg)
    //    kernel.[0].SetMemoryArgument(3, obuf)
    //    queue.Execute(kernel.[0], null, [|int64 img1.BaseImg.Width; int64 img1.BaseImg.Height|], null, events)
    //    GPUImage(obuf, ComputeImageChannelOrder.Rgbx, img1.BaseType)

    //member this.RGBA (img1 : GPUImage, img2 : GPUImage, img3 : GPUImage, img4 : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
    //    let outformat = ComputeImageFormat(ComputeImageChannelOrder.Rgba, img1.BaseType)
    //    let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
    //    kernel.[0].SetMemoryArgument(0, img1.BaseImg)
    //    kernel.[0].SetMemoryArgument(1, img2.BaseImg)
    //    kernel.[0].SetMemoryArgument(2, img3.BaseImg)
    //    kernel.[0].SetMemoryArgument(3, img4.BaseImg)
    //    kernel.[0].SetMemoryArgument(4, obuf)
    //    queue.Execute(kernel.[0], null, [|int64 img1.BaseImg.Width; int64 img1.BaseImg.Height|], null, events)
    //    GPUImage(obuf, ComputeImageChannelOrder.Rgba, img1.BaseType)

    member this.Const (buf : GPUImage, value : float, events: List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, buf.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, buf.BaseImg.Width, buf.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, obuf)
        let mutable v = Array.create 1 (int (ceil value))
        let vPtr : nativeint = NativePtr.toNativeInt<int> &&v.[0]
        let arg = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, vPtr)
        kernel.[0].SetMemoryArgument(1, arg)
        queue.Execute(kernel.[0], null, [|int64 buf.BaseImg.Width;int64 buf.BaseImg.Height|], null, events)
        GPUImage(obuf, ComputeImageChannelOrder.R, buf.BaseType)

    member this.BoolBOpSV (value : float, img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(img.BaseComps, ComputeImageChannelType.UnsignedInt8)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        let mutable v = Array.create 1 (int (ceil value))
        let vPtr : nativeint = NativePtr.toNativeInt<int> &&v.[0]
        let arg = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, vPtr)
        kernel.[0].SetMemoryArgument(2, arg)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)
        GPUImage(obuf, img.BaseComps, ComputeImageChannelType.UnsignedInt8)

    member this.BOpSV (value : float, img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(img.BaseComps, img.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)        
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        let mutable v = Array.create 1 (int (ceil value))
        let vPtr : nativeint = NativePtr.toNativeInt<int> &&v.[0]
        let arg = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, vPtr)
        kernel.[0].SetMemoryArgument(2, arg)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)
        GPUImage(obuf, img.BaseComps, img.BaseType)

    member this.Between (value1 : float, value2 : float, img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, img.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        let mutable v = Array.create 2 0
        v.[0] <- (int (ceil value1))
        v.[1] <- (int (ceil value2))
        let arg1 = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, NativePtr.toNativeInt<int> &&v.[0])
        let arg2 = new ComputeBuffer<int>(context,ComputeMemoryFlags.ReadOnly ||| ComputeMemoryFlags.UseHostPointer, 1L, NativePtr.toNativeInt<int> &&v.[1])
        kernel.[0].SetMemoryArgument(2, arg1)
        kernel.[0].SetMemoryArgument(3, arg2)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)
        GPUImage(obuf, ComputeImageChannelOrder.R, img.BaseType)
        
    member this.BoolBOp (img1 : GPUImage, img2 : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(img1.BaseComps, ComputeImageChannelType.UnsignedInt8)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img1.BaseImg)
        kernel.[0].SetMemoryArgument(1, img2.BaseImg)
        kernel.[0].SetMemoryArgument(2, obuf)
        queue.Execute(kernel.[0], null, [|int64 img1.BaseImg.Width;int64 img1.BaseImg.Height|], null, events)
        GPUImage(obuf, img1.BaseComps, ComputeImageChannelType.UnsignedInt8)

    member this.UOp (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(img.BaseComps, img.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)
        GPUImage(obuf, img.BaseComps, img.BaseType)

    //member this.Volume (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
    //    let obuf = new ComputeBuffer<float>(context,ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, int64 (img.BaseImg.Width*img.BaseImg.Height))
    //    kernel.[0].SetMemoryArgument(0, img.BaseImg)
    //    kernel.[0].SetMemoryArgument(1, obuf)
    //    kernel.[0].SetLocalArgument(2, int64 (tiledim*tiledim))
    //    queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], [| (int64) tiledim; (int64) tiledim|], events)
    //    let resultBuffer = Array.create (img.BaseImg.Width*img.BaseImg.Height) 0.0
    //    queue.ReadFromBuffer(obuf, ref resultBuffer, true, null)
    //    let mutable result = 0.0
    //    for i = 0 to img.BaseImg.Width*img.BaseImg.Height - 1 do
    //        result <- result + resultBuffer.[i]
    //    result 

    member this.Avg (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let obuf = new ComputeBuffer<float>(context,ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, int64 (img.BaseImg.Width*img.BaseImg.Height))
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        let resultBuffer = Array.create (img.BaseImg.Width*img.BaseImg.Height) 0.0
        queue.ReadFromBuffer(obuf, ref resultBuffer, true, null)
        let mutable result = 0.0
        for i = 0 to img.BaseImg.Width*img.BaseImg.Height - 1 do
            result <- result + resultBuffer.[i]
        result/(float (img.BaseImg.Width*img.BaseImg.Height))

    member this.Const (buf : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, buf.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, buf.BaseImg.Width, buf.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, obuf)
        queue.Execute(kernel.[0], null, [|int64 buf.BaseImg.Width;int64 buf.BaseImg.Height|], null, events)
        GPUImage(obuf, ComputeImageChannelOrder.R, buf.BaseType)

    member this.BOp (img1 : GPUImage, img2 : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(img1.BaseComps, img1.BaseType)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img1.BaseImg)
        kernel.[0].SetMemoryArgument(1, img2.BaseImg)
        kernel.[0].SetMemoryArgument(2, obuf)
        queue.Execute(kernel.[0], null, [|int64 img1.BaseImg.Width;int64 img1.BaseImg.Height|], null, events)
        //let out = new Image(new VectorUInt32 [|uint32 img1.BaseImg.Width;uint32 img1.BaseImg.Height|],PixelIDValueEnum.sitkUInt16)
        //let obytes = out.GetBufferAsUInt16()
        //queue.ReadFromImage(obuf,obytes,true,events)
        //SimpleITK.WriteImage(SimpleITK.Multiply(out, 65535.0),"or.png")
        GPUImage(obuf, img1.BaseComps, img1.BaseType)

    //member this.MaxMin (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
    //    let obuf = new ComputeBuffer<float>(context,ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, int64 (img.BaseImg.Width*img.BaseImg.Height))
    //    kernel.[0].SetMemoryArgument(0, img.BaseImg)
    //    kernel.[0].SetMemoryArgument(1, obuf)
    //    kernel.[0].SetLocalArgument(2, int64 (tiledim*tiledim))
    //    queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], [| (int64) tiledim; (int64) tiledim|], events)
    //    let resultBuffer = Array.create (img.BaseImg.Width*img.BaseImg.Height) 0.0
    //    queue.ReadFromBuffer(obuf, ref resultBuffer, true, null)
    //    let mutable result = 0.0
    //    for i = 0 to img.BaseImg.Width*img.BaseImg.Height - 1 do
    //        result <- result + resultBuffer.[i]
    //    result 
    //    //complete reduction here

    member this.Not (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt8)
        let obuf = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)
        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obuf)
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], null, events)
        GPUImage(obuf, ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt8)

    member this.LabelComponents (img : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        let outformat = ComputeImageFormat(ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt32)
        let obufs = [| new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero);
                       new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero);
                       new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat, img.BaseImg.Width, img.BaseImg.Height, 0L, IntPtr.Zero)|]
        
        // TEST
        // queue.Finish()
        // Logger.Debug "start cc"
        // END TEST

        kernel.[0].SetMemoryArgument(0, img.BaseImg)
        kernel.[0].SetMemoryArgument(1, obufs.[0])
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width; int64 img.BaseImg.Height|], null, events)
        kernel.[1].SetMemoryArgument(0, img.BaseImg)
        kernel.[1].SetMemoryArgument(1, obufs.[1])        
        queue.Execute(kernel.[0], null, [|int64 img.BaseImg.Width; int64 img.BaseImg.Height|], null, events)

        kernel.[1].SetMemoryArgument(0, img.BaseImg)
        kernel.[3].SetMemoryArgument(0, img.BaseImg)
        kernel.[4].SetMemoryArgument(0, img.BaseImg)
        // let mutable threshold = 
            // try int (System.Environment.ExpandEnvironmentVariables "%NUM_ITERATIONS%")
            // with _ -> 20

        
        let swap () =
            let tmp = obufs.[0]           
            obufs.[0] <- obufs.[1]
            obufs.[1] <- tmp        

        // let out = new Image(new VectorUInt32 [|uint32 img.BaseImg.Width;uint32 img.BaseImg.Height|],PixelIDValueEnum.sitkUInt32)
        // let save it =            
        //     let obytes = out.GetBufferAsUInt32()
        //     queue.ReadFromImage(obufs.[0],obytes,true,events)  
        //     SimpleITK.WriteImage(out,sprintf "lcc_%03d-seg.nii.gz" it)                          

        let mutable finish = false
        let mutable cnt = 0 
        
        while not finish do
            // Logger.Debug "iterating main kernel"            
            // Pointer jumping:
            for _ = 0 to 8 do // TODO: 8 is hardcoded, can we do better?
                kernel.[1].SetMemoryArgument(1,obufs.[0])
                kernel.[1].SetMemoryArgument(2,obufs.[1])
                queue.Execute(kernel.[1], null, [|int64 img.BaseImg.Width; int64 img.BaseImg.Height|], null, events)
                swap()
                cnt <- cnt + 1                
                // save cnt // TODO: removeme debug only!!!

            // queue.Finish() // TODO: removeme debug only!!!
            // Logger.Debug "checking termination"
            // Termination test:
            kernel.[4].SetMemoryArgument(1,obufs.[0])
            kernel.[4].SetMemoryArgument(2,obufs.[1])
            queue.Execute(kernel.[4], null, [|int64 img.BaseImg.Width; int64 img.BaseImg.Height|], null, events)            
            // DO NOT: swap() because we want to destroy obufs.[1] if not finished (by running kernel.[3])
            
            let res = new ComputeBuffer<int32>(context, ComputeMemoryFlags.WriteOnly ||| ComputeMemoryFlags.AllocateHostPointer, int64 ((img.BaseImg.Width*img.BaseImg.Height)/(tiledim*tiledim)))
            kernel.[2].SetMemoryArgument(0, obufs.[0])
            kernel.[2].SetMemoryArgument(1, obufs.[1])
            kernel.[2].SetMemoryArgument(2, res) 
            kernel.[2].SetLocalArgument(3, int64 (tiledim*tiledim))
            queue.Execute(kernel.[2], null, [|int64 img.BaseImg.Width;int64 img.BaseImg.Height|], [| (int64) tiledim; (int64) tiledim|], events)
            let results = Array.create ((img.BaseImg.Width*img.BaseImg.Height)/(tiledim*tiledim)) 0
            queue.ReadFromBuffer(res, ref results, true, events)
            let mutable vol = 1
            for i = 0 to results.Length - 1 do
                vol <- vol * results.[i]

            finish <- vol <> 0 

            // Adjustment before new pointer labelling:               
            if not finish then 
                // Logger.Debug "restarting"
                kernel.[3].SetMemoryArgument(1,obufs.[0])
                kernel.[3].SetMemoryArgument(2,obufs.[1])
                queue.Execute(kernel.[3], null, [|int64 img.BaseImg.Width; int64 img.BaseImg.Height|], null, events)
                swap()    
                cnt <- cnt + 1                 
                // save cnt // TODO: removeme debug only!!!        
        // cnt <- cnt + 1                 
        // save cnt // TODO: removeme debug only!!!
        
        Logger.Debug <| sprintf "Connected components labelling terminated in %d iterations" cnt

        GPUImage(obufs.[0], ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt32)

    member this.Through (img1 : GPUImage, img2 : GPUImage, events : List<ComputeEventBase>, queue : ComputeCommandQueue, kernel : list<ComputeKernel>) =
        //printfn "executing through"
        //ErrorMsg.Logger.Debug "Through step 1"        
        let kernel2 = [kernel.[0];kernel.[1];kernel.[2];kernel.[5];kernel.[6]]
        let cc = this.LabelComponents(img2, events, queue, kernel2)
         
        //ErrorMsg.Logger.Debug "Through step 2"        
        //let out = new Image(new VectorUInt32 [|uint32 img1.BaseImg.Width;uint32 img1.BaseImg.Height|],PixelIDValueEnum.sitkUInt32)
        //let obytes = out.GetBufferAsUInt32()
        //queue.ReadFromImage(cc.BaseImg,obytes,true,events)
        //SimpleITK.WriteImage(out,"cc.nii")
        let outformat1 = ComputeImageFormat(ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt32)
        let outformat2 = ComputeImageFormat(ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt16)
        let obuf1 = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat1, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
        let obuf2 = new ComputeImage2D(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, outformat2, img1.BaseImg.Width, img1.BaseImg.Height, 0L, IntPtr.Zero)
        let colors = new ComputeBuffer<int32>(context, ComputeMemoryFlags.ReadWrite ||| ComputeMemoryFlags.AllocateHostPointer, int64 (img1.BaseImg.Height*img1.BaseImg.Width))
        kernel.[3].SetMemoryArgument(0, cc.BaseImg)
        kernel.[3].SetMemoryArgument(1, img1.BaseImg)
        kernel.[3].SetMemoryArgument(2, obuf1)
        kernel.[3].SetMemoryArgument(3, colors)
        //ErrorMsg.Logger.Debug "Through step 3"
        queue.Execute(kernel.[3], null, [|int64 img1.BaseImg.Width; int64 img1.BaseImg.Height|], null, events)
        //let out = new Image(new VectorUInt32 [|uint32 img1.BaseImg.Width;uint32 img1.BaseImg.Height|],PixelIDValueEnum.sitkUInt32)
        //let obytes = out.GetBufferAsUInt32()
        //queue.ReadFromImage(obuf1,obytes,true,events)
        //SimpleITK.WriteImage(SimpleITK.Multiply(out, 65535.0),"through.nii")
         
        //ErrorMsg.Logger.Debug "Through step 4"
        kernel.[4].SetMemoryArgument(0, obuf1)
        kernel.[4].SetMemoryArgument(1, cc.BaseImg)
        kernel.[4].SetMemoryArgument(2, obuf2)
        kernel.[4].SetMemoryArgument(3, colors)
        //ErrorMsg.Logger.Debug "Through step 5"        
        queue.Execute(kernel.[4], null, [|int64 img1.BaseImg.Width; int64 img1.BaseImg.Height|], null, events)
         
        //ErrorMsg.Logger.Debug "Through step 6"        
        GPUImage(obuf2, ComputeImageChannelOrder.R, ComputeImageChannelType.UnsignedInt16)