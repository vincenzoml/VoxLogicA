module VoxLogicA.GPU

#nowarn "9"

open Hopac
open Silk.NET.Core.Native
open VoxLogicA.SITKUtil
open Silk.NET.OpenCL
open System.Collections.Generic
open FSharp.NativeInterop
open itk.simple

exception GPUException of c : int
    with 
        override this.Message = sprintf "GPU error. Code: %d %s" this.c (System.Enum.GetName (LanguagePrimitives.EnumOfValue this.c : CLEnum))
        member this.Code = this.c

exception GPUCompileException of s : string
    with override this.Message = sprintf "Could not compile GPU kernels:\n%s" this.s

exception UnsupportedImageDimensionException of i : int
    with override this.Message = sprintf "Unsupported image dimension: %d (only 2D and 3D images are supported by the GPU implementation)" this.i

exception FormatMismatchInGPUMemoryTransferException
    with override this.Message = "Format mismatch in GPU to CPU memory transfer or CPU to GPU memory transfer"

exception OutOfGPUMemoryException
    with override this.Message = "GPU Memory exhausted."

let nullPtr : nativeptr<nativeint> = NativePtr.ofNativeInt 0n
let uNullPtr : nativeptr<unativeint> = NativePtr.ofNativeInt 0n
let bNullPtr : nativeptr<byte> = NativePtr.ofNativeInt 0n
let nbNullPtr : nativeptr<nativeptr<byte>> = NativePtr.ofNativeInt 0n
let vNullPtr : voidptr = NativePtr.toVoidPtr nullPtr
let noNotify = NotifyCallback(fun _ _ _ _ -> ())

let checkErr code =
    if code = int CLEnum.Success then ()
    else raise <| GPUException code

let checkErrPtr f =
    let err = [|0|]
    use errPtr = fixed err
    let res = f errPtr
    let code = err.[0]
    checkErr code
    res

let API = CL.GetApi()
 
type Pointer = private { Pointer : nativeint }

type Event = private { EventPointer : nativeint }

type Data = { DataPointer : nativeint } // TODO: this was private

type KArgType = Buffer of Data | Float of float32 
type Pool() = 
    let lck = ref ()
    let mutable queue = []
    let mutable waitingList = []

    member __.NumBlockedThreads () = 
        lock lck (fun () -> List.length waitingList)

    member __.Put (x : nativeint) =
        lock lck (fun () -> 
                    ErrorMsg.Logger.DebugOnly <| sprintf "PUT: %A" x 
                    match waitingList with
                    | [] -> 
                        queue <- x::queue
                        Job.result()
                    | y::ys -> 
                        waitingList <- ys
                        IVar.fill y x) 
        

    // member __.Get () =
    //     job {
    //         ErrorMsg.Logger.DebugOnly "GET"
    //         return lock lck (fun () -> 
    //             match queue with
    //             | [] -> ErrorMsg.Logger.DebugOnly <| sprintf "GET returned: None"; None
    //             | x::xs -> 
    //                 queue <- xs
    //                 ErrorMsg.Logger.DebugOnly <| sprintf "GET returned: %A" x;
    //                 Some x)
    //     }

    member __.Wait () =           
        ErrorMsg.Logger.DebugOnly "WAIT started"
        let r = lock lck (fun () -> 
                match queue with
                | [] -> 
                    let iv = new IVar<_>()     
                    waitingList <- iv::waitingList                                     
                    job {
                        let! r = IVar.read iv                    
                        ErrorMsg.Logger.DebugOnly <| sprintf "WAIT returned: %A" r 
                        return r
                    }
                | x::xs ->
                    queue <- xs
                    Job.result x)
        r

type ImageKey = {
    NComponents : int
    BufferType : PixelType
    Dimension: int
    Width: int
    Height: int
    Depth: int
}
type MemoryKey = 
    Image of ImageKey
type GPUMemory() =
    static let globalLock = ref ()
    static let pools = new Dictionary<MemoryKey,Pool>()    
    static let mutable onWait = Job.unit ()

    static member OnWait j = onWait <- j

    static member Put key mem = 
        ErrorMsg.Logger.DebugOnly <| sprintf "PUT %A %A" key mem
        let pool = 
            lock globalLock (fun () -> 
                try pools.[key]
                with :? KeyNotFoundException ->                    
                    let r = Pool()
                    pools.[key] <- r
                    r
            )
        pool.Put mem        

    static member Wait key = job {
        ErrorMsg.Logger.DebugOnly <| sprintf "WAIT %A" key        
        let pool =
            lock globalLock (fun () ->
                try 
                    pools.[key]

                with :? KeyNotFoundException ->
                    let r = Pool()
                    pools.[key] <- r
                    r
            )   
        do! onWait                            
        return! pool.Wait()
    }

    static member NumBlockedThreads () = 
        lock globalLock (fun () -> Seq.sumBy (fun (kv : KeyValuePair<MemoryKey,Pool>) -> kv.Value.NumBlockedThreads()) pools)

    // static member Get key = 
    //     ErrorMsg.Logger.DebugOnly <| sprintf "GET %A" key
    //     lock globalLock (fun () ->
    //         try 
    //             pools.[key].Get()
    //         with :? KeyNotFoundException -> 
    //             job { return None })

[<AbstractClass>]
type KernelArg() =
    inherit RefCount()    
    abstract member Value : KArgType        
                         
[<AbstractClass>]
type GPUValue<'a>() =    
    inherit KernelArg()
    abstract member Get : unit -> 'a 

type private GPUFloat (x : float32) =
    inherit GPUValue<float32>()

    override __.ToString() = sprintf "<GPUFloat %f>" x

    override __.Value = Float x
    override __.Get() = x

type private GPUArray<'a when 'a : unmanaged> (dataPointer : nativeint,length : int,queue : Pointer) =
    inherit GPUValue<array<'a>>()
    // override __.Delete() =
    //     GPUMemory.Put ("array",length) dataPointer
    override __.ToString() = sprintf "<GPUArray %d>" dataPointer
    override __.Value = Buffer { DataPointer = dataPointer }
    override _.Get () =
        let dest = Array.zeroCreate length
        use ptr = fixed dest
        let ptr' = NativePtr.toVoidPtr ptr
        checkErr <| API.EnqueueReadBuffer(queue.Pointer,dataPointer,true,0un,unativeint (length * sizeof<'a>),ptr',0ul,nullPtr,nullPtr)
        dest

type private GPUImage (dataPointer : nativeint,img : VoxImage, nComponents : int, bufferType : PixelType, queue : Pointer) =     
    inherit GPUValue<VoxImage>() 
    override __.Delete =
        ErrorMsg.Logger.DebugOnly <| sprintf "PUT %A %A" nComponents bufferType
        GPUMemory.Put (Image { 
            NComponents = nComponents
            BufferType = bufferType
            Dimension = img.Dimension
            Width = img.Width
            Height = img.Height
            Depth = img.Depth
        }) dataPointer
    override __.ToString() = sprintf "<GPUImage %d>" dataPointer
    override __.Value = Buffer { DataPointer = dataPointer }        
    override __.Get () =         
        let pixelID = 
            match bufferType with
            | Float32 -> PixelIDValueEnum.sitkFloat32
            | UInt8 -> PixelIDValueEnum.sitkUInt8 

        let destination = 
            if img.NComponents = nComponents then 
                if nComponents = 1 then new VoxImage(img,pixelID)
                else new VoxImage(img,nComponents,pixelID)
            else
                match nComponents with
                | 1 -> new VoxImage(VoxImage.Red(img),pixelID) // TODO optimize this double allocation
                | 4 -> new VoxImage(img,4,pixelID) // TODO optimize this double allocation
                | x -> raise <| UnsupportedNumberOfComponentsPerPixelException x

        use startPtr = fixed [|0un;0un;0un|]
        use endPtr = fixed [|unativeint destination.Size.[0];unativeint destination.Size.[1];unativeint (if destination.Size.Length >= 3 then destination.Size.[2] else 1)|]            
        
        match destination.BufferType with
        | UInt8 ->
            destination.GetBufferAsUInt8
                (fun buf -> 
                    let ptr = NativePtr.toVoidPtr buf.Pointer
                    checkErr <| API.EnqueueReadImage(queue.Pointer,dataPointer,true,startPtr,endPtr,0un,0un,ptr,0ul,nullPtr,nullPtr))                            
        | Float32 ->
            destination.GetBufferAsFloat
                (fun buf -> 
                    let ptr = NativePtr.toVoidPtr buf.Pointer
                    checkErr <| API.EnqueueReadImage(queue.Pointer,dataPointer,true,startPtr,endPtr,0un,0un,ptr,0ul,nullPtr,nullPtr))

        destination

type Kernel = 
    {   Name : string
        Pointer : Pointer     }    

and GPU(kernelsFilename : string) =
    static let imageCount = Dictionary<_,_>()

    let mutex = ref ()

    // let mutable dimensionIndex = 0

    let _ = ErrorMsg.Logger.Debug "Initializing GPU"

    let platformIDs =
        let mutable num = 0ul
        do
            use ptr = fixed [||]        
            checkErr <| API.GetPlatformIDs (uint32 0,ptr,&num)            
        let platformIDs = Array.create (int num) 0n
        do
            use ptr = fixed platformIDs
            checkErr <| API.GetPlatformIDs (uint32 num,ptr,&num)  
        platformIDs

    let deviceType = 
        if isNull <| System.Environment.GetEnvironmentVariable "VOXLOGICA_USE_CPU" then 
            CLEnum.DeviceTypeGpu
        else
            CLEnum.DeviceTypeCpu

    let deviceGroups =        
        let mutable res = [] 
        for platformID in platformIDs do
            let mutable num = 0ul            
            use ptr = fixed [||]           
            if 0 = API.GetDeviceIDs(platformID,deviceType,0ul,ptr,&num) then
                if num > 0ul then
                    let tmp = Array.create (int num) 0n
                    use ptr = fixed tmp
                    if 0 = API.GetDeviceIDs(platformID,deviceType,uint32 num,ptr,&num) then                    
                        res <- (platformID,tmp)::res
        match res with 
        | [] -> raise <| GPUException 0 // TODO: error code for unknown error?
        | _ -> res    

    let (_,devices) = List.head deviceGroups // TODO: this blindly selects the first platform         

    let (device,context) = 
        // TODO: the returned "device" is needed to create the command queue, but I don't understand why an array of devices can be passed to CreateContext and only one device to CreateCommandQueue        
        use devicesPtr = fixed devices
        checkErrPtr (fun errPtr -> (devices.[0],API.CreateContext(nullPtr,1ul,devicesPtr,noNotify,vNullPtr,errPtr)))   // TODO: always selects first device              

    let kernelsByDim dimension =
        let source = 
            let defineString = "#pragma OPENCL EXTENSION cl_khr_3d_image_writes : enable\n #define DIM " + (string dimension) + "\n"
            use streamReader = new System.IO.StreamReader(kernelsFilename)
            let kernels = streamReader.ReadToEnd()
            let res = defineString + kernels
            streamReader.Close()
            res

        let program =         
            ErrorMsg.Logger.Debug "Compiling kernels"
            let prg = checkErrPtr (fun errPtr -> API.CreateProgramWithSource(context,1ul,[|source|],uNullPtr,errPtr))                
            let res = API.CompileProgram(prg,0ul,nullPtr,bNullPtr,0ul,nullPtr,nbNullPtr,noNotify,vNullPtr) 
            if res = int CLEnum.CompileProgramFailure then        
                let paramName : uint32 = uint32 CLEnum.ProgramBuildLog            
                let mutable len = [|0un|]
                use lenPtr = fixed len                        
                checkErr (API.GetProgramBuildInfo(prg,device,paramName,0un,vNullPtr,lenPtr)) 
                let output = SilkMarshal.Allocate (int len.[0] + 1)
                let outputPtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
                checkErr (API.GetProgramBuildInfo(prg,device,paramName,len.[0],outputPtr,uNullPtr)) 
                let error = SilkMarshal.PtrToString(output,NativeStringEncoding.Auto)            
                raise <| GPUCompileException error
            let res = API.BuildProgram(prg,0ul,nullPtr,bNullPtr,noNotify,vNullPtr)     
            if res = int CLEnum.BuildProgramFailure then        
                let paramName : uint32 = uint32 CLEnum.ProgramBuildLog            
                let mutable len = [|0un|]
                use lenPtr = fixed len                        
                checkErr (API.GetProgramBuildInfo(prg,device,paramName,0un,vNullPtr,lenPtr)) 
                let output = SilkMarshal.Allocate (int len.[0] + 1)
                let outputPtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
                checkErr (API.GetProgramBuildInfo(prg,device,paramName,len.[0],outputPtr,uNullPtr)) 
                let error = SilkMarshal.PtrToString(output,NativeStringEncoding.Auto)            
                raise <| GPUCompileException error
            checkErr res 
            ErrorMsg.Logger.Debug "Kernels compiled"
            prg               

        let kernels = // TODO: it is possible in opencl to retrieve information on each parameter (type etc.) with a suitable option to the compiler. Use this to generate typed methods (with a typeprovider?)
            let num = [|0ul|]
            use numPtr = fixed num
            checkErr (API.CreateKernelsInProgram(program,0ul,nullPtr,numPtr))        
            let kv = Array.create (int num.[0]) 0n        
            use kvPtr = fixed kv
            checkErr (API.CreateKernelsInProgram(program,num.[0],kvPtr,numPtr))
            let kernels = kv.[0..(int num.[0]-1)]
            let dict = Dictionary<_,_>()
            for (k,v) in
                Array.map 
                    (fun k -> 
                        let len = [|0un|]
                        use lenPtr = fixed len
                        checkErr <| API.GetKernelInfo(k,uint32 CLEnum.KernelFunctionName,0un,vNullPtr,lenPtr) 
                        let name = SilkMarshal.Allocate (int len.[0] + 1)
                        let namePtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt name : nativeptr<int>)) : voidptr
                        checkErr <| API.GetKernelInfo(k,uint32 CLEnum.KernelFunctionName,len.[0],namePtr,uNullPtr) 
                        let name = SilkMarshal.PtrToString(name,NativeStringEncoding.Auto)
                        (name,{ Name = name; Pointer = { Pointer = k}}) )                          
                    kernels
                do 
                    dict.Add(k,v)
            dict

        kernels

    let kernels = new Dictionary<_,_>() 
    let _ = Array.iter (fun k -> kernels.[k] <- kernelsByDim k) [|2;3|]

    let queue = checkErrPtr (fun errPtr -> API.CreateCommandQueue(context,device,CLEnum.QueueOutOfOrderExecModeEnable,errPtr))    
    
    let _ = ErrorMsg.Logger.Debug "GPU initialized" 

    let numThreadsLock = Lock()
    let mutable numInternalThreads = 0
    let mutable numExternalThreads = 0

    let checkStarvation k = job {        
        let b = GPUMemory.NumBlockedThreads()
        // ErrorMsg.Logger.Debug <| sprintf "CHECK STARVATION: internal: %A external: %A blocked %A surplus: %A" numInternalThreads numExternalThreads b k
        if numInternalThreads = 0 && b > 0 && numExternalThreads - k - b <= 0 then
            raise OutOfGPUMemoryException
    }

    let _ = GPUMemory.OnWait (Lock.duringJob numThreadsLock <| checkStarvation 1)

    member __.SignalNumExternalThreads n =        
        Lock.duringJob numThreadsLock <| job {
            numExternalThreads <- n
            do! checkStarvation 0
        }

    // member __.SetDimensionIndex x =
    //     dimensionIndex <- x

    member __.Float32 (f : float32) =
        GPUFloat(f) :> GPUValue<float32>

    member __.NewArrayOnDevice<'a when 'a : unmanaged> (length : int) =
        let ptr = checkErrPtr <| fun p -> API.CreateBuffer(context,CLEnum.MemReadWrite,unativeint (length * sizeof<'a>),vNullPtr,p)
        new GPUArray<'a>(ptr,length,{ Pointer = queue }) :> GPUValue<array<'a>>

    member __.CopyArrayToDevice (v : array<'a>) =
        use vptr' = fixed v
        let vptr = NativePtr.toVoidPtr vptr'
        let ptr = checkErrPtr <| fun p -> API.CreateBuffer(context,enum<CLEnum>(32),unativeint (v.Length * sizeof<'a>),vptr,p) // 32 -> TODO: UseHostPointer
        new GPUArray<'a>(ptr,v.Length,{ Pointer = queue }) :> GPUValue<array<'a>>
    
    member __.NewImageOnDevice (img: VoxImage,nComponents,bufferType) =
        let dimension =            
            match img.Dimension with 
            | 2 -> CLEnum.MemObjectImage2D
            | 3 -> CLEnum.MemObjectImage3D
            | d -> raise <| UnsupportedImageDimensionException d
        
        let channelOrder =            
            match nComponents  with
            | 1 -> CLEnum.R
            // | 2 -> CLEnum.RG TODO: handle the 2-components case and use it in connected components and dt kernels in the 2D
            | 4 -> CLEnum.Rgba
            | x -> raise <| UnsupportedNumberOfComponentsPerPixelException x

        let channelDataType =
            match bufferType with
            | UInt8 -> CLEnum.UnsignedInt8
            | Float32 -> CLEnum.Float                    
            
        let imgFormatOUT = ImageFormat(uint32 channelOrder,uint32 channelDataType)
        
        use imgFormatOUTPtr' = fixed [|imgFormatOUT|]
        let imgFormatOUTPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgFormatOUTPtr') 

        let width = img.Width
        let height = img.Height
        let depth = img.Depth

        let imgDesc = ImageDesc(uint32 dimension,unativeint width,unativeint height,unativeint depth,0un,0un,0un,0ul,0ul)
        use imgDescPtr' = fixed [|imgDesc|]
        let imgDescPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgDescPtr')

        let memoryKey = Image { 
                NComponents = nComponents
                BufferType = bufferType
                Dimension = img.Dimension
                Width = img.Width
                Height = img.Height
                Depth = img.Depth
            }
        
        job {

                let! p =
                    let c = 
                        lock imageCount (fun () ->
                            if imageCount.ContainsKey memoryKey then 
                                imageCount.[memoryKey] 
                            else 
                                imageCount.[memoryKey] <- 0
                                0)                
                    // The above should be:
                    //     try imageCount.[memoryKey] // TODO: this breaks memory, report this fsharp bug
                    //     with _ ->
                    //         imageCount.[memoryKey] <- 0
                    //         0         

                    if c < 40 then // TODO: URGENT: PROVISIONAL
                        ErrorMsg.Logger.DebugOnly <| sprintf "ALLOC %A %A" nComponents bufferType
                        lock imageCount (fun () -> imageCount.[memoryKey] <- c + 1)
                        Job.result <| 
                            checkErrPtr (fun p -> 
                                API.CreateImage(
                                    context,
                                    CLEnum.MemReadWrite,
                                    imgFormatOUTPtr,
                                    imgDescPtr,
                                    vNullPtr,
                                    p))
                    else
                        ErrorMsg.Logger.DebugOnly <| sprintf "WAIT %A %A" nComponents bufferType
                        GPUMemory.Wait memoryKey                   
                
                return GPUImage(p,img,nComponents,bufferType,{ Pointer = queue }) :> GPUValue<VoxImage>
        }
        

    member this.CopyImageToDevice (hImgSource: VoxImage) =         
        // let dimension =            
        //     match hImgSource.Dimension with 
        //     | 2 -> CLEnum.MemObjectImage2D
        //     | 3 -> CLEnum.MemObjectImage3D
        //     | d -> raise <| UnsupportedImageDimensionException(hImgSource.Dimension)
        
        // let channelOrder =
        //     match hImgSource.NComponents with
        //     | 1 -> CLEnum.Intensity
        //     | 4 -> CLEnum.Rgba 
        //     | c -> raise <| UnsupportedNumberOfComponentsPerPixelException c

        // let channelDataType =
        //     match hImgSource.BufferType with
        //     | UInt8 -> CLEnum.UnsignedInt8
        //     | Float32 -> CLEnum.Float                
        
        // let imgFormat = ImageFormat(uint32 channelOrder,uint32 channelDataType)   
        // use imgFormatPtr' = fixed [|imgFormat|]
        // let imgFormatPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgFormatPtr') 
        
        // let (width,height,depth) = hImgSource.Size.[0],hImgSource.Size.[1],if hImgSource.Size.Length >= 3 then hImgSource.Size.[2] else 1
        // let imgDesc = ImageDesc(uint32 dimension,unativeint width,unativeint height,unativeint depth,0un,0un,0un,0ul,0ul)
        // use imgDescPtr' = fixed [|imgDesc|]
        // let imgDescPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgDescPtr')
        
        // let ptr =
        //     hImgSource.GetBufferAsFloat
        //         (fun buf -> 
        //             let imgPtr = NativePtr.toVoidPtr buf.Pointer            
        //             checkErrPtr (fun p -> 
        //                 API.CreateImage(
        //                         context,
        //                         //CLEnum.MemUseHostPtr,
        //                         enum<CLEnum>(32), 
        //                         // 32 = UseHostPointer
        //                         // change the numeric constant to a symbolic one once https://github.com/dotnet/Silk.NET/issues/428 makes it to release (10th of April?)
        //                         imgFormatPtr,
        //                         imgDescPtr,
        //                         imgPtr,
        //                         p)))

        use startPtr = fixed [|0un;0un;0un|]
        use endPtr = fixed [|unativeint hImgSource.Size.[0];unativeint hImgSource.Size.[1];unativeint (if hImgSource.Size.Length >= 3 then hImgSource.Size.[2] else 1)|]            
        
        job {
            let! res = this.NewImageOnDevice(hImgSource,hImgSource.NComponents,hImgSource.BufferType)     
            let dataPointer = 
                match res.Value with
                | Buffer buf -> buf.DataPointer
                | _ -> failwith "Internal error in module GPU.fs. This should not happen, please report."

            match hImgSource.BufferType with
            | UInt8 ->
                hImgSource.GetBufferAsUInt8
                    (fun buf -> 
                        let ptr = NativePtr.toVoidPtr buf.Pointer
                        checkErr <| API.EnqueueWriteImage(queue,dataPointer,true,startPtr,endPtr,0un,0un,ptr,0ul,nullPtr,nullPtr))                            
            | Float32 ->
                hImgSource.GetBufferAsFloat
                    (fun buf -> 
                        let ptr = NativePtr.toVoidPtr buf.Pointer
                        checkErr <| API.EnqueueWriteImage(
                                        queue,
                                        dataPointer,
                                        true,
                                        startPtr,
                                        endPtr,
                                        0un,
                                        0un,
                                        ptr,
                                        0ul,
                                        nullPtr,
                                        nullPtr))

     
            return res
        }
  
    member this.Run (dimension : int,kernelName : string,events : array<Event>,args : seq<KernelArg>, globalWorkSize : array<int>,oLocalWorkSize : Option<array<int>>) =  
        job {
            ErrorMsg.Logger.DebugOnly <| sprintf "Gpu.Run kernelname %A %A" kernelName args
            do! Lock.duringFun numThreadsLock (fun () -> numInternalThreads <- numInternalThreads + 1)
            // ErrorMsg.Logger.Debug <| sprintf "start %A" kernelName
            let args = Seq.cache args
            do! Job.seqIgnore (Seq.map (fun (arg : KernelArg) -> arg.Reference()) (Seq.distinct args) )
            let res =
                lock mutex (fun () -> 
                    let kernel = kernels.[dimension].[kernelName].Pointer
                    let args' = Seq.zip (Seq.initInfinite id) args
                    let mutable dimIdx = 0
                    let events = Array.map (fun (x : Event) -> x.EventPointer) events
                    for (idx,arg) in args' do
                        dimIdx <- idx   
                        match arg.Value with
                        | Buffer d -> 
                            use a' = fixed [| d.DataPointer |] 
                            let a = NativePtr.toVoidPtr a'
                            checkErr <| API.SetKernelArg(kernel.Pointer,uint32 idx,unativeint sizeof<nativeint>,a)        
                        | Float f -> 
                            use a' = fixed [| f |]
                            let a = NativePtr.toVoidPtr a'
                            checkErr <| API.SetKernelArg(kernel.Pointer,uint32 idx,unativeint sizeof<float32>,a)          
                    use globalWorkSize' = fixed (Array.map unativeint globalWorkSize)
                    let event = [|0n|]
                    use event' = fixed event
                    use events'' = fixed events
                    let events' = if events.Length > 0 then events'' else nullPtr
                    let fn (localWorkSize' : nativeptr<unativeint>) = 
                        checkErr <|                 
                            API.EnqueueNdrangeKernel(queue,kernel.Pointer,uint32 globalWorkSize.Length,uNullPtr,globalWorkSize',localWorkSize',uint32 events.Length,events',event')                
                    match oLocalWorkSize with
                    | None -> fn uNullPtr
                    | Some localWorkSize ->
                        assert (globalWorkSize.Length = localWorkSize.Length)
                        use localWorkSize' = fixed (Array.map unativeint localWorkSize)            
                        fn localWorkSize'
                    { EventPointer = event.[0] }                    
                )
            let! _ = Job.queue <| job {                
                this.Wait [|res|]     
                ErrorMsg.Logger.DebugOnly <| sprintf "Gpu.Run finished %A" kernelName
                do! Job.seqIgnore (Seq.map (fun (arg : KernelArg) -> arg.Dereference()) (Seq.distinct args) )
                do! Lock.duringJob numThreadsLock <| job {
                    numInternalThreads <- numInternalThreads - 1
                    do! checkStarvation 0
                }
            }
            // ErrorMsg.Logger.Debug <| sprintf "exit %A" kernelName
            return res
        }

    // member this.Run(kernelName : string, events : array<Event>, variadic : seq<GPUValue<_>>, globalWorkSize : array<int>, localWorkSize : Option<array<int>>) = this.Run(kernelName,events, Seq.map (fun x -> x :> KernelArg) variadic, globalWorkSize,localWorkSize)

    // member this.Run(kernelName,events,argument,globalWorkSize,localWorkSize) = this.Run(kernelName,events,seq {argument :> KernelArg}, None, globalWorkSize,localWorkSize)

    // member this.Run(kernelName,events,argument1,argument2,globalWorkSize,localWorkSize) = this.Run(kernelName,events,seq {argument1 :> KernelArg; argument2 :> KernelArg}, globalWorkSize,localWorkSize)

    // member this.Run(kernelName,events,argument1,argument2,argument3,globalWorkSize,localWorkSize) = 
    //     this.Run(kernelName,events,seq {argument1 :> KernelArg; argument2 :> KernelArg; argument3 :> KernelArg}, None, globalWorkSize,localWorkSize)

    // member this.Run(kernelName,events,argument1,argument2,argument3,argument4,globalWorkSize,localWorkSize) = 
    //     this.Run(kernelName,events,seq {argument1 :> KernelArg; argument2 :> KernelArg; argument3 :> KernelArg; argument4 :> KernelArg}, globalWorkSize,localWorkSize)

    // member this.Run(kernelName,events,argument1,argument2,argument3,argument4,argument5,globalWorkSize,localWorkSize) = 
    //     this.Run(kernelName,events,seq {argument1 :> KernelArg; argument2 :> KernelArg; argument3 :> KernelArg; argument4 :> KernelArg; argument5 :> KernelArg}, globalWorkSize,localWorkSize)

    member __.Wait(events : array<Event>) =
        if events.Length > 0 then 
            let events = Array.map (fun (x : Event) -> x.EventPointer) events
            use events'' = fixed events
            let events' = if events.Length > 0 then events'' else nullPtr
            checkErr <| API.WaitForEvents(uint32 events.Length,events')

    member __.Finish () = checkErr <| API.Finish(queue)    
           
   
