module VoxLogicA.GPU
#nowarn "9"

open Silk.NET.Core.Native
open VoxLogicA.SITKUtil
open Silk.NET.OpenCL
open System.Collections.Generic

exception GPUException of c : int
    with override this.Message = sprintf "GPU error. Code: %d %s" this.c (System.Enum.GetName (LanguagePrimitives.EnumOfValue this.c : CLEnum))

exception GPUCompileException of s : string
    with override this.Message = sprintf "Could not compile GPU kernels:\n%s" this.s

type Kernel = 
    {   Name : string
        Pointer : nativeint     }    

type GPU() =
    let _ = ErrorMsg.Logger.Debug "Initializing GPU"
    let API = CL.GetApi()

    let nullPtr : nativeptr<nativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let uNullPtr : nativeptr<unativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let bNullPtr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt 0n
    let nbNullPtr : nativeptr<nativeptr<byte>> = NativeInterop.NativePtr.ofNativeInt 0n
    let vNullPtr : voidptr = NativeInterop.NativePtr.toVoidPtr nullPtr
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

    let deviceGroups =        
        let mutable res = [] 
        for platformID in platformIDs do
            let mutable num = 0ul            
            use ptr = fixed [||]           
            if 0 = API.GetDeviceIDs(platformID,CLEnum.DeviceTypeGpu,0ul,ptr,&num) then
                if num > 0ul then
                    let tmp = Array.create (int num) 0n
                    use ptr = fixed tmp
                    if 0 = API.GetDeviceIDs(platformID,CLEnum.DeviceTypeGpu,uint32 num,ptr,&num) then                    
                        res <- (platformID,tmp)::res
        match res with 
        | [] -> raise <| GPUException 0 // TODO: error code for unknown error?
        | _ -> res    

    let (_,devices) = List.head deviceGroups // TODO: this blindly selects the first platform         

    let (device,context) = 
        // TODO: the returned "device" is needed to create the command queue, but I don't understand why an array of devices can be passed to CreateContext and only one device to CreateCommandQueue        
        use devicesPtr = fixed devices
        checkErrPtr (fun errPtr -> (devices.[0],API.CreateContext(nullPtr,1ul,devicesPtr,noNotify,vNullPtr,errPtr)))   // TODO: always selects first device              

    let source = 
        use streamReader = new System.IO.StreamReader(System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location) + "/kernel.cl")
        let res = streamReader.ReadToEnd()
        streamReader.Close()
        res

    let program =         
        ErrorMsg.Logger.Debug "Compiling kernels"
        let prg = checkErrPtr (fun errPtr -> API.CreateProgramWithSource(context,1ul,[|source|],uNullPtr,errPtr))                
        let res = API.CompileProgram(prg,0ul,nullPtr,bNullPtr,0ul,nullPtr,nbNullPtr,noNotify,vNullPtr) 
        if res = int CLEnum.CompileProgramFailure then        
            let param_name : uint32 = uint32 CLEnum.ProgramBuildLog            
            let mutable len = [|0un|]
            use lenPtr = fixed len                        
            checkErr (API.GetProgramBuildInfo(prg,device,param_name,0un,vNullPtr,lenPtr)) 
            let output = SilkMarshal.Allocate (int len.[0] + 1)
            let outputPtr = NativeInterop.NativePtr.toVoidPtr((NativeInterop.NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
            checkErr (API.GetProgramBuildInfo(prg,device,param_name,len.[0],outputPtr,uNullPtr)) 
            let error = SilkMarshal.PtrToString(output,NativeStringEncoding.Auto)            
            raise <| GPUCompileException error
        let res = API.BuildProgram(prg,0ul,nullPtr,bNullPtr,noNotify,vNullPtr)     
        if res = int CLEnum.BuildProgramFailure then        
            let param_name : uint32 = uint32 CLEnum.ProgramBuildLog            
            let mutable len = [|0un|]
            use lenPtr = fixed len                        
            checkErr (API.GetProgramBuildInfo(prg,device,param_name,0un,vNullPtr,lenPtr)) 
            let output = SilkMarshal.Allocate (int len.[0] + 1)
            let outputPtr = NativeInterop.NativePtr.toVoidPtr((NativeInterop.NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
            checkErr (API.GetProgramBuildInfo(prg,device,param_name,len.[0],outputPtr,uNullPtr)) 
            let error = SilkMarshal.PtrToString(output,NativeStringEncoding.Auto)            
            raise <| GPUCompileException error
        checkErr res 
        ErrorMsg.Logger.Debug "Kernels compiled"
        prg               

    let kernels = // TODO: it is possible in opencl to retrieve information on each parameter (type etc.) with a suitable option to the compiler. Use this to generate typed methods (with a typeprovider?)
        // checkErr (fun p -> API.CreateKernel(program,"intdensity",p))
        let num = [|0ul|]
        use numPtr = fixed num
        checkErr (API.CreateKernelsInProgram(program,0ul,nullPtr,numPtr))        
        let kv = Array.create (int num.[0]) 0n        
        use kvPtr = fixed kv
        checkErr (API.CreateKernelsInProgram(program,num.[0],kvPtr,numPtr))
        let kernels = kv.[0..(int num.[0]-1)]
        //Map.ofSeq <|
        let dict = Dictionary<_,_>()
        for (k,v) in
            Array.map 
                (fun k -> 
                    let len = [|0un|]
                    use lenPtr = fixed len
                    let s x = failwith "" 
                    //API.GetKernelInfo(k,uint32 CLEnum.KernelFunctionName,0un,nullPtr,sizePtr))
                    checkErr <| API.GetKernelInfo(k,uint32 CLEnum.KernelFunctionName,0un,vNullPtr,lenPtr) 
                    let name = SilkMarshal.Allocate (int len.[0] + 1)
                    let namePtr = NativeInterop.NativePtr.toVoidPtr((NativeInterop.NativePtr.ofNativeInt name : nativeptr<int>)) : voidptr
                    checkErr <| API.GetKernelInfo(k,uint32 CLEnum.KernelFunctionName,len.[0],namePtr,uNullPtr) 
                    let name = SilkMarshal.PtrToString(name,NativeStringEncoding.Auto)
                    (name,{ Name = name; Pointer = k}) )                          
                kernels
            do 
                dict.Add(k,v)
        dict
    
    let queue = 
        checkErrPtr (fun errPtr -> API.CreateCommandQueue(context,device,CLEnum.QueueOutOfOrderExecModeEnable,errPtr))    

    let _ = ErrorMsg.Logger.Debug "GPU Initialized"
        
    member __.Test =         
        let img = new VoxImage("three_coloured_items_RGBA.png")
        
        let s() = failwith "stub"
        let imgFormatIN = ImageFormat(uint32 CLEnum.Rgba,uint32 CLEnum.UnsignedInt8)   
        let imgFormatOUT = ImageFormat(uint32 CLEnum.R,uint32 CLEnum.UnsignedInt8)   
        use imgFormatOUTPtr' = fixed [|imgFormatOUT|]
        let imgFormatOUTPtr = NativeInterop.NativePtr.ofNativeInt (NativeInterop.NativePtr.toNativeInt imgFormatOUTPtr') 
        use imgFormatINPtr' = fixed [|imgFormatIN|]
        let imgFormatINPtr = NativeInterop.NativePtr.ofNativeInt (NativeInterop.NativePtr.toNativeInt imgFormatINPtr')
        let (width,height) = img.Width,img.Height
        let imgDesc = new ImageDesc(uint32 CLEnum.MemObjectImage2D,unativeint width,unativeint height,0un,0un,0un,0un,0ul,0ul)
        use imgDescPtr' = fixed [|imgDesc|]
        let imgDescPtr = NativeInterop.NativePtr.ofNativeInt (NativeInterop.NativePtr.toNativeInt imgDescPtr')
        let input : nativeptr<unativeint> =             
            NativeInterop.NativePtr.ofNativeInt <|            
            img.GetBufferAsUInt32
                (fun buf -> 
                    let imgPtr = NativeInterop.NativePtr.toVoidPtr buf.Pointer            
                    checkErrPtr (fun p -> 
                        API.CreateImage(
                                context,
                                enum<CLEnum>(32|||4), // UseHostPointer|||MemReadOnly TODO: ADD READONLY AND WRITEONLY HERE AND BELOW once https://github.com/dotnet/Silk.NET/issues/428 is fixed
                                // SEE https://discord.com/channels/521092042781229087/607634593201520651/822107881591144488
                                imgFormatINPtr,
                                imgDescPtr,
                                imgPtr,
                                p)))
        let output : nativeptr<unativeint> = NativeInterop.NativePtr.ofNativeInt <| checkErrPtr (fun p -> API.CreateImage(context,CLEnum.MemWriteOnly,imgFormatOUTPtr,imgDescPtr,vNullPtr,p))
        let kernel = kernels.["intensity"].Pointer
        let i = NativeInterop.NativePtr.toVoidPtr input
        let o = NativeInterop.NativePtr.toVoidPtr output
        checkErr <| API.SetKernelArg(kernel,0ul,unativeint sizeof<voidptr>,i)
        printfn "0"
        checkErr <| API.SetKernelArg(kernel,1ul,unativeint sizeof<voidptr>,o)
        printfn "1"
        checkErr <| API.EnqueueTask(queue,kernel,0ul,nullPtr,nullPtr)
        printfn "2"
        checkErr <| API.Finish(queue)
        printfn "3"
        let img2 = new VoxImage(img)
        img2.GetBufferAsUInt32
            (fun buf -> 
                let ptr = NativeInterop.NativePtr.toVoidPtr buf.Pointer
                let size = unativeint <| img2.NPixels * img2.NComponents // TODO: implement NBytes
                let clbuf = checkErrPtr <| fun p -> API.CreateBuffer(context,CLEnum.MemUseHostPtr,size,ptr,p)
                checkErr <| API.EnqueueReadBuffer(queue,clbuf,true,0un,size,ptr,0ul,nullPtr,nullPtr))
        img2.Save("output.png")
        
        
        "All done"
        
