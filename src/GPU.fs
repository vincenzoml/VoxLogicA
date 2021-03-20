module VoxLogicA.GPU
#nowarn "9"

open Silk.NET.Core.Native
open VoxLogicA.SITKUtil
open Silk.NET.OpenCL
open System.Collections.Generic
open FSharp.NativeInterop

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
            let outputPtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
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
            let outputPtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
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
                    let namePtr = NativePtr.toVoidPtr((NativePtr.ofNativeInt name : nativeptr<int>)) : voidptr
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
        // LOAD AN IMAGE (ALWAYS CONVERTS THE BYTE VALUES TO FLOAT32)
        let img = new VoxImage("three_coloured_items_RGBA.png")
        let (width,height) = img.Width,img.Height
        let comps = img.NComponents
        let nbytes = comps * width * height * sizeof<float32>

        // SAVE IT JUST TO DEMONSTRATE THAT CONVERSION uint8->float32->uint8 is not broken
        img.Save("input.png")
        
        // ALLOCATE INPUT GLOBAL MEMORY
        let niPtrI : nativeptr<int> = GlobalMemory.Allocate(nbytes).AsPtr() // 4 because RGBA
        let vPtrI : voidptr = NativePtr.toVoidPtr (niPtrI : nativeptr<int>)
        let fPtrI : nativeptr<float32> = NativePtr.ofVoidPtr vPtrI

        // COPY IMAGE DATA TO INPUT GLOBAL MEMORY 
        img.GetBufferAsFloat(
            fun buf ->
                for i = 0 to buf.Length - 1 do
                    NativePtr.set fPtrI i (buf.UGet i)
        )

        // PREPARE IMAGE FORMAT AND IMAGE DESCRIPTOR
        let imgFormatIN = ImageFormat(uint32 CLEnum.Rgba,uint32 CLEnum.Float)   
        use imgFormatINPtr' = fixed [|imgFormatIN|]
        let imgFormatINPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgFormatINPtr')
        
        let imgDesc = new ImageDesc(uint32 CLEnum.MemObjectImage2D,unativeint width,unativeint height,0un,0un,0un,0un,0ul,0ul)
        use imgDescPtr' = fixed [|imgDesc|]
        let imgDescPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgDescPtr')
                 
        // CREATE AN IMAGE ON THE GPU COPYING DATA FROM THE INPUT GLOBAL MEMORY (flag: 32); READONLY (flag : 4)
        let input = 
            checkErrPtr (fun p -> 
                API.CreateImage(context,enum<CLEnum>(32|||4),imgFormatINPtr,imgDescPtr,vPtrI,p))

        // PREPARE REGION INFORMATION FOR COPY
        use startPtr = fixed [|0un;0un;0un|]
        use endPtr = fixed [|unativeint height;unativeint width; 1un|]

        // ALLOCATE OUTPUT GLOBAL MEMORY
        let niPtrO : nativeptr<int> = GlobalMemory.Allocate(nbytes).AsPtr() // 4 because RGBA
        let vPtrO : voidptr = NativePtr.toVoidPtr (niPtrO : nativeptr<int>)
        let fPtrO : nativeptr<float32> = NativePtr.ofVoidPtr vPtrO

        // READ IMAGE INTO OUTPUT GLOBAL MEMORY
        checkErr <| API.EnqueueReadImage(queue,input,true,startPtr,endPtr,0un,0un,vPtrO,0ul,nullPtr,nullPtr)
        checkErr <| API.Finish(queue)

        // CHECK THAT THE RESULTS ARE THE SAME
        let mutable found = false
        let mutable i = 0
        while (not found) && (i < nbytes - 1) do
            let fI = NativePtr.get fPtrI i
            let fO = NativePtr.get fPtrO i
            if fI <> fO then
                printfn "different values: %d %f %f" i fI fO                
                found <- true
            i <- i + 1

        // PREPARE A SUITABLE IMAGE OBJECT TO SAVE THE RAW DATA
        let img2 = VoxImage.RGBA (VoxImage.CreateFloat(img,0f)) (VoxImage.CreateFloat(img,0f)) (VoxImage.CreateFloat(img,255f)) (VoxImage.CreateFloat(img,255f))

        // COPY RAW DATA TO IMAGE OBJECT
        img2.GetBufferAsFloat
            (fun buf -> 
                for i = 0 to buf.Length - 1 do 
                    buf.USet i (NativePtr.get fPtrO i))
        
        // SAVE
        img2.Save("output.png")  
        