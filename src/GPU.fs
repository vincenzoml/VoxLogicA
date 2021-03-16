module VoxLogicA.GPU
#nowarn "9"

open Silk.NET.OpenCL
open Silk.NET.Core.Native
open VoxLogicA.SITKUtil

exception NoAvailableGPUException of c : int
    with override this.Message = sprintf "Could not initialize GPU; error code: %d %s" this.c (System.Enum.GetName (LanguagePrimitives.EnumOfValue this.c : CLEnum))

exception GPUCompileException of s : string
    with override this.Message = sprintf "Could not compile GPU kernels:\n%s" this.s

type GPU() =
    let API = CL.GetApi()

    let platformIDs =
        let mutable num = 0ul
        do
            use ptr = fixed [||]        
            ignore <| API.GetPlatformIDs (uint32 0,ptr,&num)            
        let platformIDs = Array.create (int num) 0n
        do
            use ptr = fixed platformIDs
            ignore <| API.GetPlatformIDs (uint32 num,ptr,&num)  // TODO don't ignore
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
        | [] -> raise <| NoAvailableGPUException 0 // TODO: error code for unknown error?
        | _ -> res    

    let nullPtr : nativeptr<nativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let uNullPtr : nativeptr<unativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let bNullPtr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt 0n
    let nbNullPtr : nativeptr<nativeptr<byte>> = NativeInterop.NativePtr.ofNativeInt 0n
    let vNullPtr : voidptr = NativeInterop.NativePtr.toVoidPtr nullPtr
    let noNotify = new Silk.NET.OpenCL.NotifyCallback(fun _ _ _ _ -> ())

    let checkErr code =
        if code = int CLEnum.Success then ()
        else raise <| NoAvailableGPUException code
    
    let checkErrPtr f =
        let err = [|0|]
        use errPtr = fixed err
        let res = f errPtr
        let code = err.[0]
        checkErr code
        res

    let (_,devices) = List.head deviceGroups // TODO: this blindly selects the first platform         

    let (device,context) = 
        // TODO: the returned "device" is needed to create the command queue, but I don't understand why an array of devices can be passed to CreateContext and only one device to CreateCommandQueue        
        use devicesPtr = fixed devices
        let numDevs = uint32 devices.Length
        checkErrPtr (fun errPtr -> (devices.[0],API.CreateContext(nullPtr,numDevs,devicesPtr,noNotify,vNullPtr,errPtr)))            

    let queue = 
        checkErrPtr (fun errPtr -> API.CreateCommandQueue(context,device,CLEnum.QueueOutOfOrderExecModeEnable,errPtr))        

    let source = 
        use streamReader = new System.IO.StreamReader(System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location) + "/kernel.cl")
        let res = streamReader.ReadToEnd()
        streamReader.Close()
        res

    let program =         
        // let lens = [|unativeint source.Length|]
        // let len = fixed lens
        checkErrPtr (fun errPtr -> API.CreateProgramWithSource(context,1ul,[|source|],uNullPtr,errPtr))        
        
    let binary =
        use devicesPtr = fixed devices        
        let res = API.CompileProgram(program,uint32 devices.Length,devicesPtr,bNullPtr,0ul,nullPtr,nbNullPtr,noNotify,vNullPtr)
        if res = int CLEnum.CompileProgramFailure then        
            let param_name : uint32 = uint32 CLEnum.ProgramBuildLog            
            let mutable len = [|0un|]
            use ptr2 = fixed len                        
            ignore (API.GetProgramBuildInfo(program,device,param_name,0un,vNullPtr,ptr2)) // TODO: why ignore
            let output = SilkMarshal.Allocate (int len.[0] + 1)
            let param_value = NativeInterop.NativePtr.toVoidPtr((NativeInterop.NativePtr.ofNativeInt output : nativeptr<int>)) : voidptr
            ignore (API.GetProgramBuildInfo(program,device,param_name,len.[0],param_value,ptr2)) // TODO: why ignore
            let error = SilkMarshal.PtrToString(output,NativeStringEncoding.Auto)            
            raise <| GPUCompileException error
        checkErr res        

    let kernels =
        // checkErr (fun p -> API.CreateKernel(program,"intdensity",p))
        let h = [|0ul|]
        use num = fixed h
        checkErr (API.CreateKernelsInProgram(program,0ul,nullPtr,num))        
        let k = Array.create (int h.[0]) 0n        
        use kernels = fixed k
        checkErr (API.CreateKernelsInProgram(program,h.[0],kernels,num))
        k.[0..(int h.[0]-1)]
        
    member __.Test =         
        // let img = new VoxImage("../examples/tutorial/three_coloured_items.png")
        // img.GetBufferAsUInt32(fun x -> x.Pointer)
        kernels
