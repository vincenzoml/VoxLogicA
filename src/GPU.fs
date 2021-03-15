module VoxLogicA.GPU

#nowarn "9"

open Silk.NET.OpenCL
open Silk.NET.OpenCL
exception NoAvailableGPUException
    with override this.Message = sprintf "Could not initialize GPU"

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
        | [] -> raise NoAvailableGPUException
        | _ -> res    

    let nullPtr : nativeptr<nativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let uNullPtr : nativeptr<unativeint> = NativeInterop.NativePtr.ofNativeInt 0n
    let bNullPtr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt 0n
    let nbNullPtr : nativeptr<nativeptr<byte>> = NativeInterop.NativePtr.ofNativeInt 0n
    let vNullPtr : voidptr = NativeInterop.NativePtr.toVoidPtr nullPtr
    let noNotify = new Silk.NET.OpenCL.NotifyCallback(fun _ _ _ _ -> ())

    let err = [|0|]
    let checkErr f =
        use errPtr = fixed err
        let res = f errPtr
        if err.[0] = 0 then res 
        else raise NoAvailableGPUException
    
    let (_,devices) = List.head deviceGroups // TODO: this blindly selects the first platform         

    let (device,context) = 
        // TODO: the returned "device" is needed to create the command queue, but I don't understand why an array of devices can be passed to CreateContext and only one device to CreateCommandQueue        
        use devicesPtr = fixed devices
        let numDevs = uint32 devices.Length
        checkErr (fun errPtr -> (devices.[0],API.CreateContext(nullPtr,numDevs,devicesPtr,noNotify,vNullPtr,errPtr)))            

    let queue = 
        let err = [|0|]
        checkErr (fun errPtr -> API.CreateCommandQueue(context,device,CLEnum.QueueOutOfOrderExecModeEnable,errPtr))        

    let source = 
        use streamReader = new System.IO.StreamReader(System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location) + "/kernel.cl")
        let res = streamReader.ReadToEnd()
        streamReader.Close()
        res

    let program = 
        let tmp = [|source|]
        checkErr (fun errPtr -> API.CreateProgramWithSource(context,1ul,tmp,uNullPtr,errPtr))        
        
    let binary =
        use devicesPtr = fixed devices        
        let res = API.CompileProgram(program,uint32 devices.Length,devicesPtr,bNullPtr,0ul,nullPtr,nbNullPtr,noNotify,vNullPtr)
        if res = int CLEnum.CompileProgramFailure then        
            let program : nativeint = program
            let device : nativeint = device
            let param_name : uint32 = uint32 CLEnum.ProgramBuildLog
            let output = Array.create 10000 ' '  
            let param_value_size = unativeint output.Length          
            use ptr = fixed output
            let param_value = NativeInterop.NativePtr.toVoidPtr ptr
            let pvsr = [|0un|]
            use ptr2 = fixed pvsr
            let param_value_size_ret: nativeptr<unativeint> = ptr2
            ignore <| API.GetProgramBuildInfo(program,device,param_name,param_value_size,param_value,param_value_size_ret)            
            printfn "%d %s" pvsr.[0] <| System.String(Array.sub output 0 (int pvsr.[0])) // TODO: don't ignore

    member __.Test = queue
