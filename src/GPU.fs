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

exception UnsupportedImageDimensionException of i : int
    with override this.Message = sprintf "Unsupported image dimension: %d (only 2D and 3D images are supported by the GPU implementation)" this.i

type Kernel = 
    {   Name : string
        Pointer : nativeint     }    

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

type KernelArgument = 
    private { 
        pointer : nativeint 
        dimension : int
        components : int
        pixeltype : PixelType
        width : int
        height : int
        depth : int        
    }

    override this.ToString() = sprintf "<KernelArgument %d>" this.pointer
        

type GPUValue<'a> =
    abstract member ToHost : unit -> 'a  
    abstract member ToDevice : unit -> KernelArgument      

type GPU(kernelsFilename : string) =
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
        use streamReader = new System.IO.StreamReader(kernelsFilename)
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
    
    let queue = checkErrPtr (fun errPtr -> API.CreateCommandQueue(context,device,CLEnum.QueueOutOfOrderExecModeEnable,errPtr))    

    let _ = ErrorMsg.Logger.Debug "Initialized GPU"
    
    member this.LoadImage (filename : string) =
        this.TransferImage(new VoxImage(filename))

    member __.TransferImage (img: VoxImage) =        
        let dimension =
            let d = img.Dimension
            match d with 
            | 2 -> CLEnum.MemObjectImage2D
            | 3 -> CLEnum.MemObjectImage3D
            | _ -> raise <| UnsupportedImageDimensionException(img.Dimension)
        
        let channelOrder =
            let c = img.NComponents 
            match c with
            | 1 -> CLEnum.R
            | 4 -> CLEnum.Rgba 
            | _ -> raise <| UnsupportedNumberOfComponentsPerPixelException c

        let channelDataType =
            match img.BufferType with
            | UInt8 -> CLEnum.UnsignedInt8
            | Float32 -> CLEnum.Float                
        
        let imgFormatIN = ImageFormat(uint32 channelOrder,uint32 channelDataType)   
        use imgFormatINPtr' = fixed [|imgFormatIN|]
        let imgFormatINPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgFormatINPtr')
        
        let (width,height,depth) = img.Width,img.Height,img.Depth
        let imgDesc = new ImageDesc(uint32 dimension,unativeint width,unativeint height,unativeint depth,0un,0un,0un,0ul,0ul)
        use imgDescPtr' = fixed [|imgDesc|]
        let imgDescPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgDescPtr')
        
        let input  =             
            img.GetBufferAsFloat
                (fun buf -> 
                    let imgPtr = NativePtr.toVoidPtr buf.Pointer            
                    checkErrPtr (fun p -> 
                        API.CreateImage(
                                context,
                                enum<CLEnum>(32|||4), 
                                // UseHostPointer|||MemReadOnly 
                                // TODO: ADD READONLY AND WRITEONLY HERE AND BELOW once https://github.com/dotnet/Silk.NET/issues/428 is fixed                                
                                imgFormatINPtr,
                                imgDescPtr,
                                imgPtr,
                                p)))

        { new GPUValue<VoxImage> with
            member __.ToDevice () = { 
                        pointer = input
                        components = img.NComponents
                        dimension = img.Dimension
                        pixeltype = img.BufferType
                        width = img.Width
                        height = img.Height
                        depth = img.Depth
                    }
            member __.ToHost () = img }

    member this.AllocateImage img = this.AllocateImage (img,img.NComponents)

    member __.AllocateImage (img: VoxImage,nComponents) =
        let dimension =            
            match img.Dimension with 
            | 2 -> CLEnum.MemObjectImage2D
            | 3 -> CLEnum.MemObjectImage3D
            | d -> raise <| UnsupportedImageDimensionException d
        
        let channelOrder =            
            match nComponents  with
            | 1 -> CLEnum.R
            | 4 -> CLEnum.Rgba 
            | x -> raise <| UnsupportedNumberOfComponentsPerPixelException x

        let channelDataType =
            match img.BufferType with
            | UInt8 -> CLEnum.UnsignedInt8
            | Float32 -> CLEnum.Float                    
            
        let imgFormatOUT = ImageFormat(uint32 channelOrder,uint32 channelDataType)
        
        use imgFormatOUTPtr' = fixed [|imgFormatOUT|]
        let imgFormatOUTPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgFormatOUTPtr') 

        let width = img.Width
        let height = img.Height
        let depth = img.Depth

        let imgDesc = new ImageDesc(uint32 dimension,unativeint width,unativeint height,unativeint depth,0un,0un,0un,0ul,0ul)
        use imgDescPtr' = fixed [|imgDesc|]
        let imgDescPtr = NativePtr.ofNativeInt (NativePtr.toNativeInt imgDescPtr')
        
        let output = checkErrPtr (fun p -> API.CreateImage(context,CLEnum.MemWriteOnly,imgFormatOUTPtr,imgDescPtr,vNullPtr,p))

        { new GPUValue<VoxImage> with
            member __.ToDevice () = { 
                pointer = output
                components = img.NComponents
                dimension = img.Dimension
                pixeltype = img.BufferType
                width = img.Width
                height = img.Height
                depth = img.Depth
            }
   
            member __.ToHost () =
                printfn "%A" depth
                use startPtr = fixed [|0un;0un;0un|]
                use endPtr = fixed [|unativeint width;unativeint height; unativeint depth|]

                let create (img : VoxImage) = 
                    match img.BufferType with
                    | UInt8 -> VoxImage.CreateUInt8(img,0uy)
                    | Float32 -> VoxImage.CreateFloat(img,0f)

                let img2 = 
                    match img.NComponents with
                    | 1 -> create img
                    | 4 -> VoxImage.RGBA (create img) (create img) (create img) (create img)
                    | x -> raise <| UnsupportedNumberOfComponentsPerPixelException x

                match img2.BufferType with
                | UInt8 ->
                    img2.GetBufferAsUInt8
                        (fun buf -> 
                            let ptr = NativePtr.toVoidPtr buf.Pointer
                            checkErr <| API.EnqueueReadImage(queue,output,true,startPtr,endPtr,0un,0un,ptr,0ul,nullPtr,nullPtr))                            
                | Float32 ->
                    img2.GetBufferAsFloat
                        (fun buf -> 
                            let ptr = NativePtr.toVoidPtr buf.Pointer
                            checkErr <| API.EnqueueReadImage(queue,output,true,startPtr,endPtr,0un,0un,ptr,0ul,nullPtr,nullPtr))
                
                img2
        }
            

    member this.Test =         
        let img = new VoxImage "./three_coloured_items_RGBA.png"        
        let input = this.TransferImage img

        use i' = fixed [|input.ToDevice().pointer|]
        let i = NativePtr.toVoidPtr i'
        
        let output = this.AllocateImage(img)
        let d = output.ToDevice()
        
        let o' = fixed [|d.pointer|]
        let o = NativePtr.toVoidPtr o'        

        let kernel = kernels.["swapRG"].Pointer 
        let globalWorkSize = fixed [|unativeint d.width;unativeint d. height|]
        
        checkErr <| API.SetKernelArg(kernel,0ul,unativeint sizeof<nativeint>,i)
        checkErr <| API.SetKernelArg(kernel,1ul,unativeint sizeof<nativeint>,o)        
        
        checkErr <| API.EnqueueNdrangeKernel(queue,kernel,2ul,uNullPtr,globalWorkSize,uNullPtr,0ul,nullPtr,nullPtr)

        checkErr <| API.Finish(queue)

        let img2 = output.ToHost()
        img2.Save("output.png")  
           
   
