module VoxLogicA.GPU

open CASS.OpenCL

exception GPUErrorException of err : CLError
    with override this.Message = sprintf "GPU Error: %A" this.err

let listGPUDrivers () = 
    Array.mapi
        (fun idx platform ->                             
            let version = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Version)
            let vendor = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Vendor)
            let profile = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Profile)
            let name = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Name)
            sprintf "platform: %d name: %A profile: %A vendor: %A version: %A" idx name profile vendor version)
        (OpenCL.GetPlatforms())

let initGPU platformId = 
    let platform = OpenCL.GetPlatforms().[platformId] 
    let version = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Version)
    let vendor = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Vendor)
    let profile = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Profile)
    let name = OpenCL.GetPlatformInfo(platform,CLPlatformInfo.Name)
    ErrorMsg.Logger.Debug <| sprintf "Selected OpenCL platform: %d name: %A profile: %A vendor: %A version: %A" platformId name profile vendor version
    let err = ref CLError.Success
    let context = OpenCLDriver.clCreateContext([|nativeint CLContextProperties.Platform;nativeint platform.Value|],1ul,OpenCL.GetDevices(platform),null,nativeint 0,err)
    if !err = CLError.Success then
        context
    else raise <| GPUErrorException !err
