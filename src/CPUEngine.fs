module VoxLogicA.CPUEngine

open VoxLogicA.Interpreter
open VoxLogicA.Resources
open VoxLogicA.Reducer
open itk.simple

// This engine implements a load function, used to load an image from disk

type PixelType = Float | Int8 | Int16

type ImgKind = {
    dimensions : VectorUInt32
    pixelType : PixelType
    channels : uint32
}

type CPUResourceKind = KImg of ImgKind | KNumber | KString | KBool

type CPUResource = 
    | CPUImg of Image 
    | CPUNumber of float 
    | CPUString of string 
    | CPUBool of bool

    static member Allocator kind =
        match kind with
        | KImg imgknd -> 
            let pixtype = 
                match imgknd.pixelType with
                | Float -> PixelIDValueEnum.sitkFloat32
                | Int8 -> PixelIDValueEnum.sitkInt8
                | Int16 -> PixelIDValueEnum.sitkInt16
            use sz = imgknd.dimensions
            Some (CPUImg (new Image(sz, pixtype, uint32 imgknd.channels)))
        | _ -> ErrorMsg.fail "Internal error in CPU resource allocation"
    
    static member GetImg kind = 
        match kind with
        | CPUImg img -> img
        | _ -> ErrorMsg.fail "This is not an image!"


type CPUTask() =
    interface ExecutionEngine<CPUResource, CPUResourceKind> with
        member __.ImplementationOf s =
            match s with
            | Number n ->
                OperatorImplementation(Requirements<CPUResourceKind>[],
                    (fun _ _ ->
                        task {
                            return (Resource (CPUNumber n, KNumber))
                        })
                )
            | String str -> 
                OperatorImplementation(Requirements<CPUResourceKind>[],
                    (fun _ _ ->
                        task {
                            return (Resource (CPUString str, KString))
                        })
                )
            | Identifier "load" ->
                OperatorImplementation(Requirements<CPUResourceKind>[],
                    (fun _ args ->
                        task {
                            printfn "starting load"
                            match args[0].Value with
                            | CPUString str ->
                                let img = SimpleITK.ReadImage(str)
                                printfn "Image loaded"
                                let sz = img.GetSize()
                                let pixId = img.GetPixelID()
                                let pixtype =
                                    if pixId = PixelIDValueEnum.sitkFloat32 then Float
                                    else if pixId = PixelIDValueEnum.sitkInt8 then Int8
                                    else if pixId = PixelIDValueEnum.sitkInt16 then Int16
                                    else ErrorMsg.fail "Not a supported image type"
                                printfn "got infos"
                                let chs = img.GetNumberOfComponentsPerPixel()
                                let record = {
                                    dimensions = sz
                                    pixelType = pixtype
                                    channels = chs
                                }
                                printfn "returning"
                                return (Resource (CPUImg img, KImg record))
                            | _ -> return (Resource (CPUImg (new Image()), KImg {
                                    dimensions = new VectorUInt32([|0,0,0|])
                                    pixelType = Int8
                                    channels = uint32 3
                            }))
                        }
                    )
                )
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"
