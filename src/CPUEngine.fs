module VoxLogicA.CPUEngine

open VoxLogicA.Interpreter
open VoxLogicA.Resources
open VoxLogicA.Reducer
open itk.simple

// This engine implements a load function, used to load an image from disk

type PixelType = 
    | Float 
    | UInt8 
    | UInt16 
    | UInt32
    static member OfSITK (t : PixelIDValueEnum) =
        match t with
        | _ when t = PixelIDValueEnum.sitkFloat32 || t = PixelIDValueEnum.sitkVectorFloat32-> Float
        | _ when t = PixelIDValueEnum.sitkUInt8 || t = PixelIDValueEnum.sitkVectorUInt8 -> UInt8
        | _ when t = PixelIDValueEnum.sitkUInt16 || t = PixelIDValueEnum.sitkVectorUInt16-> UInt16
        | _ when t = PixelIDValueEnum.sitkUInt32 || t = PixelIDValueEnum.sitkVectorUInt32-> UInt32
        | _ -> ErrorMsg.fail $"Cannot handle pixel type {t}"
    

type ImgKind = 
    {
        dimensions : array<int>
        pixelType : PixelType
        channels : int
    }
    member this.GetSTIKPixelID() =
        match this.pixelType,this.channels with
        | Float,1 -> PixelIDValueEnum.sitkFloat32
        | UInt8,1 -> PixelIDValueEnum.sitkUInt8
        | Float,_ -> PixelIDValueEnum.sitkVectorFloat32
        | UInt8,_ -> PixelIDValueEnum.sitkVectorUInt8
        | _ -> ErrorMsg.fail "THIS IS A STUB"

type CPUResourceKind = KImg of ImgKind | KNumber | KString | KBool

type CPUResource = 
    | CPUImg of Image 
    | CPUNumber of float 
    | CPUString of string 
    | CPUBool of bool

    static member Allocator kind =
        match kind with
        | KImg imgknd -> 
            let pixtype = imgknd.GetSTIKPixelID()
            use sz = new VectorUInt32(imgknd.dimensions)
            Some (CPUImg (new Image(sz, pixtype, uint32 imgknd.channels)))
        | _ -> ErrorMsg.fail "Internal error in CPU resource allocation"

type CPUEngine() =
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
            | Bool b -> 
                OperatorImplementation(Requirements<CPUResourceKind>[],
                    (fun _ _ ->
                        task {
                            return (Resource (CPUBool b, KBool))
                        })
                )
            | Identifier "load" ->
                OperatorImplementation(Requirements<CPUResourceKind>[],
                    (fun _ args ->
                        task {
                            match args[0].Value with
                            | CPUString str ->
                                let img = SimpleITK.ReadImage(str)
                                let sz = Array.ofSeq <| Seq.map int (img.GetSize())
                                let pixtype = PixelType.OfSITK (img.GetPixelID())                                
                                let chs = int <| img.GetNumberOfComponentsPerPixel()
                                let record = {
                                    dimensions = sz
                                    pixelType = pixtype
                                    channels = chs
                                }
                                return (Resource (CPUImg img, KImg record))                            
                        }
                    )
                )
            | Identifier "geq" ->
                OperatorImplementation(Requirements(
                    [ ("result", KImg {
                        dimensions = [| 0;0;0|]
                        pixelType = UInt8
                        channels = 3
                    }) ]
                ), 
                (fun resources args ->
                    task {
                        match args[1].Value with
                        | CPUImg baseImg ->
                            match args[0].Value with
                                CPUNumber f ->
                                    use flt = new GreaterEqualImageFilter()
                                    let img = new Image(flt.Execute(baseImg,f))
                                    let sz = Array.ofSeq <| Seq.map int (img.GetSize())
                                    let pixtype = PixelType.OfSITK (img.GetPixelID())                                
                                    let chs = int <| img.GetNumberOfComponentsPerPixel()
                                    let record = {
                                        dimensions = sz
                                        pixelType = pixtype
                                        channels = chs
                                    }
                                    return (Resource (CPUImg img, KImg record))
                    })
                )
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"
