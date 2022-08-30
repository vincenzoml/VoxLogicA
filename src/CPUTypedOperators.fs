module VoxLogicA.CPUTypedOperators

open itk.simple
open VoxLogicA.ITypedOperators

type CPUTypedOperator() =
    interface ITypedOperators<Image, float> with
        member this.Multiply ((img1 : Image), (img2 : Image)) =
            SimpleITK.Multiply(img1, img2)
        member this.Multiply ((img2 : Image), (value : float)) =
            SimpleITK.Multiply(value, img2)
        member this.Add ((img1 : Image), (img2 : Image)) =
            SimpleITK.Multiply(img1, img2)
        member this.Add ((img2 : Image), (value : float)) =
            SimpleITK.Multiply(value, img2)

    static member ConvertImage (img : Image, pixelID : PixelIDValueEnum) =
        SimpleITK.Cast(img, pixelID)
        
    member this.Multiply((img1 : Image), (img2 : Image)) =
        try
            (this :> ITypedOperators<Image, float>).Multiply(img1, img2)
        with e ->
            printfn "%A" e.Message
            exit -1

    member this.Multiply((f : float), (img : Image)) =
        try
            (this :> ITypedOperators<Image, float>).Multiply(img, f)
        with e ->
            printfn "%A" e.Message
            exit -1

