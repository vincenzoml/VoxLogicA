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
        (this :> ITypedOperators<Image, float>).Multiply(img1, img2)

