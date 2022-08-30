module VoxLogicA.CPUTypedOperators

open itk.simple
open VoxLogicA.ITypedOperators

type CPUTypedOperators() =
    interface ITypedOperators<Image, float, PixelIDValueEnum> with
        member this.ConvertImage (img : Image) (pixelID : PixelIDValueEnum) =
            SimpleITK.Cast(img, pixelID)
        member this.Multiply ((img1 : Image), (img2 : Image)) =
            SimpleITK.Multiply(img1, img2)
        member this.Multiply ((value : float), (img2 : Image)) =
            SimpleITK.Multiply(value, img2)

