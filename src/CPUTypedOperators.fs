module VoxLogicA.CPUTypedOperators

open itk.simple
open VoxLogicA.ITypedOperators

type CPUTypedOperators() =
    interface ITypedOperators with
        member this.ConvertImage (img : Image) (pixelID : PixelIDValueEnum) =
            SimpleITK.Cast(img, pixelID)