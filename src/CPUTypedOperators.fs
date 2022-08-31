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
        let chs = int <| img.GetNumberOfComponentsPerPixel()
        let cimg = 
            if chs <> 1 then
                use r = SimpleITK.VectorIndexSelectionCast(img,0ul)
                use g = SimpleITK.VectorIndexSelectionCast(img,1ul) 
                use b = SimpleITK.VectorIndexSelectionCast(img,2ul)
                let cimg = SimpleITK.Add(r,SimpleITK.Add(g,b))
                cimg
            else
                img
        SimpleITK.Cast(cimg, pixelID)
        
    member this.Multiply((img1 : Image), (img2 : Image)) =
        (this :> ITypedOperators<Image, float>).Multiply(img1, img2)

    member this.Multiply((f : float), (img : Image)) =
        (this :> ITypedOperators<Image, float>).Multiply(img, f)
