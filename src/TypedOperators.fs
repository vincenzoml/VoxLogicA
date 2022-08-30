module VoxLogicA.TypedOperators

open itk.simple

type TypedOperators() = 
    let multiplyImages (img1 : Image) (img2 : Image) =
        SimpleITK.Multiply(img1, img2)
    
    let multiplyImageValue (img : Image) (v : float) =
        SimpleITK.Multiply(v, img)
        
    static member ImgToFloat (img : Image) =
        SimpleITK.Cast(img, PixelIDValueEnum.sitkFloat32)