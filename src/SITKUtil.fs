// Copyright 2018 Vincenzo Ciancia.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// A copy of the license is available in the file "Apache_License.txt".
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

module VoxLogicA.SITKUtil 
open itk.simple
open System.IO
open ErrorMsg

exception UnsupportedImageTypeException of s : string
    with override this.Message = sprintf "Unsupported image type: %s" this.s

exception UnsupportedNumberOfComponentsPerPixelException of i : int
    with override this.Message = sprintf "Unsupported number of components per pixel: %d" this.i

exception UnsupportedPixelFormatException of s : string
    with override this.Message = sprintf "Unsupported pixel format: %s" this.s

exception UnsupportedImageSizeException of s : string
    with override this.Message = sprintf "More than 2 dimensions are not supported with type: %s" this.s

exception DifferentPhysicalAndLogicalSpaceException of s : string
    with override this.Message = sprintf "Image %s\ndiffers both in physical space and logical structure (number of voxels and dimensions) from previously loaded images." this.s

// TODO: review this

#nowarn "9"

open Hopac

open Microsoft.FSharp.NativeInterop

<<<<<<< HEAD
let maxImg (img : Image) =
    use flt = new MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
    flt.Execute(img)    
    flt.GetMaximum()    
    
let minImg (img : Image) =
    use flt = new MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
    flt.Execute(img)
    flt.GetMinimum()

let loadImage (filename : string) = // WARNING: the program assumes this function always returns a float32 image. Be cautious before changing this.
    Logger.Debug <| sprintf "Loading file %s" filename
    let img = SimpleITK.ReadImage(filename)
    let fname = System.IO.Path.GetFileName(filename)
    let sz = img.GetSize()
    let mutable found = false
    for i = 0 to sz.Count - 1 do
        if sz.[i] = 1u then (sz.[i] <- 0u; found <- true)
    let img = 
        if found then 
            Logger.Warning (sprintf "image %s has size 1 in some dimensions; image flattened" fname)    
            SimpleITK.Extract(img,sz) 
        else img
    Logger.DebugOnly (sprintf "Loaded image %s components per pixel: %d, pixel type: %A" fname (img.GetNumberOfComponentsPerPixel()) (img.GetPixelID()))
    match img.GetPixelID(),img.GetNumberOfComponentsPerPixel() with 
        | (x,_) when x = PixelIDValueEnum.sitkFloat32 -> img
        | (x,_) when x = PixelIDValueEnum.sitkVectorFloat32 -> img
        | (_,y) when y = 1u -> 
            Logger.DebugOnly (sprintf "image %s\ncasted to float32" fname)
            SimpleITK.Cast(img,PixelIDValueEnum.sitkFloat32)
        | (_,y) when y = 3u || y = 4u -> 
            Logger.DebugOnly (sprintf "image %s\ncasted to float32" fname)
            if y = 4u then 
                Logger.Warning <| sprintf "image %s\nhas 4 color components per voxel. Assuming RGBA color space (CMYK is not supported)." fname 
                if fname.EndsWith ".jpg" then 
                    Logger.Warning <| sprintf "image %s\nhas jpg extension and 4 components per pixel, therefore it is in CMYK color space. Only proceed if you know what you are doing. Colors and intensity of the image will be messed up in processing." fname
            SimpleITK.Cast(img,PixelIDValueEnum.sitkVectorFloat32)
        | (x,y) -> raise <| UnsupportedImageTypeException (x.ToString() + "-" + y.ToString())

let saveImage (filename : string) (img : Image) =
    let fname = System.IO.Path.GetFileName(filename)
    if fname.EndsWith(".jpg") && img.GetNumberOfComponentsPerPixel() > 3ul then
        Logger.Warning <| sprintf "Saving to %s\nusing 4 components per pixel. The resulting image will be CMYK; only proceed if you know what you are doing." fname
    let tmp = 
        if filename.EndsWith(".nii") || filename.EndsWith(".nii.gz") then img
        else 
            if filename.EndsWith(".png") || filename.EndsWith(".jpg") || filename.EndsWith ("bmp") then
                if img.GetSize().Count = 2 then 
                    if img.GetPixelID() <> PixelIDValueEnum.sitkUInt8 then
                        // TODO: double-check that "nearest integer" in the message below is correct
                        Logger.Warning (sprintf "saving to %s\nrequires cast to uint8. For each component, only values between 0 and 255 are preserved, rounded to the nearest integer; the behaviour on values outside this range is unspecified." fname)                    
                        let ncomp = img.GetNumberOfComponentsPerPixel()
                        if ncomp = 1ul
                        then SimpleITK.Cast(img,PixelIDValueEnum.sitkUInt8)
                        else // TODO: why simply casting the image doesn't work here? It works in load
                            let comps = Array.init (int ncomp) (fun i -> SimpleITK.VectorIndexSelectionCast(img,uint32 i,PixelIDValueEnum.sitkUInt8))
                            use flt = new ComposeImageFilter()
                            use v = new VectorOfImage(comps)
                            flt.Execute(v)                            
                    else
                        Logger.Warning (sprintf "saving boolean image to %s; value 'true' is set to 255, not 1"  fname)
                        SimpleITK.RescaleIntensity(img,0.0,255.0)
                else raise <| UnsupportedImageSizeException (Path.GetExtension filename)
            else raise <| UnsupportedImageTypeException (Path.GetExtension filename)
    Logger.Debug <| sprintf "Saving file %s" filename        
    SimpleITK.WriteImage(tmp,filename)  

let private getBuf<'t when 't : unmanaged> (img : Image) accessor =
    img.MakeUnique()
    let nativeInt = accessor()                    
    let ptr = NativePtr.ofNativeInt<'t> nativeInt in
    let len = int (img.GetNumberOfPixels() * img.GetNumberOfComponentsPerPixel()) in
    new NativeArray<'t>(ptr,len,img)

let floatV (img : Image) = 
    getBuf<float32> img img.GetBufferAsFloat

let int8V (img : Image) = 
    getBuf<int8> img img.GetBufferAsInt8
    
let uint8V (img : Image) = 
    getBuf<uint8> img img.GetBufferAsUInt8
    
let uint32V (img : Image) = 
    getBuf<uint32> img img.GetBufferAsUInt32
    
=======
>>>>>>> experimental
let private allocate (img : Image, pixeltype : PixelIDValueEnum) = // Does not guarantee the memory is cleared.
    match (img.GetNumberOfComponentsPerPixel(),img.GetPixelID() = pixeltype) with
        | (1ul,true) -> new Image(img)
        | (_,true) -> SimpleITK.VectorIndexSelectionCast(img,0ul)            
        | (1ul,false) -> SimpleITK.Cast(img,pixeltype)    
<<<<<<< HEAD
        | (_,_) ->
            use baseImg = SimpleITK.VectorIndexSelectionCast(img,0ul)
            SimpleITK.Cast(baseImg,pixeltype) 

let createUint8 (img : Image, value : uint8) =
    let res = allocate(img,PixelIDValueEnum.sitkUInt8)
    let buf = uint8V res
    buf.Fill value
    res

let createInt8 (img : Image, value : int8) =
    let res = allocate(img,PixelIDValueEnum.sitkInt8)
    let buf = int8V res
    buf.Fill value
    res

let createUint32 (img : Image, value : uint32) =
    let res = allocate(img,PixelIDValueEnum.sitkUInt32)
    let buf = uint32V res
    buf.Fill value
    res

let createFloat32 (img : Image, value : float32) =
    let res = allocate(img,PixelIDValueEnum.sitkFloat32)
    let buf = floatV res
    buf.Fill value
    res

let changePhysicalSpace(dest : Image, source : Image) = // NOTE: only works with float32 images
    let res = allocate(source,PixelIDValueEnum.sitkFloat32)
    let smem = floatV(dest)
    let dmem = floatV(res)
    for i = 0 to int <| dest.GetNumberOfPixels() do
        for j = 0 to int <| source.GetNumberOfComponentsPerPixel() do
            dmem.USet (i+j) (smem.UGet(i+j))
    res

=======
        | (_,_) -> SimpleITK.VectorIndexSelectionCast(img,0ul,pixeltype)
            
>>>>>>> experimental

let inline private decode (size : array<int>) (idx : int) (res : array<int>) =        
    let mutable div = idx
    for i = 0 to size.Length - 1 do
        res.[i] <- div % size.[i]
        div <- div / size.[i]    
<<<<<<< HEAD
let mkConst value img = createFloat32(img,value)
let mkBConst value img = createUint8(img,if value then 1uy else 0uy)

let border (img : Image) = 
    // TODO: make this faster by first filling the result with zeroes and then iterating only over the borders
    let szv = img.GetSize()
    let dim = szv.Count
    let sz = Array.init dim (fun i -> int szv.[i])
    let res = createUint8(img,0uy)
    let buf = uint8V res               
    let coords = Array.create dim 0  
    buf.Replace <|
        fun i ->
            decode sz i coords
            if Array.exists2 (fun c max -> c = 0 || c = max-1) coords sz 
            then 1uy 
            else 0uy           
    res
let logand (img1 : Image) (img2 : Image) = SimpleITK.And(img1,img2)
let logor (img1 : Image) (img2 : Image) = SimpleITK.Or(img1,img2)
let lognot (img : Image) = SimpleITK.Not(img)

let near (img : Image) = SimpleITK.DilateObjectMorphology(img,1ul,KernelEnum.sitkBox,1.0)
let interior (img : Image) = SimpleITK.BinaryErode img

let subtract (img1 : Image) (img2 : Image) = SimpleITK.Subtract(img1,img2)
let add (img1 : Image) (img2 : Image) = SimpleITK.Add(img1,img2)
let mult (img1 : Image) (img2 : Image) = SimpleITK.Multiply(img1,img2)

// let flood (img1 : Image) (img2 : Image) =
//     use flt = new ConnectedComponentImageFilter()
//     flt.SetFullyConnected(true)    
//     use cc1 = flt.Execute(img2)    
//     let k = int <| flt.GetObjectCount()    
//     let ccs = Array.create (k+1) false    
//     use n1 = near img1
//     let bufcc1 = uint32V cc1
//     let bufn1 = uint8V n1
//     let npixels = int (img1.GetNumberOfPixels())    
//     for i = 0 to npixels - 1 do
//         if bufn1.UGet i > 0uy then
//             let cc = bufcc1.UGet i
//             if cc > 0ul then ccs.[int cc] <- true
//     let res = new Image(img1)
//     let bufres = uint8V res
//     for i = 0 to npixels - 1 do
//         let cc = bufcc1.UGet i
//         if cc > 0ul && ccs.[int cc] then // TODO: is the check "cc > 0ul" necessary? ccs.[0] should be "false" anyway. Also, can we directly use 1uy and 0uy instead of true and false, and write "bufres.USet i ccs.[int cc]"?
//             bufres.USet i 1uy
//     res   

let dt img =
    use flt = new itk.simple.SignedMaurerDistanceMapImageFilter()
    flt.Execute(img, false, false, true, 0.0)

let eq (value : float) (img : Image) =        
    use flt = new BinaryThresholdImageFilter() 
    flt.Execute(img,value,value,1uy,0uy)

let geq (value : float) (img : Image) =
    use flt = new GreaterEqualImageFilter()
    flt.Execute(img,value)    

let leq (value : float) (img : Image) =    
    use flt = new LessEqualImageFilter()
    flt.Execute(img,value)
    
let between value1 value2 img =
    use flt = new BinaryThresholdImageFilter()    
    flt.SetLowerThreshold(value1)
    flt.SetUpperThreshold(value2)
    flt.SetInsideValue(1uy)
    flt.SetOutsideValue(0uy)
    flt.Execute(img)

let mask img maskImg = SimpleITK.Mask(img,maskImg)
let through (img1 : Image) (img2 : Image) =  // x satisfies (through phi1 phi2) iff there is path p and index l s.t. p(l) satisfies phi1, and for all k in [0,l] p(k) satisfies phi2
    use flt = new ConnectedComponentImageFilter()
    flt.SetFullyConnected(true)
    use cc2 = flt.Execute(img2)
    let k = int <| flt.GetObjectCount()
    let ccs = Array.create (k+1) 0uy

    use m1 = mask cc2 img1 

    let bufm1 = uint32V m1
    let bufcc1 = uint32V cc2

    let npixels = int (img1.GetNumberOfPixels())

    for i = 0 to npixels - 1 do
        let cc = int (bufm1.UGet i)
        if cc > 0 then ccs.[cc] <- 1uy

    let res = createUint8(img1,0uy)
    let bufres = uint8V res

    for i = 0 to npixels - 1 do        
        let cc = int <| bufcc1.UGet i
        if cc > 0 then bufres.USet i ccs.[cc]

    res
=======
>>>>>>> experimental

/// <summary>Creates a 1-dimensional array <c>a</c> with as many elements as the voxels in a hyperrectangle with dimensions specified by <c>hyperRadius</c>.
/// The i-th dimension is obtained as <c>2*hyperRadius[i]+1</c>. The created array is only useful in a image <c>img</c> of dimensions as specified in the <c>size</c> parameter. 
/// Each element <c>a[i]</c> is a "displacement" expressed as a pair of a linear relative coordinate and a dimensional relative coordinate. Given a linear coordinate <c>x</c>, the linear coordinate <c>x+a[i]</c>, if it lies in <c>0..n</c> where <c>n</c> is the number of voxels, is the coordinate of a point in the hyperrectangle centered at <c>x</c> (and of the specified dimensions).
/// The displacements in <c>a</c> are ordered in raster-scan order.
/// </summary>
/// <remarks>
/// In other words, the contents of the array <c>a</c> are the linear coordinates (including negative ones), in raster scan, of a hyperrectangle centered at the origin.
/// </remarks>
/// <param name="size">The size of images on which the obtained hyperrectangle makes sense. Must have the same length as <c>hyperRadius</c>.</param>
/// <param name="hyperRadius">The "multidimensional radius" of the hyperrectangle (in voxels).  Must have the same length as <c>size</c>. </param>
/// <returns>A pair whose first element is the array <c>a</c>, and whose second element is an array <c>faces</c> of arrays of lists.
/// The array <c>faces</c> contains the coordinates of the faces of the hyperrectangle <c>a</c>: 
/// each element <c>face[dim][i]</c>, with <c>dim</c> an index in the same range as the <c>size</c> parameter, and <c>i</c> ranging over <c>{0,1}</c>,
/// is the list of coordinates of one of the two faces of the hypercube where the <c>dim-th</c> coordinate is either minimal, when <c>i=0</c>, or maximal, when <c>i=1</c>.
/// </returns>
let private hyperrectangle (size : array<int>) (hyperRadius : array<int>) = 
    assert (size.Length = hyperRadius.Length)
    let ndims = size.Length        
    let diameter = Array.map (fun x -> (2 * x) + 1) hyperRadius
    let smallNPixels = Array.fold (*) 1 diameter
    
    let displacements = Array.copy size
    displacements.[0] <- 1
    for i = 1 to ndims - 1 do
        displacements.[i] <- displacements.[i-1] * size.[i-1]
    let dimensionalCursor = Array.map (~-) hyperRadius 
    let tmp = Array.create ndims 0
    let mutable linearCursor = 0
    let inline doprod () =
        Array.iteri2 (fun i v1 v2 -> tmp.[i] <- v1 * v2) dimensionalCursor displacements 
        linearCursor <- Array.sum tmp            
    doprod()
    let inline inc () =
        let mutable n = 0
        while n < ndims do        
            let x = (dimensionalCursor.[n] + 1)
            let y = hyperRadius.[n]
            if x > y then
                dimensionalCursor.[n] <- (-y)
                n <- n + 1
            else 
                dimensionalCursor.[n] <- x
                n <- ndims
        doprod() // TODO: this step could be made more efficient. Instead of doing this product (which should, anyway, be done using a specialised library), one could increment "linearCursor" directly in the above while loop; but this is tricky since one has to compute the displacement minus the diameter of the ball multiplied by something (what?)
    let faces = Array.init ndims (fun _ -> Array.create 2 [])        

    let indices = 
        Array.init smallNPixels <|            
            fun _ -> 
                let x = linearCursor // TODO perhaps there's an off by one error here (print dimensionalCursor and LinearCursor in doprod() to see this)
                for dim = 0 to ndims - 1 do
                    if dimensionalCursor.[dim] = -hyperRadius.[dim] then
                        faces.[dim].[0] <- x::(faces.[dim].[0])
                    else if dimensionalCursor.[dim] = hyperRadius.[dim] then
                        faces.[dim].[1] <- x::(faces.[dim].[1])
                inc()                
                x
    (indices,faces)        

/// <summary>Computes cross-correlation between two histograms; the first argument can be curried for a speedup</summary
/// <remarks>The histograms must have the same length</remarks>
let private r : array<int> -> array<int> -> float = 
    fun h2 ->
    // computes cross-correlation between two histograms; the first argument can be curried for a speedup
        let avg2 = (float (Array.sum h2)) / (float (Array.length h2))
        let sqrtDen2 = sqrt (Array.sumBy (fun n -> ((float n) - avg2)**2.0) h2)
        fun (h1 : array<int>) ->
            assert (h1.Length = h2.Length)        
            let avg1 = (float (Array.sum h1)) / (float (Array.length h1))        
            let den1 = Array.sumBy (fun n -> ((float n) - avg1)**2.0) h1
            if den1 = 0.0 && sqrtDen2 = 0.0 then 1.0
            else 
                if den1 = 0.0 || sqrtDen2 = 0.0 then 0.0
                else     
                    let num = Array.fold2 (fun acc n1 n2 -> acc + (((float n1)-avg1)*((float n2)-avg2))) 0.0 h1 h2 // TODO: PRECOMPUTE n2 - avg2
                    let den = ((sqrt den1)*sqrtDen2)      
                    let res = num / den
                    res



let inline private mkDelta m1 m2 k =
    (m2 - m1) / (float k)



/// <summary>
/// Adds <c>increment</c> to the value in <c>histogram</c> corresponding to the bin of <c>value</c> with extremes <c>m1</c> and <c>m2</c>, and interval size <c>delta</c> 
/// </summary>
/// <remarks>
/// <c>delta</c> must be equal to <c>(m2 - m1) / l</c> where <c>l</c> is the number of elements of <c>histogram</c>
/// </remarks>
let inline private bin (m1 : float,m2 : float,delta : float, increment : int) (value : float)  (histogram : array<int>) =
    assert (delta = mkDelta m1 m2 histogram.Length)
    if value >= m1 && value < m2 then // TODO: can this bounds checking be omitted if the image is pre-thresholded?
        let histIdx = int ((value - m1) / delta) //TODO: do we want "int (floor (value - m1 / delta)) here?"
        histogram.[histIdx] <- (histogram.[histIdx] + increment)
    


    ///<summary>
/// Applies <c>fn</c> to the linear coordinates of all points in a Hamiltonian path over an image of dimension <c>size</c> **except the origin 0**; 
/// the first argument passed to <c>fn</c> is the coordinate (linear,dimensional) of the *previous* point (which can be 0)
/// the second argument passed to <c>fn</c> is the coordinate (linear,dimensional) of the *current* point
/// the third argument passed to <c>fn</c> is an integer that represents the direction of movement:
/// the modulus of the third argument is the index of the dimension of movement (1 is first dimension)
/// the sign of the second argument is the direction (positive, negative).
///</summary>     
    
let private snake (innerSize : array<int>) (radius : array<int>) = // TODO: does this work when radius is 0?
    let innerLength = Array.fold (*) 1 innerSize 
    let outerSize = Array.mapi (fun i n -> n + (2*radius.[i])) innerSize    
    let ndims = radius.Length
    let pathidx = Array.create innerLength 0 // To be returned 
    let pathdir = Array.create innerLength 0 // To be returned

    // Initialise the "displacements" array     
    // displacements.[i] is the difference between the linear coordinates of a point, and another point off by one in the i-th dimension.            
    let displacements = Array.copy outerSize
    displacements.[0] <- 1
    for i = 1 to ndims - 1 do
        displacements.[i] <- displacements.[i-1] * outerSize.[i-1]

    // Helpers for the "step" function
    let direction = Array.create ndims 1 
    let dimensionalCursor = Array.copy radius 
    let mutable linearCursor = 0
    let tmp = Array.create ndims 0    
    let inline updateLinearCursor () =
        Array.iteri2 (fun i v1 v2 -> tmp.[i] <- v1 * v2) dimensionalCursor displacements 
        linearCursor <- Array.sum tmp            

    // One step of the "snake" algorithm
    let inline step () =
        let mutable resDir = 0
        let mutable n = 0
        while n < ndims do    
            let d = direction.[n]
            let x = (dimensionalCursor.[n] + d)                       
            if x < radius.[n] || x >= (radius.[n] + innerSize.[n]) then    // TODO: this sum could be pre-computed; furthermore only one check is really needed per direction.          
                direction.[n] <- -d
                n <- n + 1
            else
                resDir <- d * (n+1)
                dimensionalCursor.[n] <- x
                n <- ndims + 1
        updateLinearCursor ()           
        resDir
      
    // Algorithm initialization
    let mutable dir = 0
    let mutable n = 0
    updateLinearCursor()

    // Main loop
    while n < innerLength do
        pathidx.[n] <- linearCursor
        pathdir.[n] <- dir
        n <- n + 1        
        dir <- step()
    (pathidx,pathdir)  

type VoxImage private (img : Image,uniqueName : string) =
    //let threadid = System.Threading.Thread.CurrentThread       
    let hashImg = img.GetHashCode().ToString() // TODO: hash the image data (voxels and headers) instead of using .NET's hashing (what's that based on, in this case, anyway?)
    let disposed = ref false
    static let internalNum = ref 0
    let internalId = 
        lock internalNum 
            (fun () -> 
                let res = !internalNum
                internalNum := res + 1
                res)
    let dispose () =
        lock disposed (fun () -> if not !disposed then disposed := true; img.Dispose())

    let niBuffer = 
        let id = img.GetPixelID() 
        if id = PixelIDValueEnum.sitkFloat32 || id = PixelIDValueEnum.sitkFloat64 || id = PixelIDValueEnum.sitkVectorFloat32 || id = PixelIDValueEnum.sitkVectorFloat64 then img.GetBufferAsFloat()
        else if id = PixelIDValueEnum.sitkUInt8 || id = PixelIDValueEnum.sitkVectorUInt8 then img.GetBufferAsUInt8()
        else if id = PixelIDValueEnum.sitkInt8 || id = PixelIDValueEnum.sitkVectorInt8 then img.GetBufferAsInt8()
        else if id = PixelIDValueEnum.sitkInt32 || id = PixelIDValueEnum.sitkVectorInt32 then img.GetBufferAsInt32()
        else if id = PixelIDValueEnum.sitkUInt32 || id = PixelIDValueEnum.sitkVectorUInt32 then img.GetBufferAsUInt32()        
        else raise (UnsupportedPixelFormatException (img.GetPixelIDTypeAsString()))

    interface System.IDisposable with
        member __.Dispose () = dispose ()

    override __.Finalize () = dispose()

    // TODO: make private!
    member __.Image = img

    override __.ToString() = sprintf "{ hash: \"%s\"; uniqueName: \"%s\"; progressiveId: %d; }" hashImg uniqueName internalId

    new (img : Image) =
        new VoxImage(img,"") 

    new (filename : string) = 
        // WARNING: the program assumes this function always returns a float32 image. Be cautious before changing this.
        let loadedImg =
            Logger.Debug <| sprintf "Loading file %s" filename
            let img = SimpleITK.ReadImage(filename)
            let fname = System.IO.Path.GetFileName(filename)
            let sz = img.GetSize()
            let mutable found = false
            if sz.Count > 2 then
                for i = 0 to sz.Count - 1 do
                    if sz.[i] = 1u then (sz.[i] <- 0u; found <- true)
            let img = 
                if found then 
                    Logger.Warning (sprintf "image %s has size 1 in some dimensions; image flattened" fname)    
                    let res = SimpleITK.Extract(img,sz) 
                    img.Dispose()
                    res
                else img
            Logger.DebugOnly (sprintf "Loaded image %s components per pixel: %d, pixel type: %A" fname (img.GetNumberOfComponentsPerPixel()) (img.GetPixelID()))
            match img.GetPixelID(),img.GetNumberOfComponentsPerPixel() with 
                | (x,_) when x = PixelIDValueEnum.sitkFloat32 -> img
                | (x,_) when x = PixelIDValueEnum.sitkVectorFloat32 -> img
                | (_,y) when y = 1u -> 
                    Logger.DebugOnly (sprintf "image %s\ncasted to float32" fname)                    
                    let res = SimpleITK.Cast(img,PixelIDValueEnum.sitkFloat32)
                    img.Dispose()
                    res
                | (_,y) when y = 3u || y = 4u -> 
                    Logger.DebugOnly (sprintf "image %s\ncasted to float32" fname)
                    if y = 4u then 
                        Logger.Warning <| sprintf "image %s\nhas 4 color components per voxel. Assuming RGBA color space (CMYK is not supported)." fname 
                        if fname.EndsWith ".jpg" then 
                            Logger.Warning <| sprintf "image %s\nhas jpg extension and 4 components per pixel, therefore it is in CMYK color space. Only proceed if you know what you are doing. Colors and intensity of the image will be messed up in processing." fname
                    let res = SimpleITK.Cast(img,PixelIDValueEnum.sitkVectorFloat32)
                    img.Dispose()
                    res
                | (x,y) -> img.Dispose(); raise <| UnsupportedImageTypeException (x.ToString() + "-" + y.ToString())
        new VoxImage(loadedImg,sprintf "file:%s" filename)  // TODO: canonicize filename and / or use the file hash
    
    new (img : VoxImage, pixeltype : PixelIDValueEnum) =        
        new VoxImage(
                allocate(img.Image,pixeltype),
                (   Logger.DebugOnly <| sprintf "Allocating image from %s pixeltype: %s" (img.ToString()) (pixeltype.ToString()); 
                    sprintf "allocate:[%s][%s]" (img.ToString()) (pixeltype.ToString())))

    new (img : VoxImage) = new VoxImage(new Image(img.Image),sprintf "copy:[%s]" (img.ToString()))   
    
<<<<<<< HEAD
            let (hidx,hdir) = snake size ballRadius
            let outerBuf = floatV outerImage 
            use temporaryImage = new Image(outerImage)
            let temporaryBuf = floatV temporaryImage            
            let nprocs = System.Environment.ProcessorCount
            let fragsize = npixels / nprocs

            let jobFn procindex =
                let fragstart = procindex * fragsize
                let start = hidx.[fragstart]    
                let localHistogram = init outerBuf temporaryBuf start
                let target = fragstart + fragsize - 1 
                let mutable previous = start                           
                for pos = fragstart+1 to min target (npixels - 1) do
                    let (center,direction) = (hidx.[pos],hdir.[pos])                            
                    forEachPoint outerBuf temporaryBuf localHistogram center previous direction
                    previous <- center   

            let mkJob procindex =
                job { jobFn procindex }

            do! Util.Concurrent.conIgnore (Array.init nprocs mkJob)
            
            return SimpleITK.Crop(temporaryImage,vradius,vradius) }

let volume (img : Image) =
    let mutable res = 0
    let npixels = int <| img.GetNumberOfPixels()
    let b = uint8V img
    for i = 0 to npixels - 1 do
        res <- res + (if b.UGet i > 0uy then 1 else 0)    
    float res

let maxvol (img : Image) =
    use flt = new ConnectedComponentImageFilter()
    let npixels = int <| img.GetNumberOfPixels()
    flt.SetFullyConnected(true)    
    use ccs = flt.Execute(img)
    let bufccs = uint32V ccs
    let k = int <| flt.GetObjectCount()    
    let volumes = Array.create (k + 1) 0
    for i = 0 to npixels - 1 do
        let cc = int <| bufccs.UGet i
        if cc <> 0 then volumes.[cc] <- volumes.[cc] + 1
    let mutable maxvol,ccs = 0,[]
    for i = 1 to volumes.Length - 1 do
        let vol = volumes.[i]
        if vol > maxvol then         
            maxvol <- vol
            List.iter (fun cc -> volumes.[cc] <- 0) ccs
            volumes.[i] <- 1
            ccs <- [i]
        else if vol = maxvol then 
            ccs <- i::ccs
            volumes.[i] <- 1
        else volumes.[i] <- 0
    let res = new Image(img)    
    let bufres = uint8V res
    for i = 0 to npixels - 1 do
        let cc = int <| bufccs.UGet i
        bufres.USet i (uint8 (volumes.[cc]))
    res

let percentiles (img : Image) (mask : Image) (correction : float) =    
    let res = SimpleITK.Mask(img,mask,-1.0)
    res.MakeUnique()
    img.MakeUnique()
    let bufimg = floatV img
    let bufmask = uint8V mask
    let npixels = bufimg.Length
    let population = Seq.filter (fun i -> bufmask.UGet i > 0uy) (seq {0..npixels - 1})
    let data =         
        population  |> 
        Seq.groupBy bufimg.UGet |> 
        Seq.sortBy fst |>
        Seq.map (fun (_,indices) -> (Seq.length indices,indices))    
    let bufres = floatV res
    let mutable curvol = 0
    let vol = float32 (Seq.length population)
    for (size,indices) in data do
        let value = ((float32 curvol) + ((float32 correction) * (float32 size))) / vol
        for idx in indices do
            bufres.USet idx value
        curvol <- curvol + size        
    res    
=======
    member __.Save (filename : string) =
        let fname = System.IO.Path.GetFileName(filename)
        if fname.EndsWith(".jpg") && img.GetNumberOfComponentsPerPixel() > 3ul then
            Logger.Warning <| sprintf "Saving to %s\nusing 4 components per pixel. The resulting image will be CMYK; only proceed if you know what you are doing." fname
        let tmp = 
            if filename.EndsWith(".nii") || filename.EndsWith(".nii.gz") then img
            else 
                if filename.EndsWith(".png") || filename.EndsWith(".jpg") || filename.EndsWith ("bmp") then
                    if img.GetSize().Count = 2 then 
                        if img.GetPixelID() <> PixelIDValueEnum.sitkUInt8 then
                            // TODO: double-check that "nearest integer" in the message below is correct
                            Logger.Warning (sprintf "saving to %s\nrequires cast to uint8. For each component, only values between 0 and 255 are preserved, rounded to the nearest integer; the behaviour on values outside this range is unspecified." fname)                    
                            let ncomp = img.GetNumberOfComponentsPerPixel()
                            if ncomp = 1ul
                            then SimpleITK.Cast(img,PixelIDValueEnum.sitkUInt8)
                            else // TODO: why simply casting the image doesn't work here? It works in load
                                let comps = Array.init (int ncomp) (fun i -> SimpleITK.VectorIndexSelectionCast(img,uint32 i,PixelIDValueEnum.sitkUInt8))
                                use flt = new ComposeImageFilter()
                                use v = new VectorOfImage(comps)
                                flt.Execute(v)                            
                        else
                            Logger.Warning (sprintf "saving boolean image to %s; value 'true' is set to 255, not 1"  fname)
                            SimpleITK.Multiply(img,255.0)                            
                    else raise <| UnsupportedImageSizeException (Path.GetExtension filename)
                else raise <| UnsupportedImageTypeException (Path.GetExtension filename)
        Logger.Debug <| sprintf "Saving file %s" filename        
        SimpleITK.WriteImage(tmp,filename)  

    member __.Dimension = int (img.GetDimension())

    member __.NPixels = int (img.GetNumberOfPixels())

    member __.NComponents = int (img.GetNumberOfComponentsPerPixel())

    member this.GetBufferAsUInt8 fn = 
        use vect = new NativeArray<uint8>(NativePtr.ofNativeInt niBuffer,(int <| img.GetNumberOfPixels()) * (int <| img.GetNumberOfComponentsPerPixel()),this)
        fn vect

    member this.GetBufferAsFloat fn = 
        use vect = new NativeArray<float32>(NativePtr.ofNativeInt niBuffer,(int <| img.GetNumberOfPixels()) * (int <| img.GetNumberOfComponentsPerPixel()),this)
        fn vect

    member this.GetBufferAsUInt32 fn = 
        use vect = new NativeArray<uint32>(NativePtr.ofNativeInt niBuffer,(int <| img.GetNumberOfPixels()) * (int <| img.GetNumberOfComponentsPerPixel()),this)
        fn vect

    static member CreateUInt8 (img : VoxImage,v : uint8) =
        let res = new VoxImage(img,PixelIDValueEnum.sitkUInt8)
        res.GetBufferAsUInt8 (fun (buf : NativeArray<_>) -> buf.Fill v) // TODO: instead of calling Fill each time, call fill only once in an internal buffer and then use Marhsall.Copy (in a Copy method of NativeArray)
        res

    static member CreateFloat (img : VoxImage,v : float32) =
        let res = new VoxImage(img,PixelIDValueEnum.sitkFloat32)
        res.GetBufferAsFloat (fun (buf : NativeArray<_>) -> buf.Fill v)    
        res

    static member SamePhysicalSpace (img1 : VoxImage) (img2 : VoxImage) =
        try
            use x = SimpleITK.Add(img1.Image,img2.Image) 
            true
        with _ -> 
            false

    static member Abs (img : VoxImage) =
        new VoxImage(SimpleITK.Abs(img.Image))

    static member Max (img : VoxImage) =
        use flt = new MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
        flt.Execute(img.Image)    
        flt.GetMaximum()    
    
    static member Min (img : VoxImage) =
        use flt = new MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
        flt.Execute(img.Image)
        flt.GetMinimum()

    member this.ChangePhysicalSpace(source : VoxImage) = // NOTE: only works with float32 images 
        // TODO: this method can only be called from "SITKModel.Load". Change the logic so that this is an internal function of SITKModel.Load 
        assert (source.Image.GetNumberOfComponentsPerPixel() = img.GetNumberOfComponentsPerPixel() && source.Image.GetNumberOfPixels() = img.GetNumberOfPixels())        
        let res = new VoxImage(source,PixelIDValueEnum.sitkFloat32)
        this.GetBufferAsFloat (
            fun smem ->
                res.GetBufferAsFloat (
                    fun dmem ->
                        dmem.CopyFrom smem))
        res

    static member Intensity (img : VoxImage) =
            if img.Image.GetNumberOfComponentsPerPixel() = 1ul
            then new VoxImage(img) 
            else  // TODO check color space correctly!!! Assumes it's rgb                
                use r = SimpleITK.VectorIndexSelectionCast(img.Image,0ul)
                use g = SimpleITK.VectorIndexSelectionCast(img.Image,1ul) 
                use b = SimpleITK.VectorIndexSelectionCast(img.Image,2ul)
                // Source: https://en.wikipedia.org/wiki/Relative_luminance
                // the same formula is used in GIMP and called "Luminosity"
                let (rcoeff,gcoeff,bcoeff) = 0.2126,0.7152,0.0722
                new VoxImage(SimpleITK.Add(SimpleITK.Multiply(rcoeff,r),SimpleITK.Add(SimpleITK.Multiply(gcoeff,g),SimpleITK.Multiply(bcoeff,b))))

    static member Component (img : VoxImage) (i : int) = 
        assert (i >= 0 && i < (int (img.Image.GetNumberOfComponentsPerPixel())))
        new VoxImage(SimpleITK.VectorIndexSelectionCast(img.Image,(uint32 i)))

    static member Red (img : VoxImage) = VoxImage.Component img 0
    static member Green (img : VoxImage) = VoxImage.Component img 1
    static member Blue (img : VoxImage) = VoxImage.Component img 2
    static member Alpha (img : VoxImage) = 
        if img.Image.GetNumberOfComponentsPerPixel() < 4ul 
        then 
            let res = new VoxImage(img,PixelIDValueEnum.sitkFloat32) 
            res.GetBufferAsFloat(fun buf -> buf.Fill 255.0f)
            res
        else VoxImage.Component img 3

    static member RGB (img1 : VoxImage) (img2 : VoxImage) (img3 : VoxImage) = 
        use flt = new ComposeImageFilter()
        new VoxImage(flt.Execute(img1.Image,img2.Image,img3.Image))

    static member RGBA (img1 : VoxImage) (img2 : VoxImage) (img3 : VoxImage) (img4 : VoxImage) =
        use flt = new ComposeImageFilter()
        new VoxImage(flt.Execute(img1.Image,img2.Image,img3.Image,img4.Image))

    static member Avg (img : VoxImage) (mask : VoxImage) = // TODO: type check that there is one component only
        let mutable (l : list<float32>) = [] 
        img.GetBufferAsFloat(
            fun imgv ->
                mask.GetBufferAsUInt8(
                    fun maskv ->
                        for i = 0 to imgv.Length - 1 do
                        if maskv.UGet i > 0uy then
                            l <- (imgv.UGet i)::l    // TODO: URGENT: BUG: check why results are slightly different from the master branch (commmit: e24602f1c54348a0c6f8188b3bf44573bd393703)
                ))
        List.averageBy float l
    
    static member TT (img : VoxImage) = VoxImage.CreateUInt8(img,1uy)
    static member FF (img : VoxImage) = VoxImage.CreateUInt8(img,0uy)

    static member Border (img : VoxImage) = 
        // TODO: make this faster by first filling the result with zeroes and then iterating only over the borders
        let szv = img.Image.GetSize()
        let dim = szv.Count
        let sz = Array.init dim (fun i -> int szv.[i])
        let res = VoxImage.CreateUInt8(img,0uy)
        res.GetBufferAsUInt8 
            (fun buf ->               
                let coords = Array.create dim 0  
                buf.Replace <|
                    fun i ->
                        decode sz i coords
                        if Array.exists2 (fun c max -> c = 0 || c = max-1) coords sz 
                        then 1uy 
                        else 0uy)
        res

    static member Logand (img1 : VoxImage) (img2 : VoxImage) = new VoxImage(SimpleITK.And(img1.Image,img2.Image))
    static member Logor (img1 : VoxImage) (img2 : VoxImage) = new VoxImage(SimpleITK.Or(img1.Image,img2.Image))
    static member Lognot (img : VoxImage) = new VoxImage(SimpleITK.Not(img.Image))

    static member Near (img : VoxImage) = new VoxImage(SimpleITK.DilateObjectMorphology(img.Image,1ul,KernelEnum.sitkBox,1.0))
    static member Interior (img : VoxImage) = new VoxImage(SimpleITK.BinaryErode img.Image)

    static member Subtract (img1 : VoxImage,img2 : VoxImage) = new VoxImage(SimpleITK.Subtract(img1.Image,img2.Image))
    static member Subtract (k : float,img2 : VoxImage) = new VoxImage(SimpleITK.Subtract(k,img2.Image))
    static member Subtract (img1 : VoxImage,k : float) = new VoxImage(SimpleITK.Subtract(img1.Image,k))
    static member Add (img1 : VoxImage,img2 : VoxImage) = new VoxImage(SimpleITK.Add(img1.Image,img2.Image))
    static member Add (img1 : VoxImage,k : float) = new VoxImage(SimpleITK.Add(img1.Image,k))
    static member Mult (img1 : VoxImage,img2 : VoxImage) = new VoxImage(SimpleITK.Multiply(img1.Image,img2.Image))
    static member Mult (img1 : VoxImage,k : float) = new VoxImage(SimpleITK.Multiply(img1.Image,k))
    static member Div (k : float, img2 : VoxImage) = new VoxImage(SimpleITK.Divide(k,img2.Image))

    static member Dt (img : VoxImage) =
        use flt = new SignedMaurerDistanceMapImageFilter()
        new VoxImage(flt.Execute(img.Image, false, false, true, 0.0))

    static member Eq (value : float) (img : VoxImage) =        
        use flt = new BinaryThresholdImageFilter() 
        new VoxImage(flt.Execute(img.Image,value,value,1uy,0uy))

    static member Geq (value : float) (img : VoxImage) =
        use flt = new GreaterEqualImageFilter()
        new VoxImage(flt.Execute(img.Image,value))

    static member Leq (value : float) (img : VoxImage) =    
        use flt = new LessEqualImageFilter()
        new VoxImage(flt.Execute(img.Image,value))
    
    static member Between value1 value2 (img : VoxImage) =
        use flt = new BinaryThresholdImageFilter()    
        flt.SetLowerThreshold(value1)
        flt.SetUpperThreshold(value2)
        flt.SetInsideValue(1uy)
        flt.SetOutsideValue(0uy)
        new VoxImage(flt.Execute(img.Image))

    static member Mask (img : VoxImage) (mask : VoxImage) (value : float) = new VoxImage(SimpleITK.Mask(img.Image,mask.Image,value))
    
    static member LabelConnectedComponents (img : VoxImage) = 
        use flt = new ConnectedComponentImageFilter()
        flt.SetFullyConnected(true)
        (new VoxImage(flt.Execute(img.Image)),int <| flt.GetObjectCount())

    static member Lcc (img : VoxImage) =        
        let (tmp,_) = VoxImage.LabelConnectedComponents img
        use tmp = tmp
        new VoxImage(SimpleITK.Cast(tmp.Image,PixelIDValueEnum.sitkFloat32))

    static member Through (img1 : VoxImage) (img2 : VoxImage) =  // x satisfies (through phi1 phi2) iff the connected component of phi2 containing x also contains a point of phi1
        let (cc,k) = VoxImage.LabelConnectedComponents img2
        use cc2 = cc
        let ccs = Array.create (k+1) 0uy

        use m1 = VoxImage.Mask cc2 img1 0.0
        m1.GetBufferAsUInt32 (
            fun bufm1 ->
                cc2.GetBufferAsUInt32 (
                    fun bufcc2 ->
                        let npixels = bufm1.Length
                        for i = 0 to npixels - 1 do
                            let cc = int (bufm1.UGet i)
                            if cc > 0 then ccs.[cc] <- 1uy
                        let res = VoxImage.CreateUInt8(img1,0uy)
                        res.GetBufferAsUInt8 (
                            fun bufres ->                            
                                for i = 0 to npixels - 1 do        
                                    let cc = int <| bufcc2.UGet i
                                    if cc > 0 then bufres.USet i ccs.[cc])
                        res))

    static member Volume (img : VoxImage) =
        let mutable res = 0
        img.GetBufferAsUInt8 (
            fun b ->
                for i = 0 to b.Length - 1 do
                    res <- res + (if b.UGet i > 0uy then 1 else 0))
        float res                

    static member MaxVol (img : VoxImage) =
        let (ccs,k) = VoxImage.LabelConnectedComponents img
        use ccs = ccs
        let res = new VoxImage(img)
        ccs.GetBufferAsUInt32
            (fun bufccs ->
                let volumes = Array.create (k + 1) 0
                for i = 0 to bufccs.Length - 1 do
                    let cc = int <| bufccs.UGet i
                    if cc <> 0 then volumes.[cc] <- volumes.[cc] + 1
                let mutable maxvol,ccs = 0,[]
                for i = 1 to volumes.Length - 1 do
                    let vol = volumes.[i]
                    if vol > maxvol then         
                        maxvol <- vol
                        List.iter (fun cc -> volumes.[cc] <- 0) ccs
                        volumes.[i] <- 1
                        ccs <- [i]
                    else if vol = maxvol then 
                        ccs <- i::ccs
                        volumes.[i] <- 1
                    else volumes.[i] <- 0
                res.GetBufferAsUInt8
                    (fun bufres ->                
                        for i = 0 to bufres.Length - 1 do
                            let cc = int <| bufccs.UGet i
                            bufres.USet i (uint8 (volumes.[cc]))))
        res       

    static member Percentiles (img : VoxImage) (mask : VoxImage) (correction : float) =    
        let res = VoxImage.Mask img mask -1.0
        img.GetBufferAsFloat (
            fun bufimg ->
                mask.GetBufferAsUInt8 (
                    fun bufmask ->     
                        res.GetBufferAsFloat (
                            fun bufres ->                                           
                                let population = Seq.filter (fun i -> bufmask.UGet i > 0uy) (seq {0..bufimg.Length - 1})
                                let data =         
                                    population  |> 
                                    Seq.groupBy bufimg.UGet |> 
                                    Seq.sortBy fst |>
                                    Seq.map (fun (_,indices) -> (Seq.length indices,indices))    
                                let mutable curvol = 0
                                let vol = float32 (Seq.length population)
                                for (size,indices) in data do
                                    let value = ((float32 curvol) + ((float32 correction) * (float32 size))) / vol
                                    for idx in indices do
                                        bufres.USet idx value
                                    curvol <- curvol + size)))
        res    

    /// <summary>Implements the "crossCorrelation" / "statistical comparison (SCMP)" operator
    /// </summary>
    /// 
    static member Crosscorrelation (rad : float) (a : VoxImage) (b : VoxImage) (fb : VoxImage) (m1 : float) (m2 : float) (k : float) = 
        job {   let npixels = int (a.Image.GetNumberOfPixels())
                let dims = a.Image.GetSpacing()
                let ballRadius = Array.create dims.Count 0
                for i = 0 to dims.Count - 1 do 
                    ballRadius.[i] <- int (round (rad / (float dims.[i]))) // Compute anisotropic voxel radiuses from real-world radius
                    if ballRadius.[i] = 0 then
                        Logger.Debug (sprintf "Computing cross correlation with radius %A but the pixdim number %A (starting from 0) is %A; approximated to 1 voxel in this dimension" rad i dims.[i])
                        ballRadius.[i] <- 1
                use vradius = new VectorUInt32(Array.map uint32 ballRadius)
                use outerImage = new VoxImage(SimpleITK.ConstantPad(a.Image,vradius,vradius,infinity))                 
                let size = Array.ofSeq (Seq.map int (a.Image.GetSize()))  
                let outerSize = Array.ofSeq (Seq.map int (outerImage.Image.GetSize()))
                let indices,faces = hyperrectangle outerSize ballRadius    
                //let npixels = int (a.GetNumberOfPixels())
                let nbins = int k
                let delta = mkDelta m1 m2 nbins

                let r' = // Curried cross-correlation where the global histogram has been pre-computed
                    let bigHistogram = Array.create nbins 0 // Global histogram to compare to         
                    b.GetBufferAsFloat (
                        fun bufb ->
                            fb.GetBufferAsUInt8 (
                                fun buffb ->                      
                                    for linearCoord = 0 to bufb.Length - 1 do // Fill big histogram
                                        let vfb = buffb.UGet linearCoord        
                                        if vfb > 0uy then
                                            let vb = float (bufb.UGet linearCoord)            
                                            bin (m1,m2,delta,1) vb bigHistogram))
                    r bigHistogram

                let inline doThing (buf : NativeArray<float32>) localHistogram (linearCenter : int) (increment : int) (linearEl : int) = 
                    let linearCoord = linearCenter + linearEl  
                    let va = float (buf.UGet linearCoord)
                    bin (m1,m2,delta,increment) va localHistogram            

                let init buf (bufres : NativeArray<float32>)  linearCenter = // Initialize local histogram, and sets the cross-correlation value, returns the local histogram
                    let localHistogram = Array.create nbins 0
                    Array.iter (doThing buf localHistogram linearCenter 1) indices
                    bufres.USet linearCenter (float32 (r' localHistogram))
                    localHistogram
                       
                let inline forEachPoint buf (bufres : NativeArray<float32>) localHistogram linearCenter linearPrevious direction =
                    let faceIdx = (abs direction) - 1                
                    let facepair = faces.[faceIdx]                
                    let face1,face2 = facepair.[0],facepair.[1]                
                    let (faceMinus,facePlus) = if direction > 0 then (face1,face2) else (face2,face1)                
                    List.iter (doThing buf localHistogram linearPrevious -1) faceMinus
                    List.iter (doThing buf localHistogram linearCenter 1) facePlus
                    bufres.USet linearCenter (float32 (r' localHistogram))

        
                let (hidx,hdir) = snake size ballRadius
                use temporaryImage = new VoxImage(outerImage)
                let nprocs = System.Environment.ProcessorCount

                
                let fragsize = npixels / nprocs                

                let jobFn procindex =
                    outerImage.GetBufferAsFloat (
                        fun outerBuf ->
                            temporaryImage.GetBufferAsFloat (
                                fun temporaryBuf ->
                                    let fragstart = procindex * fragsize
                                    let start = hidx.[fragstart]    
                                    let localHistogram = init outerBuf temporaryBuf start
                                    let target = fragstart + fragsize - 1 
                                    let mutable previous = start                           
                                    for pos = fragstart+1 to min target (npixels - 1) do
                                        let (center,direction) = (hidx.[pos],hdir.[pos])                            
                                        forEachPoint outerBuf temporaryBuf localHistogram center previous direction
                                        previous <- center))

                let mkJob procindex =
                    job { jobFn procindex }

                do! Util.Concurrent.conIgnore (Array.init nprocs mkJob)
                
                return new VoxImage(SimpleITK.Crop(temporaryImage.Image,vradius,vradius)) }
>>>>>>> experimental
