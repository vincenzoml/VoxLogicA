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
open VoxLogicA
open itk.simple

exception UnsupportedImageTypeException of s : string
    with override this.Message = sprintf "Unsupported image type: %s" this.s

exception UnsupportedNumberOfComponentsPerPixelException of i : int
    with override this.Message = sprintf "Unsupported number of components per pixel: %d" this.i

// TODO: review this

#nowarn "9"

open Hopac

open Microsoft.FSharp.NativeInterop

let maxImg (img : Image) =
    use flt = new itk.simple.MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
    flt.Execute(img)    
    flt.GetMaximum()    
    
let minImg (img : Image) =
    use flt = new itk.simple.MinimumMaximumImageFilter() // TODO: implement tuples and projections, make max and min a single operator
    flt.Execute(img)
    flt.GetMinimum()
    
let loadImage (filename : string) (logger : ErrorMsg.Logger) =
    let img = SimpleITK.ReadImage(filename)
    let fname = System.IO.Path.GetFileName(filename)
    let sz = img.GetSize()
    let mutable found = false
    for i = 0 to sz.Count - 1 do
        if sz.[i] = 1u then (sz.[i] <- 0u; found <- true)
    let img = 
        if found then 
            logger.Warning (sprintf "image %s has size 1 in some dimensions; image flattened" fname)    
            SimpleITK.Extract(img,sz) 
        else img
    logger.Debug (sprintf "Loaded image %s components per pixel: %d, pixel type: %A" fname (img.GetNumberOfComponentsPerPixel()) (img.GetPixelID()))
    match img.GetPixelID(),img.GetNumberOfComponentsPerPixel() with 
        | (x,_) when x = PixelIDValueEnum.sitkFloat32 -> img
        | (x,_) when x = PixelIDValueEnum.sitkVectorFloat32 -> img
        | (_,y) when y = 1u -> 
            logger.Warning (sprintf "image %s\ncasted to float32" fname) 
            SimpleITK.Cast(img,PixelIDValueEnum.sitkFloat32)
        | (_,y) when y = 3u || y = 4u-> 
            logger.Warning (sprintf "image %s\ncasted to float32" fname) 
            SimpleITK.Cast(img,PixelIDValueEnum.sitkVectorFloat32)        
        | (x,y) -> raise <| UnsupportedImageTypeException (x.ToString() + "-" + y.ToString())

let saveImage (filename : string) (img : Image) (logger : ErrorMsg.Logger) =
    let fname = System.IO.Path.GetFileName(filename)
    let tmp = 
        match System.IO.Path.GetExtension filename with
            | ".nii" -> img
            | (".png"|".jpg") -> 
                if img.GetPixelID() <> PixelIDValueEnum.sitkUInt8 then
                    logger.Warning (sprintf "saving to %s\nrequires cast to uint8" fname)                    
                    let m1 = minImg(img)
                    let m2 = maxImg(img)
                    let tmp = 
                        if m1 < 0.0 || m2 > 255.0 
                        then 
                            logger.Warning (sprintf "saving to %s\nrequires rescaling of intensity" fname)
                            SimpleITK.RescaleIntensity(img,0.0,255.0)
                        else img
                    SimpleITK.Cast(tmp,PixelIDValueEnum.sitkUInt8)
                else
                    logger.Warning (sprintf "saving boolean image to %s; value 'true' is set to 255, not 1"  fname)
                    SimpleITK.RescaleIntensity(img,0.0,255.0)
            | _ -> img
    SimpleITK.WriteImage(tmp,filename)

let floatV (img : Image) = 
    let ptr = NativePtr.ofNativeInt<float32> (lock img <| fun () -> img.GetBufferAsFloat()) in
    let len = int (img.GetNumberOfPixels()) in
    new NativeArray<float32>(ptr,len,Some (img :> obj))
let uint8V (img : Image) = 
    let ptr = NativePtr.ofNativeInt<uint8> (lock img <| fun () -> img.GetBufferAsUInt8()) in
    let len = int (img.GetNumberOfPixels() * img.GetNumberOfComponentsPerPixel()) in
    new NativeArray<uint8>(ptr,len,Some (img :> obj))
let uint32V (img : Image) = 
    let ptr = NativePtr.ofNativeInt<uint32> (lock img <| fun () -> img.GetBufferAsUInt32()) in
    let len = int (img.GetNumberOfPixels()) in
    new NativeArray<uint32>(ptr,len,Some (img :> obj))

let emptyUint8 (img : Image) =
    use img2 = 
        if img.GetNumberOfComponentsPerPixel() = 1ul then new Image(img)
        else 
            use baseImg = SimpleITK.VectorIndexSelectionCast(img,0ul)
            new Image(baseImg)
    SimpleITK.Cast(img2,PixelIDValueEnum.sitkUInt8)    

let createUint8 (img : Image, value : uint8) =
    let res = emptyUint8(img)
    let buf = uint8V res
    buf.fill value
    res

let intensity (img : Image) =
            if img.GetNumberOfComponentsPerPixel() = 1ul
            then img  
            else  // TODO check color space correctly!!! Assumes it's rgb                
                let r = SimpleITK.VectorIndexSelectionCast(img,0ul)
                let g = SimpleITK.VectorIndexSelectionCast(img,1ul) 
                let b = SimpleITK.VectorIndexSelectionCast(img,2ul)
                // TODO: is the following correct? Source: https://en.wikipedia.org/wiki/Relative_luminance
                // the same formula is used in GIMP and called "Luminosity"
                let (rcoeff,gcoeff,bcoeff) = 0.2126,0.7152,0.0722
                SimpleITK.Add(SimpleITK.Multiply(rcoeff,r),SimpleITK.Add(SimpleITK.Multiply(gcoeff,r),SimpleITK.Multiply(bcoeff,r)))
let avg (img : Image) (mask : Image) = // TODO: type check that there is one component only
    // TODO: test if this function using arrays in place of lists is more efficient, and if there is any difference in the result
    let npixels = int (img.GetNumberOfPixels())
    let ncpp = img.GetNumberOfComponentsPerPixel ()
    if ncpp > 1ul
    then raise <| UnsupportedNumberOfComponentsPerPixelException (int ncpp)
    let imgv = floatV img
    let maskv = uint8V mask
    let mutable l = []
    for i = 0 to npixels - 1 do
        if maskv.Get i > 0uy then
            l <- (imgv.Get i)::l    
    float (List.average l)

let tt img = createUint8(img,1uy)

let ff img = createUint8(img,0uy)

let border (img : Image) = 
    // TODO: make this faster by first filling the result with zeroes and then iterating only over the borders
    let szv = img.GetSize()
    let dim = szv.Count
    let sz = Array.init dim (fun i -> int szv.[i])
    let res = emptyUint8(img)
    let buf = uint8V res               
    let coords = Array.create dim 0  
    let inline decode (size : array<int>) (idx : int) (res : array<int>) =        
        let mutable div = idx
        for i = 0 to size.Length - 1 do
            res.[i] <- div % size.[i]
            div <- div / size.[i]    
    buf.replace <|
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

let flood (img1 : Image) (img2 : Image) =
    use flt = new ConnectedComponentImageFilter()
    flt.SetFullyConnected(true)    
    use cc1 = flt.Execute(img2)    
    let k = int <| flt.GetObjectCount()    
    let ccs = Array.create (k+1) false    
    use n1 = near img1
    let bufcc1 = uint32V cc1
    let bufn1 = uint8V n1
    let npixels = int (img1.GetNumberOfPixels())    
    for i = 0 to npixels - 1 do
        if bufn1.UGet i > 0uy then
            let cc = bufcc1.UGet i
            if cc > 0ul then ccs.[int cc] <- true
    let res = new Image(img1)
    let bufres = uint8V res
    for i = 0 to npixels - 1 do
        let cc = bufcc1.UGet i
        if cc > 0ul && ccs.[int cc] then // TODO: is the check "cc > 0ul" necessary? ccs.[0] should be "false" anyway. Also, can we directly use 1uy and 0uy instead of true and false, and write "bufres.USet i ccs.[int cc]"?
            bufres.USet i 1uy
    res   

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

let reachesOlder (img1 : Image) (img2 : Image) =
    use flt = new ConnectedComponentImageFilter()
    flt.SetFullyConnected(true)
    use cc1 = flt.Execute(img2)
    let k = int <| flt.GetObjectCount()
    let ccs = Array.create (k+1) 0uy
    use n1 = near img1 
    let bufcc1 = uint32V cc1
    let bufn1 = uint8V n1
    let npixels = int (img1.GetNumberOfPixels())
    for i = 0 to npixels - 1 do
        if bufn1.UGet i > 0uy 
        then
            let cc = bufcc1.UGet i
            if bufcc1.UGet i > 0ul then ccs.[int cc] <- 1uy
    let res = new Image(img1)
    let bufres = uint8V res
    for i = 0 to npixels - 1 do        
        let cc = int (bufcc1.UGet i)
        let cval = ccs.[cc]
        if cval > 0uy then bufres.USet i cval
    res    

let reaches (img1 : Image) (img2 : Image) =
    use flt = new ConnectedComponentImageFilter()
    flt.SetFullyConnected(true)
    use cc2 = flt.Execute(img2)
    let k = int <| flt.GetObjectCount()
    let ccs = Array.create (k+1) 0uy
    use n1 = near img1
    use m1 = mask cc2 n1
    let bufm1 = uint32V m1
    let bufcc1 = uint32V cc2
    let npixels = int (img1.GetNumberOfPixels())    
    for i = 0 to npixels - 1 do
        let cc = int (bufm1.UGet i)
        if cc > 0 then ccs.[cc] <- 1uy        
    let res = new Image(img1)
    let bufres = uint8V res
    for i = 0 to npixels - 1 do        
        let cc = int (bufcc1.UGet i)
        let cval = ccs.[cc]
        if cval > 0uy then bufres.USet i cval
    res    

/// <summary>Creates a 1-dimensional array <c>a</c> with as many elements as the voxels in a hyperrectangle with dimensions specified by <c>hyperRadius</c>.
/// The i-th dimension is obtained as <c>2*hyperRadius[i]+1</c>. The created array is only useful in a image <c>img</c> of dimensions as specified in the <c>size</c> parameter. 
/// Each element <c>a[i]</c> is a "displacement". Given a linear coordinate <c>x</c>, the linear coordinate <c>x+a[i]</c>, if it lies in <c>0..n</c> where <c>n</c> is the number of voxels, is the coordinate of a point in the hyperrectangle centered at <c>x</c> (and of the specified dimensions).
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
let hyperrectangle (size : array<int>) (hyperRadius : array<int>) = 
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
let r : array<float> -> array<float> -> float = 
    fun h2 ->
    // computes cross-correlation between two histograms; the first argument can be curried for a speedup
        let avg2 = Array.average h2
        let sqrtDen2 = sqrt (Array.sumBy (fun n -> (n - avg2)**2.0) h2)
        fun (h1 : array<float>) ->
            assert (h1.Length = h2.Length)        
            let avg1 = Array.average h1        
            let den1 = Array.sumBy (fun n -> (n - avg1)**2.0) h1
            if den1 = 0.0 && sqrtDen2 = 0.0 then 1.0
            else 
                if den1 = 0.0 || sqrtDen2 = 0.0 then 0.0
                else     
                    let num = Array.fold2 (fun acc n1 n2 -> acc + ((n1-avg1)*(n2-avg2))) 0.0 h1 h2 // TODO: PRECOMPUTE n2 - avg2
                    let den = ((sqrt den1)*sqrtDen2)      
                    let res = num / den
                    res



let private mkDelta m1 m2 k =
    (m2 - m1) / (float k)

/// <summary>
/// Adds <c>increment</c> to the value in <c>histogram</c> corresponding to the bin of <c>value</c> with extremes <c>m1</c> and <c>m2</c>, and interval size <c>delta</c> 
/// </summary>
/// <remarks>
/// <c>delta</c> must be equal to <c>(m2 - m1) / l</c> where <c>l</c> is the number of elements of <c>histogram</c>
/// </remarks>
let inline private bin (m1 : float,m2 : float,delta : float, increment : float) (value : float)  (histogram : array<float>) =
    assert (delta = mkDelta m1 m2 histogram.Length)
    if value >= m1 && value < m2 then // TODO: can this bounds checking be omitted if the image is pre-thresholded?
        let histIdx = int ((value - m1) / delta) //TODO: do we want "int (floor (value - m1 / delta)) here?"
        histogram.[histIdx] <- (histogram.[histIdx] + increment)
    

///<summary>
/// Applies <c>fn</c> to the linear coordinates of all points in a Hamiltonian path over an image of dimension <c>size</c> **except the origin 0**; 
/// the first argument passed to <c>fn</c> is the linear coordinate of the *previous* point (which can be 0)
/// the second argument passed to <c>fn</c> is the linear coordinate of the *current* point
/// the third argument passed to <c>fn</c> is an integer that represents the direction of movement:
/// the modulus of the third argument is the index of the dimension of movement (1 is first dimension)
/// the sign of the second argument is the direction (positive, negative).
///</summary>
let snake (size : array<int>) (fn : int -> int -> int -> unit) = 
    
    let direction = Array.create size.Length 1
    let dimensionalCursor = Array.create size.Length 0
    let ndims = size.Length

    let displacements = Array.copy size
    displacements.[0] <- 1
    for i = 1 to ndims - 1 do
        displacements.[i] <- displacements.[i-1] * size.[i-1] // displacements.[i] is the difference between the linear coordinates of a point, and another point off by one in the i-th dimension.    
    let tmp = Array.create ndims 0
    let mutable linearCursor = 0
    let mutable prevCursor = -1
    let inline doprod () =
        Array.iteri2 (fun i v1 v2 -> tmp.[i] <- v1 * v2) dimensionalCursor displacements 
        prevCursor <- linearCursor
        linearCursor <- Array.sum tmp            

    let inline inc () =
        let mutable res = 0
        let mutable n = 0
        while n < ndims do    
            let d = direction.[n]
            let x = (dimensionalCursor.[n] + d)
                       
            if x < 0 || x >= size.[n] then                
                direction.[n] <- -d
                n <- n + 1
            else 
                res <- d * (n+1)
                dimensionalCursor.[n] <- x
                n <- ndims + 1
        doprod()            
        (n = ndims,res)

    let mutable state = inc()
    while not (fst state) do
        fn linearCursor prevCursor (snd state)
        state <- inc ()                     

let memo fn =
    let h = new System.Collections.Generic.Dictionary<_,_>() 
    fun x ->
        try h.[x]
        with _ -> 
            let y = fn x in
            h.[x] <- y
            y

let hamiltonianPath = // takes a list as input for memoisation (arrays have reference equality) // TODO: consider adding this to the model interface instead of memoizing
    memo (fun size ->
            let nelems= (List.fold (*) 1 size) - 1
            let path = Array.create nelems (0,0,0)
            let mutable idx = 0
            snake (Array.ofList size) (fun x y z -> path.[idx] <- (x,y,z); idx <- idx + 1)
            path)

/// <summary>Implements the "crossCorrelation" / "statistical comparison (SCMP)" operator
/// </summary>
/// 
let crosscorrelation (rad : float) (a : Image) (b : Image) (fb : Image) (m1 : float) (m2 : float) (k : float) = 
    job {   let dims = a.GetSpacing()
            let ballRadius = Array.create dims.Count 0
            for i = 0 to dims.Count - 1 do ballRadius.[i] <- int (round (rad / (float dims.[i]))) // Compute anisotropic voxel radiuses from real-world radius            
            let size = Array.ofSeq (Seq.map int (a.GetSize()))  
            let indices,faces = hyperrectangle size ballRadius            
            let npixels = int (a.GetNumberOfPixels())
            let nbins = int k
            let delta = mkDelta m1 m2 nbins

            let bufa = floatV a

            let r' = // Curried cross-correlation where the global histogram has been pre-computed
                let bigHistogram = Array.create nbins 0.0 // Global histogram to compare to
                
                let bufb = floatV b
                let buffb = uint8V fb    
                for linearCoord = 0 to npixels - 1 do // Fill big histogram
                    let vfb = buffb.UGet linearCoord        
                    if vfb > 0uy then 
                        let vb = float (bufb.UGet linearCoord)            
                        bin (m1,m2,delta,1.0) vb bigHistogram  
                r bigHistogram

            let inline doThing localHistogram center increment el = 
                let linearCoord = center + el
                if (linearCoord >= 0) && (linearCoord < npixels) 
                then 
                    let va = float (bufa.UGet linearCoord) // TODO: use integer arithmetics and power-of-two deltas
                    bin (m1,m2,delta,increment) va localHistogram
            
            let res = new Image(a)
            let bufres = floatV res  

            let init center = // Initialize local histogram, and sets the cross-correlation value, returns the local histogram
                let localHistogram = Array.create nbins 0.0 
                Array.iter (doThing localHistogram center 1.0) indices 
                bufres.USet center (float32 (r' localHistogram))
                localHistogram
                   
            let inline fn localHistogram center previous direction =
                let faceIdx = (abs direction) - 1
                let facepair = faces.[faceIdx]
                let face1,face2 = facepair.[0],facepair.[1]
                let (faceMinus,facePlus) = if direction > 0 then (face1,face2) else (face2,face1)
                List.iter (doThing localHistogram previous -1.0) faceMinus
                List.iter (doThing localHistogram center 1.0) facePlus
                bufres.USet center (float32 (r' localHistogram))

            let h = hamiltonianPath (List.ofArray size) // TODO: this takes time because of array allocation and such. Can we do "snake" already from a start to an end point? It's tricky.            
            let nprocs = System.Environment.ProcessorCount            
            let fragsize = h.Length / nprocs

            let mkJob procindex =
                job {   let fragstart = procindex * fragsize    
                        let localHistogram = init fragstart
                        for pos = fragstart to min (fragstart + fragsize - 1) (h.Length - 1) do
                        let (center,previous,direction) = h.[pos]
                        fn localHistogram center previous direction }

            do! Util.Concurrent.doParallel (Array.init nprocs mkJob)
            return res  }


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

let percentiles (img : Image) (mask : Image) =
    let bufimg = floatV img
    let bufmask = uint8V mask
    let npixels = int <| img.GetNumberOfPixels()
    let population = Seq.filter (fun i -> bufmask.UGet i > 0uy) (seq {0..npixels - 1})
    let data =         
        population  |> 
        Seq.groupBy bufimg.UGet |> 
        Seq.sortBy fst |>
        Seq.map (fun (key,indices) -> (Seq.length indices,indices))
    let res = SimpleITK.Mask(img,mask,-1.0)
    let bufres = floatV res
    let mutable curvol = 0
    let vol = float32 (Seq.length population)
    for (size,indices) in data do
        for idx in indices do
            bufres.USet idx ((float32 curvol) / vol)
        curvol <- curvol + size        
    res    
