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

namespace VoxLogicA

open Hopac

type ILogicModel<'Value> = // empty interface used to constrain all implemented logic fragments to the same 'Value type
    interface
    end

type IBooleanModel<'Value when 'Value : equality> =  
    inherit ILogicModel<'Value>
    [<OperatorAttribute("bconstant","bool","valuation(bool)","The image which has the given boolean value at each voxel")>]
    abstract member BConst : bool -> Job<'Value>
    [<OperatorAttribute("tt","valuation(bool)","The image which is true at each voxel")>]
    abstract member TT : Job<'Value>
    [<OperatorAttribute("ff","valuation(bool)","The image which is false at each voxel")>]
    abstract member FF : Job<'Value>
    [<OperatorAttribute("not","valuation(bool)","valuation(bool)","Boolean negation of each voxel")>]
    abstract member Not : 'Value -> Job<'Value>
    [<OperatorAttribute("and",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)",true,"Boolean and voxel-wise")>]
    abstract member And : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("or",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)",true,"Boolean or voxel-wise")>]
    abstract member Or : 'Value -> 'Value -> Job<'Value>

type IDistanceModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("dt","valuation(bool)","valuation(number)","Euclidean distance transform of its argument: replaces each voxel with the positive (or 0) distance from the nearest voxel which is true in the argument.")>]
    abstract member DT : 'Value -> Job<'Value>

type IQuantitativeModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("constant","number","valuation(number)","The image which has the given value at each voxel")>]
    abstract member Const : float -> Job<'Value>
    [<OperatorAttribute(".=",[|"number";"valuation(number)"|],"valuation(bool)","eq(n,i) is true at voxels of i that are equal to n")>]    
    abstract member EqSV : float -> 'Value -> Job<'Value>
    [<OperatorAttribute(".<=",[|"number";"valuation(number)"|],"valuation(bool)","n .<= img is true at voxels of img that are greater than or equal to n")>]    
    abstract member LeqSV : float -> 'Value -> Job<'Value>
    [<OperatorAttribute(".>=",[|"number";"valuation(number)"|],"valuation(bool)","n .>= img is true at voxels of img that are less than or equal to n")>]    
    abstract member GeqSV : float -> 'Value -> Job<'Value>
    [<OperatorAttribute("between",[|"number";"number";"valuation(number)"|],"valuation(bool)","between(n1,n2,i) is true at voxels of i that are greater than or equal to n1, and less than or equal to n2")>]
    abstract member Between : float -> float -> 'Value -> Job<'Value>
    //[<OperatorAttribute("max","valuation(number)","number","Finds the maximum value among the voxels in its argument")>]
    //abstract member Max : 'Value -> Job<float>
    [<OperatorAttribute("abs","valuation(number)","valuation(number)","Computes an image where each voxel contains the absolute value of the corresponding voxel in the input image")>]
    abstract member Abs : 'Value -> Job<'Value>
    // [<OperatorAttribute("min","valuation(number)","number","Finds the minimum value among the voxels in its argument")>]
    // abstract member Min : 'Value -> Job<float>
    [<OperatorAttribute("+",[|"valuation(number)";"valuation(number)"|],"valuation(number)","Voxel-wise addition")>]    
    abstract member AddVV : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("*",[|"valuation(number)";"valuation(number)"|],"valuation(number)","Voxel-wise multiplication")>]    
    abstract member MultiplyVV : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("/",[|"valuation(number)";"valuation(number)"|],"valuation(number)","Voxel-wise division")>]    
    abstract member DivideVV : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("-",[|"valuation(number)";"valuation(number)"|],"valuation(number)","Voxel-wise subtraction")>]    
    abstract member SubtractVV : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("mask",[|"valuation(number)";"valuation(bool)"|],"valuation(number)","mask(img,bimg) has value 0 at voxels that are false in bimg, and the same value of img at voxels that are true in bimg")>]    
    abstract member Mask : 'Value -> 'Value -> Job<'Value>
    //[<OperatorAttribute("avg",[|"valuation(number)";"valuation(bool)"|],"number","avg(img,bimg) is the average of the values of img at voxels that are true in bimg")>]
    //abstract member Avg : 'Value -> 'Value -> Job<float>
    [<OperatorAttribute("./",[|"number";"valuation(number)"|],"valuation(number)","divides each voxel by a constant")>]    
    abstract member DivSV : float -> 'Value  -> Job<'Value>    
    [<OperatorAttribute(".-",[|"number";"valuation(number)"|],"valuation(number)","subtracts a constant from each voxel")>]    
    abstract member SubSV : float -> 'Value -> Job<'Value>    
    [<OperatorAttribute("/.",[|"valuation(number)";"number"|],"valuation(number)","divides each voxel by a constant")>]    
    abstract member DivVS : 'Value -> float -> Job<'Value>    
    [<OperatorAttribute("-.",[|"valuation(number)";"number"|],"valuation(number)","subtracts a constant from each voxel")>]    
    abstract member SubVS : 'Value -> float -> Job<'Value>    
    [<OperatorAttribute("+.",[|"valuation(number)";"number"|],"valuation(number)","adds a constant to each voxel")>]    
    abstract member AddVS : 'Value -> float -> Job<'Value>    
    [<OperatorAttribute("*.",[|"valuation(number)";"number"|],"valuation(number)","multiplies each voxel by a constant")>]    
    abstract member MulVS : 'Value -> float -> Job<'Value>    

type ISpatialModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("near","valuation(bool)","valuation(bool)","Spatial-logical closure (that is, dilation)")>]
    abstract member Near : 'Value -> Job<'Value>
    [<OperatorAttribute("interior","valuation(bool)","valuation(bool)","Spatial-logical interior (that is, erosion)")>]
    abstract member Interior : 'Value -> Job<'Value>
    [<OperatorAttribute("through",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)","through(img1,img2) is true at voxel x if there is a path p, starting in x and ending in a voxel y, with y true in img1, and all points of p (including extremes) true in img2")>]
    abstract member Through : 'Value -> 'Value -> Job<'Value>
    
type IStatisticalModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("crossCorrelation",[|"number";"valuation(number)";"valuation(number)";"valuation(bool)";"number";"number";"number"|],"valuation(number)","crossCorrelation(radius,local,target,mask,min,max,nbins) computes similarity scores via statistical cross-correlation (see academic papers or extended documentation)")>]
    abstract member CrossCorrelation : float -> 'Value -> 'Value -> 'Value -> float -> float -> float -> Job<'Value>
    
type IBoundedModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("border",[||],"valuation(bool)","True at voxels in the border of the image")>]
    abstract member Border : Job<'Value>

type IImageModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("intensity","model","valuation(number)","The intensity  of an image. For RGB images this is computed with the well known colorimetric formula.")>]
    abstract member Intensity : 'Value -> Job<'Value>
    [<OperatorAttribute("red","model","valuation(number)","The red component of an image. For grayscale images this is equal to the intensity")>]
    abstract member Red : 'Value -> Job<'Value>
    [<OperatorAttribute("green","model","valuation(number)","The green component of an image. For grayscale images this is equal to the intensity")>]
    abstract member Green : 'Value -> Job<'Value>
    [<OperatorAttribute("blue","model","valuation(number)","The blue component of an image. For grayscale images this is equal to the intensity")>]
    abstract member Blue : 'Value -> Job<'Value>
    [<OperatorAttribute("alpha","model","valuation(number)","The alpha channel of an image. If there is no alpha channel, a constant image with all voxels equal to 255 is returned")>]
    abstract member Alpha : 'Value -> Job<'Value>
    [<OperatorAttribute("volume","valuation(bool)","number","The number of voxels that are true in the given image")>]
    abstract member Volume : 'Value -> Job<float>
    //[<OperatorAttribute("maxvol","valuation(bool)","valuation(bool)","The connected component of the given image with maximum volume (if more components have the same maximum volume, their union is returned)")>]
    //abstract member MaxVol : 'Value -> Job<'Value>
    [<OperatorAttribute("percentiles",[|"valuation(number)";"valuation(bool)";"number"|],"valuation(number)","Each voxel in percentiles(img,bimg,k) is the percentile rank, between 0 and 1, of its value in img, considering only voxels that are true in bimg (voxels that are false in bimg are assigned value 0); the rank is not rounded, and it is corrected with k * n where n is the number of voxels equal to the considered one.")>]
    abstract member Percentiles : 'Value -> 'Value -> float -> Job<'Value>
    [<OperatorAttribute("rgb",[|"valuation(number)";"valuation(number)";"valuation(number)"|],"model","Creates a RGB image given the red, green, and blue components")>]
    abstract member RGB : 'Value -> 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("rgba",[|"valuation(number)";"valuation(number)";"valuation(number)";"valuation(number)"|],"model","Creates a RGBA image given the red, green, blue, and alpha components")>]
    abstract member RGBA : 'Value -> 'Value -> 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("lcc","valuation(bool)","valuation(number)","Labels connected components of a boolean model. The result is a quantitative model with connected components labelled consecutively starting from 1, and with the background labelled with 0")>]
    abstract member LCC : 'Value -> Job<'Value>
    
    
    