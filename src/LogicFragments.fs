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
    [<OperatorAttribute("tt","valuation(bool)")>]
    abstract member TT : Job<'Value>
    [<OperatorAttribute("ff","valuation(bool)")>]
    abstract member FF : Job<'Value>
    [<OperatorAttribute("not","valuation(bool)","valuation(bool)")>]
    abstract member Not : 'Value -> Job<'Value>
    [<OperatorAttribute("and",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)",true)>]
    abstract member And : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("or",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)",true)>]
    abstract member Or : 'Value -> 'Value -> Job<'Value>

type IDistanceModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("dt","valuation(bool)","valuation(number)")>]
    abstract member DT : 'Value -> Job<'Value    >

type IQuantitativeModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("eq",[|"number";"valuation(number)"|],"valuation(bool)")>]
    abstract member Eq : float -> 'Value -> Job<'Value>
    [<OperatorAttribute("geq",[|"number";"valuation(number)"|],"valuation(bool)")>]    
    abstract member Geq : float -> 'Value -> Job<'Value>
    [<OperatorAttribute("leq",[|"number";"valuation(number)"|],"valuation(bool)")>]    
    abstract member Leq : float -> 'Value -> Job<'Value>
    [<OperatorAttribute("between",[|"number";"number";"valuation(number)"|],"valuation(bool)")>]
    abstract member Between : float -> float -> 'Value -> Job<'Value>
    [<OperatorAttribute("max","valuation(number)","number")>]
    abstract member Max : 'Value -> Job<float>
    [<OperatorAttribute("min","valuation(number)","number")>]
    abstract member Min : 'Value -> Job<float>
    [<OperatorAttribute("subtract",[|"valuation(number)";"valuation(number)"|],"valuation(number)")>]    
    abstract member Subtract : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("mask",[|"valuation(number)";"valuation(bool)"|],"valuation(number)")>]    
    abstract member Mask : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("avg",[|"valuation(number)";"valuation(bool)"|],"number")>]
    abstract member Avg : 'Value -> 'Value -> Job<float>
    [<OperatorAttribute("sdiv",[|"valuation(number)";"number"|],"valuation(number)")>]    
    abstract member Sdiv : 'Value -> float -> Job<'Value>    

type ISpatialModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("near","valuation(bool)","valuation(bool)")>]
    abstract member Near : 'Value -> Job<'Value>
    [<OperatorAttribute("interior","valuation(bool)","valuation(bool)")>]
    abstract member Interior : 'Value -> Job<'Value>
    [<OperatorAttribute("flood",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)")>]
    abstract member Flood : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("reaches",[|"valuation(bool)";"valuation(bool)"|],"valuation(bool)")>]
    abstract member Reaches : 'Value -> 'Value -> Job<'Value>
    
type IStatisticalModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("crossCorrelation",[|"number";"valuation(number)";"valuation(number)";"valuation(bool)";"number";"number";"number"|],"valuation(number)")>]
    abstract member CrossCorrelation : float -> 'Value -> 'Value -> 'Value -> float -> float -> float -> Job<'Value>
    
type IBoundedModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("border",[||],"valuation(bool)")>]
    abstract member Border : Job<'Value>

type IImageModel<'Value when 'Value : equality> =
    inherit ILogicModel<'Value>
    [<OperatorAttribute("intensity","model","valuation(number)")>]
    abstract member Intensity : 'Value -> Job<'Value>
    [<OperatorAttribute("red","model","valuation(number)")>]
    abstract member Red : 'Value -> Job<'Value>
    [<OperatorAttribute("green","model","valuation(number)")>]
    abstract member Green : 'Value -> Job<'Value>
    [<OperatorAttribute("blue","model","valuation(number)")>]
    abstract member Blue : 'Value -> Job<'Value>
    [<OperatorAttribute("alpha","model","valuation(number)")>]
    abstract member Alpha : 'Value -> Job<'Value>
    [<OperatorAttribute("volume","valuation(bool)","number")>]
    abstract member Volume : 'Value -> Job<float>
    [<OperatorAttribute("maxvol","valuation(bool)","valuation(bool)")>]
    abstract member MaxVol : 'Value -> Job<'Value>
    [<OperatorAttribute("percentiles",[|"valuation(number)";"valuation(bool)"|],"valuation(number)")>]
    abstract member Percentiles : 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("rgb",[|"valuation(number)";"valuation(number)";"valuation(number)"|],"valuation(number)")>]
    abstract member RGB : 'Value -> 'Value -> 'Value -> Job<'Value>
    [<OperatorAttribute("rgba",[|"valuation(number)";"valuation(number)";"valuation(number)";"valuation(number)"|],"valuation(number)")>]
    abstract member RGBA : 'Value -> 'Value -> 'Value -> 'Value -> Job<'Value>
    