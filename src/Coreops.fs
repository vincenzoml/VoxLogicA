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

/// Implementation of several operators that can be evaluated without knowing the model type
type Coreops() =
    [<OperatorAttribute("./.",[|"number";"number"|],"number","Floating point division")>]
    member __.Div (a : float) (b: float) : Job<float> = job { return a / b }

    [<OperatorAttribute(".*.",[|"number";"number"|],"number","Floating point multiplication")>]
    member __.Mult (a : float) (b: float) : Job<float> = job { return a * b }

    
    [<OperatorAttribute(".+.",[|"number";"number"|],"number","Floating point addition")>]
    member __.Sum (a : float) (b: float) : Job<float> = job { return a + b }

    [<OperatorAttribute(".-.",[|"number";"number"|],"number","Floating point subtraction")>]
    member __.Sub (a : float) (b: float) : Job<float> = job { return a - b }

    [<OperatorAttribute(".&.",[|"bool";"bool"|],"bool","Boolean and")>]
    member __.And (a : bool) (b: bool) : Job<bool> = job { return a && b }

    [<OperatorAttribute(".|.",[|"bool";"bool"|],"bool","Boolean or")>]
    member __.Or (a : bool) (b: bool) : Job<bool> = job { return a || b }

    [<OperatorAttribute("!.",[|"bool"|],"bool","Boolean not")>]
    member __.Not (a : bool) : Job<bool> = job { return not a }

    [<OperatorAttribute(".=.",[|"number";"number"|],"bool","Equality")>]
    member __.Eq (a : float) (b: float) : Job<bool> = job { return (a = b) }
    
    [<OperatorAttribute(".<=.",[|"number";"number"|],"bool","Less or equal than")>]
    member __.Leq (a : float) (b: float) : Job<bool> = job { return (a <= b) }

    [<OperatorAttribute(".<.",[|"number";"number"|],"bool","Less than")>]
    member __.Lt (a : float) (b: float) : Job<bool> = job { return (a < b) }

    [<OperatorAttribute(".>=.",[|"number";"number"|],"bool","Greater or equal than")>]
    member __.Geq (a : float) (b: float) : Job<bool> = job { return (a >= b) }

    [<OperatorAttribute(".>.",[|"number";"number"|],"bool","Greater than")>]
    member __.Gt (a : float) (b: float) : Job<bool> = job { return (a > b) }
