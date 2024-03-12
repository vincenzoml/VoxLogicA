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

open System.IO
open Hopac


exception CantSaveException of t : Type * s : string
    with override this.Message = sprintf "Saving expression of type %s to file \"%s\" is not supported" (this.t.ToString()) this.s

exception CantLoadException of s : string
    with override this.Message = sprintf "Loading %s is not supported" this.s

[<AbstractClass>]
type IModel() =
    inherit Coreops()
    abstract member Load : string -> obj
    abstract member Save : string -> obj -> unit 
    abstract member CanSave : Type -> string -> bool
    abstract member OnExit : unit -> unit
