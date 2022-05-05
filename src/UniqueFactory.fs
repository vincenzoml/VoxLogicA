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

exception UniqueFactoryException
    with override this.Message = "Internal error in module UniqueFactory.fs"

open System
type UniqueFactory<'Key,'Value when 'Key : equality>() =
    let mutable curId = 0
    let dict = new System.Collections.Generic.Dictionary<'Key,(int * 'Value)>()
    let mutable values = Array.create 1000 None
  
    let create k maker =         
        let newId = curId
        curId <- curId + 1 // The whole application crucially depends on the fact that ids are allocated sequentially.
        let created = maker newId
        dict.[k] <- (newId,created)
        let l = values.Length
        if newId >= l 
        then values <- Array.init (2 * l) (fun id -> if id < l then values.[id] else None)
        values.[newId] <- Some created
        (newId,created)    
    
    //member __.Values = Array.ofSeq (Seq.map snd (Seq.sortBy fst dict.Values)) // TODO do we need to keep this stored instead of computing it each time?

    member __.Count = curId
    member __.Item i = 
        if i < curId 
        then match values.[i] with 
                | None -> raise UniqueFactoryException 
                | Some x -> x
        else raise (IndexOutOfRangeException())
        
    member __.Add(k,maker) = // returns the created element
        if dict.ContainsKey(k) 
        then dict.[k]
        else create k maker
