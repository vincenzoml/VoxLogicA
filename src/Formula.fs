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

exception TypeErrorException of op : string * got : array<string> * wanted : array<string> 
    with override this.Message = sprintf "%s requires %s, called with %s" this.op (Type.StringOfArgumentsStringType this.got) (Type.StringOfArgumentsStringType this.wanted)

exception WrongNumberOfArgumentsException of op : string * got : int * wanted : int 
    with override this.Message = sprintf "%s requires %d arguments, got %d" this.op this.got this.wanted

type Formula (operator : Operator, arguments : array<Formula>,uid : int) =
    member __.Uid = uid
    member __.Operator = operator
    member __.Arguments = arguments    

and FormulaFactory() =
    let uif = new UniqueFactory<_,_>()
   
    let checkType (operator : Operator) (arguments : array<Formula>) =
        if operator.Argtype.Length <> arguments.Length 
        then raise (WrongNumberOfArgumentsException(operator.Name,operator.Argtype.Length,arguments.Length)  )
        if not (Seq.forall2 (=) operator.Argtype (Seq.map (fun (f : Formula) -> f.Operator.Rettype) arguments)) 
        then raise <| 
                TypeErrorException(operator.Name,Array.map string operator.Argtype,Array.map (fun (f : Formula) -> f.Operator.Rettype.ToString()) arguments)

    member __.Create (operator : Operator) (arguments : array<Formula>) =        
        checkType operator arguments
        let arguments = 
            if operator.Commutative
            then Array.sortBy (fun (x : Formula) -> x.Uid) arguments 
            else arguments
        let kargs = List.ofArray <| Array.mapi (fun _ (phi : Formula) -> phi.Uid) arguments 
        // kargs is a list as a key instead of an array for consistent hashing; don't change this without changing also the dictionary comparer in UniqueFactory
        let key = (operator.Name,kargs)
        // ErrorMsg.Logger.DebugOnly <| sprintf "FormulaFactory.Create: key: %A" key
        let (uid,phi) = uif.Add(key,fun uid -> Formula(operator, arguments, uid))        
        assert(uid = phi.Uid)
        phi

    member this.CreateConst (x,t) = this.Create (Constant(x,t)) [||]  
 
    member __.Count = uif.Count    
    member __.Item i = uif.[i]

    member this.AsDot =
        let mutable str = "digraph {"
        for i = 0 to this.Count-1 do
            let item = this.[i]            
            str <- str + sprintf "%d [label=\"%s [%d]\"];\n" item.Uid item.Operator.Name item.Uid
            for target in item.Arguments do                
                str <- str + sprintf "%d -> %d;\n" item.Uid target.Uid 
        str + "\n}"