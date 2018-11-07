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

open System.Collections.Generic
open Hopac

type OperatorAttribute (name : string, argtype : string array, rettype : string, commutative : bool) =
    let at = Array.map Type.Parse argtype
    let rt = Type.Parse rettype
    member __.Name = name
    member __.Commutative = commutative
    member __.Argtype = at
    member __.Rettype = rt
    override __.ToString() = sprintf "%s : %A -> %A" name argtype rettype
    new (name, argtype, rettype) = new OperatorAttribute(name,argtype,rettype,false)
    new (name, rettype) = new OperatorAttribute(name,[||],rettype,false)    
    new (name, argtype, rettype) = new OperatorAttribute(name,[|argtype|],rettype,false)


type Operator(name : string, argtype : array<Type>, rettype : Type, fn : array<obj> -> Job<obj>,commutative : bool,constant : bool) =
    do if commutative && argtype.Length > 0 then 
        if Array.exists (fun t -> t <> argtype.[0]) argtype then 
            raise (BugException(sprintf "Operator %s declared as commutative, but the type of its arguments is not the same" name))
    member __.Constant = constant
    member __.Commutative = commutative           
    member __.Name = name
    member __.Argtype = argtype
    member __.Rettype = rettype
    member __.Eval : array<obj> -> Job<obj> = fn 
    new (name,argtype,rettype,eval,commutative) = new Operator(name,argtype,rettype,eval,commutative,false)
    new (name,argtype,rettype,eval) = new Operator(name,argtype,rettype,eval,false,false)    

and Constant(x,t) =
    inherit Operator(x.ToString(),[||],t,(fun _ -> Job.result (x :> obj)),false,true)

and OperatorFactory() =
    let dict = new Dictionary<string,Operator>(1000)    
    static member private mkJobObj<'a> (j : Job<'a>) = 
                    job {   let! v = j
                            return (v :> obj) }    
    member __.Item name = dict.[name]
    

    member __.Create name argtype rettype fn commutative =        
        if dict.ContainsKey name
        then raise (BugException (sprintf "operator not unique %s" name))
        else dict.[name] <- new Operator(name,argtype,rettype,fn,commutative)

    new(obj : obj) as this =
        new OperatorFactory() 
        then                                                    
            // Iterates over all methods
            let t = obj.GetType()                           
            let meths = Seq.collect (fun (t : System.Type) -> t.GetMethods()) (Seq.append [t] (t.GetInterfaces()))                                 
            for m in meths do    
                for attr in m.GetCustomAttributes(typeof<OperatorAttribute>,true) do 
                    let opattr = attr :?> OperatorAttribute
                    this.Create opattr.Name opattr.Argtype opattr.Rettype (Util.Concurrent.CastMemberToJob (obj,m)) opattr.Commutative
                
            // Iterates over all properties (zeroary operators)
            let props = Seq.collect (fun (t : System.Type) -> t.GetProperties()) (Seq.append [t] (t.GetInterfaces()))                        
            for p in props do
                for attr in p.GetCustomAttributes(typeof<OperatorAttribute>,true) do
                    let opattr = attr :?> OperatorAttribute                            
                    this.Create opattr.Name opattr.Argtype opattr.Rettype (Util.Concurrent.CastPropertyToJob (obj,p)) opattr.Commutative
                    