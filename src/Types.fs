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
open FParsec

type UnsupportedTypeException(t : System.Type) =
    inherit BugException(sprintf "System type '%s' cannot be converted to VoxLogicA type (you should improve Types.fs to change this)" t.Name)

type Type = 
    TModel | TNumber |TBool | TString |TValuation of Type
    static member OfSystemType t =
        if t = typeof<float32> then TNumber
        else if t = typeof<bool> then TBool
        else if t = typeof<string> then TString
        else raise (UnsupportedTypeException t)
    static member Parse(s : string) = 
        let parser,parserImpl = createParserForwardedToRef()
        parserImpl :=           
                choice [
                    skipString "number" >>% TNumber
                    skipString "bool" >>% TBool
                    skipString "string" >>% TString
                    skipString "model" >>% TModel
                    skipString "valuation" >>. between (skipString "(") (skipString ")") parser |>> TValuation
                ]            
        match (run parser s) with  
            | Success (t,_,_) -> t
            | Failure _ as f -> 
                let xn = BugException (sprintf "parse error in type declaration %s; the parser reported %A" s f)
                xn.Data.[0] <- f
                raise xn
                
    override this.ToString() =
        match this with
            | TModel -> "model"
            | TNumber -> "number"
            | TBool -> "bool"
            | TString -> "string"
            | TValuation(t)-> sprintf "valuation(%s)" (t.ToString())
    static member StringOfArgumentsStringType (ta : array<string>) =      // TODO: can this be generalised and moved to Util?  
        if ta.Length = 0 then ""
        else if ta.Length = 1 then sprintf "%s" (ta.[0].ToString())
        else 
            let mutable res = sprintf "("
            for i = 0 to ta.Length - 2 do res <- res + (ta.[i].ToString()) + ","
            res <- res + ta.[ta.Length - 1].ToString() + ")"
            res
            