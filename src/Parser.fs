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

module VoxLogicA.Parser

open FParsec

exception ParseErrorException of s : string
    with override this.Message = sprintf "Parse error: %s" this.s

type Expression = 
    | Call of Position * string * (Expression list)
    | Float of float
    | String of string

type ParsedItem = 
    | Declaration of string * (string list) * Expression
    | ModelLoad of string * string
    | ModelSave of Position * string * Expression 
    | Print of Position * string * Expression
    | Import of string

type private Program = ParsedItem list
let private commentLine = skipString "//" >>. skipRestOfLine true >>. spaces

let private spacesOrComment = spaces .>> skipMany commentLine <?> "whitespace"
let private spacesOrComment1 = (spaces1 <|> commentLine) .>> skipMany commentLine <?> "whitespace (required)"
let private ideFirstChar c = isLower c 
let private ideOtherChar c = isLetter c || isDigit c
let private ide = (many1Satisfy2 ideFirstChar ideOtherChar <?> "identifier") .>> spacesOrComment
let private operator = (many1Chars (upper <|> anyOf "#;:_'.|!$%&/^=*-+<>?@~\\") <?> "operator") .>> spacesOrComment
let private quotes p = between (pstring "\"") (pstring "\"") p .>> spacesOrComment
let private strConst = quotes (manyChars (noneOf "\"")) <?> "quoted string"
let private commaSepList p = sepBy1 p ((pstring ",") .>> spacesOrComment)
let private defeq = (pstring "=" .>> spacesOrComment)

let mybw b1 b2 p = (attempt b1) >>. p .>> b2
let private abrackets b1 b2 p = mybw (skipString b1 .>> spacesOrComment) (skipString b2 .>> spacesOrComment) p
let private brackets p = abrackets "(" ")" p
let private sqbrackets p = abrackets "[" "]" p
let private optlst p = opt p |>> Option.defaultValue []
let private farglist = brackets (commaSepList ide) <?> "formal arguments list" .>> spacesOrComment
    
let private parseExpression = 
        let (call,callImpl) : (Parser<Expression,unit> * (Parser<Expression,unit> ref)) = createParserForwardedToRef()            
        let simpleExpr = ((attempt pfloat) .>> spacesOrComment |>> Float) <|> ((attempt strConst) .>> spacesOrComment |>> String) <|> call 
        let (expr',exprImpl) : (Parser<Expression,unit> * (Parser<Expression,unit> ref)) = createParserForwardedToRef()
        let expr = expr' <?> "expression"
        let application = optlst (brackets (commaSepList expr)) <?> "actual arguments list"
        let opapplication = optlst (sqbrackets (commaSepList expr)) <?> "operator arguments list"
        callImpl := getPosition .>>. (attempt ide) .>>. application |>> (fun ((x,y),z) -> Call (x,y,z))
        exprImpl := 
            ((getPosition .>>. (attempt operator) .>>. expr) |>> (fun ((p,x),y) -> Call (p,x,[y])))
            <|> 
            ((attempt (getPosition .>>. (simpleExpr <|> brackets expr) .>>. operator) .>>. opapplication .>>. expr) |>> (fun ((((p,x),y),t),z) -> Call (p,y,[x;z]@t)))
            <|>            
            simpleExpr
            <|>
            (brackets expr)
        expr

let private command requireSpace s args =  attempt (skipString s) >>. (if requireSpace then spacesOrComment1 else spacesOrComment) >>. args .>> spacesOrComment
let private lhs = ide <|> operator
let private loadCommand = command true "load" ide .>> defeq .>>. strConst |>> ModelLoad
let private saveCommand = getPosition .>>. command false "save" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> ModelSave (x,y,z))
let private printCommand = getPosition .>>. command false "print" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> Print (x,y,z))

let private importCommand = command false "import" strConst |>> Import
let private letCommand = command true "let" (lhs .>>. optlst farglist .>>. (defeq >>. parseExpression)) |>> (fun ((x,y),z) -> Declaration (x,y,z))    
let private file contents = spacesOrComment >>. contents .>> eof
let private import = file <| many (importCommand <|> letCommand) 
let private program = file <| many (choice [loadCommand; saveCommand; printCommand; importCommand; letCommand])
let private getResult res = 
    match res with  
        | Success (result,_,_) -> result
        | Failure(msg,_,_) -> raise (ParseErrorException msg)
let private runParser name p stream =
    getResult (runParserOnStream p () name stream System.Text.Encoding.Default) 
let parseProgram name stream =  runParser name program stream
let  parseImport filename =
    use stream = new System.IO.FileStream(filename,System.IO.FileMode.Open) :> System.IO.Stream   
    runParser filename import stream
