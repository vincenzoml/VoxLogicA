module VoxLogicA.Parser

open FParsec

type Position = string

let positionOfFParsecPosition (p : FParsec.Position) =
    p.ToString()

let unknownPosition : Position = "unknown"
type Expression = 
    | ECall of Position * string * (Expression list)
    | ENumber of float
    | EBool of bool
    | EString of string
    override this.ToString() = 
        match this with
        | ECall (_,ide,l) -> 
            if l.Length = 0 
            then $"{ide}" 
            else $"{ide}({List.map (fun x -> x.ToString()) l})"            
        | ENumber f -> $"{f}"
        | EBool b -> $"{b}"
        | EString s -> s.ToString() // Includes quotes in the output
type Command = 
    | Declaration of string * (string list) * Expression    
    | Save of Position * string * Expression 
    | Print of Position * string * Expression
    | Import of string

type Program = Program of list<Command>

// let rec sizeP (Program p) =
//     List.sumBy sizeC p

// and sizeC c =
//     match c with
//     | Declaration (_,_,e) -> sizeE e 
//     | Save (_,_,e) -> sizeE e
//     | Print (_,_,e) -> sizeE e
//     | Import _ -> 0

// and sizeE e =
//     match e with
//     | ECall (_,_,args) -> 
//         List.sumBy sizeE args
//     | _ -> 1


type Library = Library of list<Command>

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
        let pbool = (attempt (pstring "true" >>. parse {return true}) <|> attempt (pstring "false" >>. parse {return false})) <?> "boolean value"
        let simpleExpr = ((attempt pfloat) .>> spacesOrComment |>> ENumber) <|> ((attempt pbool) .>> spacesOrComment |>> EBool) <|> ((attempt strConst) .>> spacesOrComment |>> EString) <|> call 
        let (expr',exprImpl) : (Parser<Expression,unit> * (Parser<Expression,unit> ref)) = createParserForwardedToRef()
        let expr = expr' <?> "expression"
        let application = optlst (brackets (commaSepList expr)) <?> "actual arguments list"
        let opapplication = optlst (sqbrackets (commaSepList expr)) <?> "operator arguments list"
        callImpl := getPosition .>>. (attempt ide) .>>. application |>> (fun ((x,y),z) -> ECall (positionOfFParsecPosition x,y,z))
        exprImpl := 
            ((attempt (getPosition .>>. (simpleExpr <|> brackets expr) .>>. operator) .>>. opapplication .>>. expr) |>> (fun ((((p,x),y),t),z) -> ECall (positionOfFParsecPosition p,y,[x;z]@t)))
            <|>            
            (attempt simpleExpr)
            <|>
            ((getPosition .>>. (attempt operator) .>>. expr) |>> (fun ((p,x),y) -> ECall (positionOfFParsecPosition p,x,[y])))
            <|>                         
            (brackets expr)
        expr

let private command requireSpace s args =  attempt (skipString s) >>. (if requireSpace then spacesOrComment1 else spacesOrComment) >>. args .>> spacesOrComment
let private lhs = ide <|> operator
let private saveCommand = getPosition .>>. command false "save" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> Save (positionOfFParsecPosition x,y,z))
let private printCommand = getPosition .>>. command false "print" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> Print (positionOfFParsecPosition x,y,z))

let private importCommand = command false "import" strConst |>> Import
let private letCommand = command true "let" (lhs .>>. optlst farglist .>>. (defeq >>. parseExpression)) |>> (fun ((x,y),z) -> Declaration (x,y,z))    
let private file contents = spacesOrComment >>. contents .>> eof
let private import = file <| many (importCommand <|> letCommand) 
let private program = file <| parse {
    return! many (choice [ saveCommand; printCommand; importCommand; letCommand])    
}
let private getResult res = 
    match res with  
        | Success (result,_,_) -> result
        | Failure(msg,_,_) -> ErrorMsg.fail msg // raise (ErrorMsg.VLExn msg)

let private runParser name p stream =
    getResult (runParserOnStream p () name stream System.Text.Encoding.Default) 

let parseProgram filename = 
    use stream = new System.IO.FileStream(filename,System.IO.FileMode.Open) :> System.IO.Stream   
    Program <| runParser filename program stream

let parseImport filename =
    use stream = new System.IO.FileStream(filename,System.IO.FileMode.Open) :> System.IO.Stream   
    runParser filename import stream
