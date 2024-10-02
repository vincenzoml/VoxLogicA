module VoxLogicA.Parser

type Position = string
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

type Library = Library of list<Command>

module private Internals =

    open FParsec

    let positionOfFParsecPosition (p : FParsec.Position) =
        p.ToString()

    let commentLine = skipString "//" >>. skipRestOfLine true >>. spaces

    let spacesOrComment = spaces .>> skipMany commentLine <?> "whitespace"
    let spacesOrComment1 = (spaces1 <|> commentLine) .>> skipMany commentLine <?> "whitespace (required)"
    let ideFirstChar c = isLower c
    let ideOtherChar c = isLetter c || isDigit c
    let ide = (many1Satisfy2 ideFirstChar ideOtherChar <?> "identifier") .>> spacesOrComment
    let operator = (many1Chars (upper <|> anyOf "#;:_'.|!$%&/^=*-+<>?@~\\") <?> "operator") .>> spacesOrComment
    let quotes p = between (pstring "\"") (pstring "\"") p .>> spacesOrComment
    let strConst = quotes (manyChars (noneOf "\"")) <?> "quoted string"
    let commaSepList p = sepBy1 p ((pstring ",") .>> spacesOrComment)
    let defeq = (pstring "=" .>> spacesOrComment)

    let mybw b1 b2 p = (attempt b1) >>. p .>> b2
    let abrackets b1 b2 p = mybw (skipString b1 .>> spacesOrComment) (skipString b2 .>> spacesOrComment) p
    let brackets p = abrackets "(" ")" p
    let sqbrackets p = abrackets "[" "]" p
    let optlst p = opt p |>> Option.defaultValue []
    let farglist = brackets (commaSepList ide) <?> "formal arguments list" .>> spacesOrComment

    let parseExpression =
            let (call,callImpl) : (Parser<Expression,unit> * (Parser<Expression,unit> ref)) = createParserForwardedToRef()
            let pbool = (attempt (pstring "true" >>. parse {return true}) <|> attempt (pstring "false" >>. parse {return false})) <?> "boolean value"
            let simpleExpr = ((attempt pfloat) .>> spacesOrComment |>> ENumber) <|> ((attempt pbool) .>> spacesOrComment |>> EBool) <|> ((attempt strConst) .>> spacesOrComment |>> EString) <|> call
            let (expr',exprImpl) : (Parser<Expression,unit> * (Parser<Expression,unit> ref)) = createParserForwardedToRef()
            let expr = expr' <?> "expression"
            let application = optlst (brackets (commaSepList expr)) <?> "actual arguments list"
            let opapplication = optlst (sqbrackets (commaSepList expr)) <?> "operator arguments list"
            callImpl.Value <- getPosition .>>. (attempt ide) .>>. application |>> (fun ((x,y),z) -> ECall (positionOfFParsecPosition x,y,z))
            exprImpl.Value <-
                ((attempt (getPosition .>>. (simpleExpr <|> brackets expr) .>>. operator) .>>. opapplication .>>. expr) |>> (fun ((((p,x),y),t),z) -> ECall (positionOfFParsecPosition p,y,[x;z]@t)))
                <|>
                (attempt simpleExpr)
                <|>
                ((getPosition .>>. (attempt operator) .>>. expr) |>> (fun ((p,x),y) -> ECall (positionOfFParsecPosition p,x,[y])))
                <|>
                (brackets expr)
            expr

    let command requireSpace s args =  attempt (skipString s) >>. (if requireSpace then spacesOrComment1 else spacesOrComment) >>. args .>> spacesOrComment
    let lhs = ide <|> operator
    let saveCommand = getPosition .>>. command false "save" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> Save (positionOfFParsecPosition x,y,z))
    let printCommand = getPosition .>>. command false "print" (strConst .>>. parseExpression) |>> (fun (x,(y,z)) -> Print (positionOfFParsecPosition x,y,z))

    let importCommand = command false "import" strConst |>> Import
    let letCommand = command true "let" (lhs .>>. optlst farglist .>>. (defeq >>. parseExpression)) |>> (fun ((x,y),z) -> Declaration (x,y,z))
    let file contents = spacesOrComment >>. contents .>> eof
    let import = file <| many (importCommand <|> letCommand)
    let program = file <| parse {
        return! many (choice [ saveCommand; printCommand; importCommand; letCommand])
    }
    let getResult res =
        match res with
            | Success (result,_,_) -> result
            | Failure(msg,_,_) -> ErrorMsg.fail msg // raise (ErrorMsg.VLExn msg)

    let runParser name p stream =
        getResult (runParserOnStream p () name stream System.Text.Encoding.Default)

let parseProgram filename =
    use stream = new System.IO.FileStream(filename,System.IO.FileMode.Open) :> System.IO.Stream
    Program <| Internals.runParser filename Internals.program stream

let parseImport filename =
    use stream = new System.IO.FileStream(filename,System.IO.FileMode.Open) :> System.IO.Stream
    Internals.runParser filename Internals.import stream
