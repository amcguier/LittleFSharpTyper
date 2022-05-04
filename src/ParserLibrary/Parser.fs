namespace ParserLibrary

module TypeParser =
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    open ParserLibrary.Grammer

    let ws1 = unicodeSpaces1

    let (typeParser : Parser<Types,unit>), exprRef = createParserForwardedToRef()
    
    let nat =
        pstring "NAT"
        |>> fun _ -> Types.NAT

    let atom =
        pstring "ATOM"
        |>> fun _ -> Types.ATOM

    let pair =
        pstring "PAIR" .>> ws1 >>. typeParser .>> ws1 .>>. typeParser
        |>> fun (x, y) -> PAIR(x, y)

    exprRef.Value <- choice [ nat
                              atom
                              pair]
module Parser =
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    open ParserLibrary.Grammer


    let atomMatcher c = isLetter c || c = '-'
    let ws = unicodeSpaces
    let ws1 = unicodeSpaces1
    let atomParser : Parser<AST,unit> =
        pstring "'" >>. (many1Satisfy2L atomMatcher atomMatcher "atom" ) |>> ATOM
        .>>  followedBy (choice [unicodeSpaces1
                                 eof                          
                                 (pstring ")" |>> ignore)])

    let expr, exprRef = createParserForwardedToRef()

    let parens parser =
        between (pstring "(") (pstring ")") parser 

    let zeroParser =
        pstring "ZERO" |>> fun _ -> ZERO

    let addOneParser =
        pstring "add1" >>.ws1 >>. expr .>> ws
        |> parens
    
    let consParser =
        pstring "cons" >>. ws1 >>. expr .>> ws1 .>>. expr .>> ws
        |>> fun (x,y) -> CONS(x,y)
        |> parens

    let cdrParser =
        pstring "cdr" >>. ws1 >>. expr .>> ws
        |>> CDR
        |> parens
        
    let carParser =
        pstring "car" >>. ws1 >>. expr .>> ws
        |>> CAR
        |> parens

    let digitParser =
        many1SatisfyL isDigit "digit"
        >>= fun x ->
            let rs,x = System.Int64.TryParse x
            if rs then
                preturn (DIGITS x)
            else
                fail $"Unable to parse digits from {x}"

    let claimParser =
        pstring "claim" .>> ws1 >>. many1Satisfy atomMatcher .>> ws1 .>>. TypeParser.typeParser
        |>> fun (str,typ) -> CLAIM_EXP(str,typ)
        |> parens 

    let defineParser =
        pstring "define" >>. ws >>. many1Satisfy atomMatcher .>> ws .>>. expr
        |>> fun (str,exp) -> DEFINE_EXP(str,exp)
        |> parens

    let plusParser =
        pstring "+" >>.ws1 >>. expr .>> ws .>>. expr .>> ws
        |>> fun (x,y) -> PLUS(x,y)
        |> parens

    exprRef.Value <-  choice [ atomParser
                               consParser
                               cdrParser
                               carParser
                               zeroParser
                               addOneParser
                               digitParser
                               claimParser
                               defineParser
                               plusParser]



    let parse = ws >>. expr .>> ws .>> eof




    let parseString = run parse 
