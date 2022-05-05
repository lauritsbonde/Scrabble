// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil
    open Eval

    (*
    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.
    *)

    //open JParsec.TextParser             // Example parser combinator library. Use for CodeJud
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring  "intToChar"
    let pPointValue = pstring  "pointValue"

    let pCharToInt  = pstring  "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel    = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 
    let braces p = pchar '{' >*>. p .>*> pchar '}'

    let rec charListToString =
        function
        | [] -> ""
        | c::cl -> string c + charListToString cl
        
    let pid =
        (pletter <|> pchar '_') 
        .>>. many (palphanumeric <|> pchar '_')
        |>> (fun (c, cl) -> string c + charListToString cl)

    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AexpParse = TermParse 
    let CexpParse = CharParse

    // Parsers for AexpParse
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let VarParse = pid |>> V <?> "Var"
    let NegParse = pchar '-' >>. TermParse |>> (fun i -> Mul ((N -1), i)) <?> "neg"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PointValue"
    let CharToIntParse = unop pCharToInt (parenthesise CexpParse) |>> CharToInt <?> "CharToInt"

    // Parsers for CexpParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let ToUpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let IntToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let CharValueParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CharValue"

    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [NegParse; NParse; CharToIntParse; PVParse; VarParse; ParParse]

    do cref := choice [CParse; ToUpperParse; ToLowerParse; IntToCharParse; CharValueParse]

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}


