// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    //opgaverbe forneden er en parser der minder om regex for hvad der matcher hvad
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"//alternativt: satisfy (fun f -> System.Char.IsLetter f) <?> letter er grundet label til f#
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces = many whitespaceChar <?> "spaces"
    let spaces1 = many1 whitespaceChar <?> "space1"
 

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 //parentiese er for forståelsen. Function gør således at vi fjerner
    //spaces fra p1 via .>>, bæmerk at punktummet er på venstre side. Nå de er fjernet giver jeg resultatet videre
    //til p2 via de to puntummer .>>. derved er de kombineret 
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2 //retuerne p1
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2 //returnere p2, p1 er ligegyldig. eksampel x = 4 hvor x = er p1 og
    //4 er p2
 

    let parenthesise p = pchar '(' >*>. (p .>*> pchar ')') //pchar tager en parser, fjerner '(' og spaces fra venstre
    //og ligger det over til højre via >*>. til p, fjerner det til jøjre fra p som der parses via .>*> phar ')'
    //let parenthesise p = pchar '(' >*>. pchar ')' >*>. p //de

    //7.5
    let pid = pchar '_' <|> pletter .>>. many (pchar '_' <|> palphanumeric) |>> fun (x,y) -> System.String.Concat (x::y) 
    //foreoven tager en char som er enten '_' eller pletter <|> betyder eller i parser sprog. sender det videre via
    //.>>. og så many der siger om vi har mange af pchar eller palphanumeric. alt det til venstre for |>> returnere
    //en parser af p<char * charlist> men vi vil have det som en string. det er hvor |>> kan bruges. tager parseren 
    //fra før og en function hvor jeg sætter x på listen y

    //7.6    
    let unop op a = op >*>. a //returner kun a uden da det er  >*>. og fjerner alle spaces da dette  var hvad parseren gjorde
    let binop op p1 p2 = p1 .>*> op .>*>. p2 //returnere parseren fra p1 og fjerner spaces fra op, tilf'jer 
    //resultatet fra venstre side til p2 via .>*>.

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub" //laver egne subtract
    do tref := choice [AddParse;SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegationParse = unop (pchar '-') TermParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"//unop tager kun en enkelt derfor brug for den
    // bruger |>> fordi Termparse er det grammer som har de funtioner vi skal bruge her Mul som er gange. 
    let PointValueParse = unop (pPointValue)(parenthesise TermParse) |>> PV <?> "PV" //ifølge grammer på side 3 i opgaven
    //er pointvalue ( A ), parentasiese metode n fjerner jo dem og TermPase er min aEXp
    let VariablesParser = (pid) |>> V <?> "V"//ved ikke hvorfor jeg bruger pid som jeg lavede før undersøg det!
    let CharToIntParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt" 
    let NParse   = pint32 |>> N <?> "Int" 
    let ParParse = parenthesise TermParse <?> "Par"
    do aref := choice [NegationParse; PointValueParse; CharToIntParse; VariablesParser; NParse; ParParse]

    let AexpParse = TermParse 

    let CParse = pchar '\'' >>.(anyChar .>> pchar '\'') |>> C <?> "C" //fjerner begge ' inspiration fundet fra
    //opgave parenthesise linje 53
    let CVParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CV" 
    let IntToCharParse = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"
    let ToUpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"

    do cref:= choice [CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParse]

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
