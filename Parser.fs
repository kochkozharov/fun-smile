module Parser
open FParsec
open Constants

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let ws = spaces
let str = pstring 

// Перенаправляем вызовы к парсеру через ссылку
let fexpr, fexprRef = createParserForwardedToRef<expr, UserState> ()

let ftok : Parser<_> = 
    let isVarFirstChar c = isLetter c || c = '_'
    let isVarChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isVarFirstChar isVarChar |>> tok

let fvar : Parser<_> = 
    ftok |>> Var

let flambda : Parser<_> = 
    pipe2 (str FUN >>. ws >>. ftok .>> ws) (str ARROW >>. ws >>. fexpr) 
        (fun a b -> Lambda(a, b))

let fapp : Parser<_> = 
    pipe2 (str R_APP >>. ws >>. fexpr .>> ws) (fexpr) (fun a b -> App(a, b))

let fint : Parser<_> = 
    pint32 |>> Int

let ops = [ADD; SUB ; MUL; DIV; GE; LE; GT; LT; EQ; HEAD; TAIL]

let fbuiltin : Parser<_> = 
    ops |> List.map str |> choice |>> Builtin

let fbool : Parser<_> = (stringReturn TRUE  true) <|> 
                        (stringReturn FALSE false) |>> Bool

let fcond : Parser<_> = 
    pipe3 (str IF >>. ws >>. fexpr .>> ws) (str THEN >>. ws >>. fexpr .>> ws)
            (str ELSE >>. ws >>. fexpr) (fun a b c -> Cond(a, b ,c))

let flet : Parser<_> = 
    pipe3 (str LET >>. ws >>. ftok .>> ws) (str ASSIGN >>. ws >>. fexpr .>> ws)
        (fexpr) (fun a b c -> Let(a, b, c))

let fletrec : Parser<_> = 
    pipe3 (str LET_REC >>. ws >>. ftok .>> ws) (str ASSIGN >>. ws >>. fexpr .>> ws)
        (fexpr) (fun a b c -> LetRec(a, b, c))

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

let flist   = listBetweenStrings "[" "]" fexpr List

// do - однократное присваивание jvalueRef выражания choice
// jvalue теперь указывает на парсер, который хранится в jvalueRef.
// do fexprRef := choice
do fexprRef.Value <- choice [
    fint
    fapp
    flambda
    fbuiltin
    fcond
    flet
    fletrec
    fvar
    flist
]

let fprogram : Parser<_> = ws >>. fexpr .>> ws .>> eof

let testParser p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let getResult p str = 
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg