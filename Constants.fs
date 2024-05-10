module Constants
[<Literal>] 
let ADD = ":+"
[<Literal>] 
let SUB = ":-"
[<Literal>] 
let MUL = ":*"
[<Literal>] 
let DIV = ":/"
[<Literal>] 
let GT = ">"
[<Literal>] 
let LT = "<"
[<Literal>] 
let GE = ">="
[<Literal>] 
let LE = "<="
[<Literal>] 
let EQ = "="
[<Literal>] 
let NE = "<>"
[<Literal>] 
let LET = "-_-"
[<Literal>] 
let LET_REC = "(-_-)"
[<Literal>] 
let ARROW = "(/>_<)/~o "
[<Literal>] 
let IF = "(?_?)"
[<Literal>] 
let THEN = "(T_T)"
[<Literal>] 
let ELSE = "(E_E)"
[<Literal>] 
let R_APP =  ")"
[<Literal>] 
let FUN = ":F"
[<Literal>] 
let TRUE = "True"
[<Literal>] 
let FALSE = "False"
[<Literal>] 
let ASSIGN = "0_0"
[<Literal>] 
let HEAD = "head"
[<Literal>] 
let TAIL = "tail"

type tok = string
type expr =
    | Var of tok
    | Lambda of tok * expr             // Лямбда-выражение (имя и тело)
    | App of expr * expr               // _(A,B)
    | Int of int
    | Bool of bool
    | Cond of expr * expr * expr       // Условное выражение (if | then | else)
    | Let of tok * expr * expr         // Определение (имя, значение, тело)
    | LetRec of tok * expr * expr      // Рекурсивное определение
    | Builtin of tok
    | Op of tok * int * expr list      // Операция (оператор, арность, аргументы)
    | Closure of expr * env            // Замыкание (тело, окружение)
    | RecClosure of expr * env * tok   // Рекурсивное замыкание
    | List of expr list                // Список вида [expr, expr ...]
    | String of tok                   // Строки вида "abc\n abc"
and env = Map<string, expr>  

