// todo исправить FunSmile.md

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
let GT = "B>" // раньше было ">"
[<Literal>] 
let LT = "B<" // раньше было "<"
[<Literal>] 
let GE = ">B=" // раньше было ">="
[<Literal>] 
let LE = "<B=" // раньше было "<="
[<Literal>] 
let EQ = "B=" // раньше было "="
[<Literal>] 
let LET = "(F_F)>" // раньше было "-_-"
[<Literal>] 
let LET_REC = "((F_F)>>" // раньше было "(-_-)"
[<Literal>] 
let ARROW = "(/>_<)/~o" // по планам "(/>-<)/~o"
[<Literal>] 
let IF = "(?_?)"
[<Literal>] 
let THEN = "(T_T)"
[<Literal>] 
let ELSE = "(E_E)"
[<Literal>] 
let R_APP =  "_" // раньше было ")" - недуобно за скобочками следить
[<Literal>] 
let FUN = ":F"
[<Literal>] 
let TRUE = "True"
[<Literal>] 
let FALSE = "False"
[<Literal>] 
let ASSIGN = ":@" // раньше было "0_0"
[<Literal>] 
let HEAD = "head"
[<Literal>] 
let TAIL = "tail"
[<Literal>] 
let CONCAT = ">V<"
[<Literal>] 
let NEG = ":|"


type tok = string
type expr =
    | Var of tok
    | Lambda of tok * expr             // Лямбда-выражение (имя и тело)
    | App of expr * expr               // _ A B
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
    | String of tok                    // Строки вида "abc\n abc"
    | Float of float
and env = Map<string, expr>  

