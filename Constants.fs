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

type tok = string
type expr = 
    | Var of tok
    | Lambda of tok*expr
    | App of expr*expr
    | Int of int
    | Bool of bool
    | Cond of expr*expr*expr
    | Let of tok*expr*expr
    | LetRec of tok*expr*expr
    | Builtin of tok
    | Op of tok*int*expr list
    | Closure of expr*env
    | RecClosure of expr*env*tok
and 
    env = Map<string, expr>
