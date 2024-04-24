module Constants
let INPUT = ":o"
let PRINT = ":)"
let ADD = ":+"
let SUB = ":-"
let MUL = ":*"
let DIV = ":/"
let LET = "-_-"
let LET_REC =  "(-_-)"
let ARROW = "(/>_<)/~o "
let IF = "(?_?)"
let THEN = "(T_T)"
let ELSE = "(E_E)"
let R_APP =  "<:"
let L_APP =  ":>"

type tok = string
type expr = 
    | Var of tok
    | Lambda of tok *expr
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

