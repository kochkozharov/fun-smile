module Interpreter
open Constants



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


let arity = function
    | "sin" -> 1
    | _ -> 2

let funof = function
    | "+" -> (fun [Int(a); Int(b)] -> Int(a+b))
    | "-" -> (fun [Int(a); Int(b)] -> Int(a-b))
    | "*" -> (fun [Int(a); Int(b)] -> Int(a*b))
    | "/" -> (fun [Int(a); Int(b)] -> Int(a/b))
    | "=" -> (fun [Int(a); Int(b)] -> Bool(a=b))
    | "<" -> (fun [Int(a); Int(b)] -> Bool(a<b))
    | ">" -> (fun [Int(a); Int(b)] -> Bool(a>b))
    | "<=" -> (fun [Int(a); Int(b)] -> Bool(a<=b))
    | ">=" -> (fun [Int(a); Int(b)] -> Bool(a>=b))

let rec eval exp env =
    let _ = printfn "eval %A where %A" exp env 
    match exp with
        | App(e1, e2) -> apply (eval e1 env) (eval e2 env)
        | Int(n) -> Int(n)
        | Var(x) -> Map.find x env
        | Builtin(f) -> Op(f, arity f, [])
        | Op(tok, n, el) -> Op(tok, n, el)
        | Cond(e0, e1, e2) ->
          if Bool(true) = eval e0 env then eval e1 env else eval e2 env
        | Let(tok, e1, e2) ->
          let r = eval e1 env
          eval e2 (Map.add tok r env)
        | LetRec(tok, e1, e2) ->
          eval e2 (Map.add tok (RecClosure(e1, env, tok)) env)
        | Lambda(tok, ex) -> Closure(exp, env)
        | Closure(exp, env) -> exp
        | RecClosure(exp, env, tok) -> exp
        | Bool(s) -> Bool(s)
and apply e1 e2 = 
    let _ = printfn "app (%A) (%A)" e1 e2
    match e1 with
    | Closure(Lambda(v, e), env) -> eval e (Map.add v e2 env)
    | RecClosure(Lambda(v, e), env, tok) ->
      eval e (Map.add v e2 (Map.add tok e1 env))
    | Op(tok, n, args) ->
      if n=1 then (funof tok) (args@[e2])
      else Op(tok, n-1, args@[e2])

let E exp = eval exp Map.empty

let test = E (App(App(Builtin("+"), Int(8)), Int(3)))

let fact () = E (LetRec("fact", Lambda(
    "x", Cond(App(App(Builtin("<="), Var("x")), Bool(true)), Bool(true),
    App(App(Builtin("*"),Var("x")), App(Var("fact"),App(App(Builtin("-"), Var("x")), Bool(true)))))
    ), App(Var("fact"), Int(5))))
