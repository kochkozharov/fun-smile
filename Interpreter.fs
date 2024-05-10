module Interpreter
open Constants

let apply_operation op args =
  match op, args with
  | ADD, [Int(a); Int(b)] -> Int(a + b)
  | SUB, [Int(a); Int(b)] -> Int(a - b)
  | MUL, [Int(a); Int(b)] -> Int(a*b)
  | DIV, [Int(a); Int(b)] -> Int(a/b)
  | EQ, [Int(a); Int(b)] -> Bool(a=b)
  | LT, [Int(a); Int(b)] -> Bool(a<b)
  | GT, [Int(a); Int(b)] -> Bool(a>b)
  | LE, [Int(a); Int(b)] -> Bool(a<=b)
  | GE, [Int(a); Int(b)] -> Bool(a>=b)
  | _, _ -> failwith "Invalid arguments"

let funof = function
  | ADD 
  | SUB 
  | MUL
  | DIV
  | EQ
  | LT
  | GT
  | LE 
  | GE
   as op -> apply_operation op
  | _ -> failwith "Unknown builtin"

let arity = function
    | "sin" -> 1
    | _ -> 2

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
        | RecClosure(exp, env, tol) -> exp
        | Closure(exp, env) -> exp
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
    | _ -> failwith "Invalid application"

let exec exp = eval exp Map.empty

let fact = exec (LetRec("fact", Lambda(
    "x", Cond(App(App(Builtin("<="), Var("x")), Int(1)), Int(1),
    App(App(Builtin(MUL),Var("x")), App(
        Var("fact"),App(App(Builtin(SUB), Var("x")), Int(1))
        )))
    ), App(Var("fact"), Int(5))))

let testSum = exec

    
let printExpr = function
    | Int(x) -> printfn "%d" x
    | Var(x) -> printfn "%s" x
    | _ -> failwith "TODO"

printExpr fact