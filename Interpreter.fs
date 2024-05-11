module Interpreter
open Constants

let apply_operation op args =
    let arithmetic_op f_int f_float =
        match args with
        | [Int(a); Int(b)] -> Int(f_int a b)
        | [Float(a); Float(b)] -> Float(f_float a b)
        | _ -> failwith "Invalid arguments"

    let comparison_op f_int f_float =
        match args with
        | [Int(a); Int(b)] -> Bool(f_int a b)
        | [Float(a); Float(b)] -> Bool(f_float a b)
        | _ -> failwith "Invalid arguments"
    
    match op, args with
    | EQ, [List(a); List(b)] -> Bool(a=b)
    | HEAD, [List(a)] -> List.head a
    | TAIL, [List(a)] -> (List.tail >> List) a
    | CONCAT, [List(a); List(b)] -> List (a@b)
    | CONCAT, [a ; List(b)] -> List(a::b)
    | NEG, [Bool(a)] -> Bool(not a)
    | _ ->

    match op with
    | ADD -> arithmetic_op (+) (+)
    | SUB -> arithmetic_op (-) (-)
    | MUL -> arithmetic_op (*) (*)
    | DIV -> arithmetic_op (/) (/)
    | EQ -> comparison_op (=) (=)
    | LT -> comparison_op (<) (<)
    | GT -> comparison_op (>) (>)
    | LE -> comparison_op (<=) (<=)
    | GE -> comparison_op (>=) (>=)
    | _ -> failwith "Invalid arguments"

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
  | HEAD
  | TAIL
  | NEG
  | CONCAT
   as op -> apply_operation op
  | _ -> failwith "Unknown builtin"

let arity = function
    | HEAD -> 1
    | TAIL -> 1
    | NEG -> 1
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
        | List(l) -> List.map (fun expr -> eval expr env) l |> List
        | Float(n) -> Float(n)
        | String(s) -> String(s)

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


let printExpr : expr -> unit =  printfn "%A" 
