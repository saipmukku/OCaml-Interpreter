open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 

  match e with

  | Value v -> 
    v

  | ID i -> 
    lookup env i

  | Not n ->
    (match eval_expr env n with 
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError ("Expected Type Bool")))

  | Binop(op, expr, expr') ->

    (match eval_expr env expr with 

    | Int first ->

      (match eval_expr env expr' with 

        | Int(second) -> 

          (match op with

          | Less -> Bool(first < second)

          | Greater -> Bool(first > second)

          | GreaterEqual -> Bool(first >= second)

          | LessEqual -> Bool(first <= second)

          | Equal -> Bool(first = second)

          | NotEqual -> Bool(first <> second)

          | Add -> Int(first + second)

          | Sub -> Int(first - second)

          | Mult -> Int(first * second)

          | Div -> if second <> 0 then Int(first / second) else raise (DivByZeroError)

          | _ -> raise(TypeError("Invalid OP")))

        | _ -> raise (TypeError ("Expected Type Int")))

  | String first ->

    (match eval_expr env expr' with 
    
      | String second ->

        (match op with 

        | Concat -> String(first ^ second)

        | Equal -> Bool(first = second)

        | NotEqual -> Bool(first <> second)

        | _ -> raise (TypeError ("Invalid OP")))

    | _ -> raise (TypeError ("Expecting Type String")))

  | Bool(first) ->

    (match eval_expr env expr' with 

    | Bool(second) ->

      (match op with 

      | And -> Bool(first && second)
      | Or -> Bool (first || second)
      | Equal -> Bool(first = second)
      | NotEqual -> Bool (first <> second)
      | _ -> raise(TypeError("Invalid OP")))

    | _ -> raise (TypeError ("Expecting Type Bool")))

  | _ -> raise (TypeError("Binop Error")))

| If(expr, expr', expr'') ->

  (match eval_expr env expr with 

  | Bool(b) -> if b = true then eval_expr env expr' else eval_expr env expr''
  | _ -> raise (TypeError ("Expecting bool expr")))

| Let(id, true, expr, expr') ->

  let new_env = extend_tmp env id in
  let new_expr = eval_expr new_env expr in
  (update new_env id new_expr);
  (eval_expr new_env expr')

| Let(id, false, expr, expr') ->

  (match eval_expr env expr with

  | Int i -> eval_expr (extend env id (Int i)) expr'

  | Bool b -> eval_expr (extend env id (Bool b)) expr'

  | String s -> eval_expr (extend env id (String s)) expr'

  | Closure(a, b, c) -> eval_expr (extend env id (Closure(a, b, c))) expr')

| Fun (id, expr) -> Closure(env, id, expr)

| FunctionCall (id, expr) ->

  (match eval_expr env id with 
  
  | Closure(a, b, c) -> 
    (match eval_expr env expr with

    |Int i -> eval_expr(extend a b (Int i)) c

    |Bool boolean -> eval_expr(extend a b (Bool boolean)) c

    |String s -> eval_expr (extend a b (String s)) c

    |Closure(d, e, f) -> eval_expr(extend a b (Closure(d, e, f))) c)

  | _ -> raise (TypeError ("Not a Function")))
  
  (* Part 2: Evaluating mutop directive *)
  
  (* Evaluates MicroCaml mutop directive [m] in environment [env],
     returning a possibly updated environment paired with
     a value option; throws an exception on error *)

let eval_mutop env m =

  match m with 

  | NoOp -> ([], None)

  | Expr e -> (env, Some (eval_expr env e))

  | Def (value, expr) -> 
    let new_env = (extend_tmp env value) in 
    let next_env = (eval_expr new_env expr) in 
    (update new_env value next_env); 
    new_env, Some next_env