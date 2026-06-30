include Aux
open Soteria.Soteria_std.Syntaxes.FunctionWrap
open Pytecode
open Phir
(* open PytecodeValue *)
open Symex
open Symex.Syntax
open S_val.Infix

type error = DivisionByZero | AssertError | NotImplementedYet
type frame = unit

(* let eval_var (v : var) : S_val.t = match v with | Fast i -> S_val.(mk_var
   (Var.of_int i) TNull) | Deref i -> Svalue.(mk_var (Var.of_int i) TNull) |
   Name n -> Svalue.(mk_var (Var.of_string n) TNull) | Global g ->
   Svalue.(mk_var (Var.of_string g) TNull) *)

let rec eval_const (subst : subst) (c : code) (co : Ast.const) :
    (S_val.t, 'err, 'a) Symex.Result.t =
  match co with
  | None_ -> failwith "ToDo"
  | Bool b -> S_val.of_bool b |> Result.ok
  | Int i -> S_val.SInt.int_z i |> Result.ok
  | Float f -> S_val.SFloat.float f |> Result.ok
  | Complex { re; im } -> failwith "ToDo"
  | Str _ -> failwith "ToDo"
  | Bytes _ -> failwith "ToDo"
  | Tuple _ -> failwith "ToDo"
  | Frozenset _ -> failwith "ToDo"
  | Code _ -> failwith "ToDo"
  | Ellipsis -> failwith "ToDo"

let rec eval_value (subst : subst) (c : code) (v : value) :
    (S_val.t, 'err, 'a) Symex.Result.t =
  match v with
  | Stack -> failwith "Todo"
  | Null -> failwith "ToDo"
  | Const co -> eval_const subst c co
  | Code c -> failwith "ToDo"
  | Var v -> failwith "ToDo"

let rec eval_binop (subst : subst) (c : code) (op: Phir.binop) (v1 : value) (v2 : value) :
    (S_val.t, 'err, 'a) Symex.Result.t =
match op with
  | Add ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' +@ v2' |> Result.ok
  | Sub ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' -@ v2' |> Result.ok
  | Mul ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' *@ v2' |> Result.ok
  | Div ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' /@ v2' |> Result.ok
  | Floor_div ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' //@ v2' |> Result.ok
  | Mod ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' %@ v2' |> Result.ok
  | Pow ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' **@ v2' |> Result.ok
  | Mat_mul ->
    let** v1' = eval_value subst c v1 in
    let** v2' = eval_value subst c v2 in
    v1' @@ v2' |> Result.ok
  | And -> failwith "ToDo"
  | Or -> failwith "ToDo"
  | Lshift -> failwith "ToDo"
  | Rshift -> failwith "ToDo"
  | Xor -> failwith "ToDo"

and eval_instr (subst : subst) (c : code) (i : instr) :
    (S_val.t, 'err, 'a) Symex.Result.t =
  match i with Assign (x, v) -> failwith "ToDo" | _ -> failwith "ToDo"

and eval_code (subst : subst) (c : code) : (S_val.t, 'err, 'a) Symex.Result.t =
  match c with _ -> failwith "ToDo"

and eval_module (c : code) : (S_val.t, 'err, 'a) Symex.Result.t =
  failwith "ToDo"
