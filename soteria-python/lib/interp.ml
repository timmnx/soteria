open Pytecode
open Pytecode.Phir
module Symex = Soteria.Symex.Make (Values.Solver.Z3_solver)
open Symex
open Symex.Syntax
open Values
open Typed.Infix
open Typed.Syntax
module String_map = Map.Make (Soteria.Soteria_std.String)

type subst = Svalue.t String_map.t
type error = DivisionByZero | AssertError

let eval_var (v : var) : Svalue.t =
  match v with
  | Fast i -> Svalue.(mk_var (Var.of_int i) TNull)
  | Deref i -> Svalue.(mk_var (Var.of_int i) TNull)
  | Name n -> Svalue.(mk_var (Var.of_string n) TNull)
  | Global g -> Svalue.(mk_var (Var.of_string g) TNull)

let rec eval_const (subst : subst) (c : code) (co : Ast.const) :
    (Svalue.t, 'err, 'a) Symex.Result.t =
  match co with
  | None_ -> Svalue.(None_ <| TNone_) |> Result.ok
  | Bool b -> Svalue.of_bool b |> Result.ok
  | Int i -> Svalue.SInt.int_z i |> Result.ok
  | Float f -> Svalue.SFloat.float f |> Result.ok
  | Complex { re : _; im : _ } -> failwith "ToDo"
  | Str _ -> failwith "ToDo"
  | Bytes _ -> failwith "ToDo"
  | Tuple _ -> failwith "ToDo"
  | Frozenset _ -> failwith "ToDo"
  | Code _ -> failwith "ToDo"
  | Ellipsis -> failwith "ToDo"

let rec eval_value (subst : subst) (c : code) (v : value) :
    (Svalue.t, 'err, 'a) Symex.Result.t =
  match v with
  | Stack -> failwith "Todo"
  | Null -> Svalue.null |> Result.ok
  | Const co -> eval_const subst c co
  | Code c -> failwith "ToDo"
  | Var v -> failwith "ToDo"

and eval_instr (subst : subst) (c : code) (i : instr) :
    (Svalue.t, 'err, 'a) Symex.Result.t =
  match i with Assign (x, v) -> failwith "ToDo" | _ -> failwith "ToDo"

and eval_code (subst : subst) (c : code) : (Svalue.t, 'err, 'a) Symex.Result.t =
  match c with _ -> failwith "ToDo"
