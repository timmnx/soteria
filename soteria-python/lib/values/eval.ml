open Soteria
open Soteria_std
open Svalue

type _ Effect.t += Eval_var : Var.t * Svalue.ty -> t Effect.t

let eval_var (v : Var.t) (ty : Svalue.ty) : t =
  Effect.perform (Eval_var (v, ty))

let eval_binop : Binop.t -> t -> t -> t = function
  (* Math *)
  | Add -> add
  | Sub -> sub
  | Mul -> mul
  | Div -> div
  | Floor_div -> floor_div
  | Mod -> mod_
  | Pow -> pow
  | Mat_mul -> mat_mul
  (* Bool *)
  | And -> and_
  | Or -> or_
  (* Bit *)
  | BitAnd -> bit_and
  | BitOr -> bit_or
  | BitXor -> bit_xor
  | BitLshift -> bit_lshift
  | BitRshift -> bit_rshift
  (* Comp *)
  | Eq -> sem_eq
  | Ne -> sem_ne
  | Lt -> lt
  | Le -> le
  | Gt -> gt
  | Ge -> ge

let eval_unop : Unop.t -> t -> t = function
  | Negative -> neg
  | Not -> not_
  | Invert -> invert
  | To_bool -> cast_to_bool

let rec eval (x : t) : t =
  match x.node.kind with
  | Var v -> eval_var v x.node.ty
  | Bool _ | Int _ -> x
  | Unop (unop, v) ->
      let nv = eval v in
      if v == nv then x else eval_unop unop nv
  | Binop (binop, v1, v2) ->
      (* TODO: for binops that may short-circuit such as || or &&, we could do
         this without evaluating both sides, and deciding if any of either side
         evaluates properly to e.g. true/false *)
      let nv1 = eval v1 in
      let nv2 = eval v2 in
      if v1 == nv1 && v2 == nv2 then x else eval_binop binop nv1 nv2
  | Nop (nop, l) -> (
      let l, changed = List.map_changed eval l in
      if Stdlib.not changed then x else match nop with Distinct -> distinct l)
  | Ite (guard, then_, else_) ->
      let guard = eval guard in
      if equal guard v_true then eval then_
      else if equal guard v_false then eval else_
      else ite guard (eval then_) (eval else_)
  | _ -> failwith "ToDo"

let eval ~(eval_var : Var.t -> Svalue.ty -> t option) (x : t) : t option =
  try Some (eval x) with
  | Division_by_zero -> None
  | effect Eval_var (v, ty), k -> (
      match eval_var v ty with
      | Some v -> Effect.Deep.continue k v
      | None -> None)
