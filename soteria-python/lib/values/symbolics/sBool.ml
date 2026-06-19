open Hc
open Soteria
open Soteria_std
open Definitions

(** {2 Booleans} *)

let v_true = Bool true <| TBool
let v_false = Bool false <| TBool

let to_bool t =
  if equal t v_true then Some true
  else if equal t v_false then Some false
  else None

let of_bool b =
  (* avoid re-alloc and re-hashconsing *)
  if b then v_true else v_false

let and_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 v2 -> v1
  | Bool false, _ | _, Bool false -> v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> mk_commut_binop And v1 v2 <| TBool

let or_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool true, _ | _, Bool true -> v_true
  | Bool false, _ -> v2
  | _, Bool false -> v1
  | _ -> mk_commut_binop Or v1 v2 <| TBool

(* let conj l = List.fold_left and_ v_true l *)
let conj = function
  | [] -> v_true
  | h :: [] -> h
  | h :: t -> List.fold_left and_ h t

let rec not_ sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | Binop (Lt, v1, v2) -> Binop (Le, v2, v1) <| TBool
    | Binop (Le, v1, v2) -> Binop (Lt, v2, v1) <| TBool
    | Binop (Gt, v1, v2) -> Binop (Ge, v2, v1) <| TBool
    | Binop (Ge, v1, v2) -> Binop (Gt, v2, v1) <| TBool
    | Binop (Or, v1, v2) -> mk_commut_binop And (not_ v1) (not_ v2) <| TBool
    | Binop (And, v1, v2) -> mk_commut_binop Or (not_ v1) (not_ v2) <| TBool
    | _ -> Unop (Not, sv) <| TBool

let ite guard if_ else_ =
  match (guard.node.kind, if_.node.kind, else_.node.kind) with
  | Bool true, _, _ -> if_
  | Bool false, _, _ -> else_
  | _, Bool true, Bool false -> guard
  | _, Bool false, Bool true -> not_ guard
  | _, Bool false, _ -> and_ (not_ guard) else_
  | _, Bool true, _ -> or_ guard else_
  | _, _, Bool false -> and_ guard if_
  | _, _, Bool true -> or_ (not_ guard) if_
  | _ when equal if_ else_ -> if_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty

let sem_eq v1 v2 =
  if equal v1 v2 then v_true
  else
    match (v1.node.kind, v2.node.kind) with
    | Bool b1, Bool b2 -> of_bool (b1 = b2)
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

let sem_ne v1 v2 =
  if equal v1 v2 then v_false
  else
    match (v1.node.kind, v2.node.kind) with
    | Bool b1, Bool b2 -> of_bool (b1 <> b2)
    | _ -> mk_commut_binop Ne v1 v2 <| TBool

let gt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> of_bool (b1 > b2)
  | _ -> Binop (Gt, v1, v2) <| TBool

let ge v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> of_bool (b1 >= b2)
  | _ -> Binop (Ge, v1, v2) <| TBool

let lt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> of_bool (b1 < b2)
  | _ -> Binop (Lt, v1, v2) <| TBool

let le v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Bool b1, Bool b2 -> of_bool (b1 <= b2)
  | _ -> Binop (Le, v1, v2) <| TBool
