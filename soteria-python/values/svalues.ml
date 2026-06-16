open Soteria
open Soteria_std
open Hc
open Definitions

let distinct l =
  (* [Distinct l] when l is empty or of size 1 is always true *)
  match l with
  | [] | [ _ ] -> SBool.v_true
  | l ->
      let cross_product = List.to_seq l |> Seq.self_cross_product in
      let sure_distinct =
        Seq.for_all (fun (a, b) -> sure_neq a b) cross_product
      in
      if sure_distinct then SBool.v_true else Nop (Distinct, l) <| TBool

let distinct_seq s = distinct (List.of_seq s)

(** {2 General constructors} *)

let mk_unop (unop : Unop.t) (v : t) : t =
  match (unop, v.node.ty) with
  | Negative, TInt -> SInt.neg v
  | Negative, _ -> failwith "ToDo"
  | Not, TBool -> SBool.not v
  | Not, _ -> failwith "ToDo"
  | Invert, TInt -> SInt.invert v
  | Invert, _ -> failwith "ToDo"
  | To_bool, TBool -> v
  | To_bool, TInt -> SInt.to_bool v
  | To_bool, _ -> failwith "ToDo"

let mk_binop (binop : Binop.t) (v1 : t) (v2 : t) : t =
  assert (v1.node.ty = v2.node.ty);
  match binop with
  (* Math *)
  | Add -> (
      match v1.node.ty with TInt -> SInt.add v1 v2 | _ -> failwith "Todo")
  | Sub -> (
      match v1.node.ty with TInt -> SInt.sub v1 v2 | _ -> failwith "ToDo")
  | Mul -> (
      match v1.node.ty with TInt -> SInt.mul v1 v2 | _ -> failwith "ToDo")
  | Div -> (
      match v1.node.ty with TInt -> SInt.div v1 v2 | _ -> failwith "ToDo")
  | Floor_div -> (
      match v1.node.ty with
      | TInt -> SInt.floor_div v1 v2
      | _ -> failwith "ToDo")
  | Mod -> (
      match v1.node.ty with TInt -> SInt.mod_ v1 v2 | _ -> failwith "ToDo")
  | Pow -> (
      match v1.node.ty with TInt -> SInt.pow v1 v2 | _ -> failwith "ToDo")
  | Mat_mul -> failwith "ToDo"
  (* Bool *)
  | And -> SBool.and_ v1 v2
  | Or -> SBool.or_ v1 v2
  (* Bit *)
  | BitAnd -> failwith "ToDo"
  | BitOr -> failwith "ToDo"
  | BitXor -> failwith "ToDo"
  | BitLshift -> failwith "ToDo"
  | BitRshift -> failwith "ToDo"
  (* Comp *)
  | Eq -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")
  | Ne -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")
  | Lt -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")
  | Le -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")
  | Gt -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")
  | Ge -> (
      match v1.node.ty with
      | TBool -> SBool.sem_eq v1 v2
      | TInt -> SInt.sem_eq v1 v2
      | _ -> failwith "ToDo")

let mk_nop : Nop.t -> t list -> t = function Distinct -> distinct

(** {2 Infix operators} *)

module Infix = struct
  let int_z = SInt.int_z
  let int = SInt.int
  (* Math *)
  let ( +@ ) = mk_binop Add
  let ( -@ ) = mk_binop Sub
  let ( ~- ) = mk_unop Negative
  let ( *@ ) = mk_binop Mul
  let ( /@ ) = mk_binop Div
  let ( //@ ) = mk_binop Floor_div
  let ( %@ ) = mk_binop Mod
  let ( **@ ) = mk_binop Pow
  let ( @@ ) = mk_binop Mat_mul
  (* Bool *)
  let ( &&@ ) = mk_binop And
  let ( ||@ ) = mk_binop Or
  (* Bit *)
  let ( &@ ) = mk_binop BitAnd
  let ( |@ ) = mk_binop BitOr
  let ( ^@ ) = mk_binop BitXor
  let ( >>@ ) = mk_binop BitLshift
  let ( <<@ ) = mk_binop BitRshift
  (* Comp *)
  let ( ==@ ) = mk_binop Eq
  let ( !=@ ) = mk_binop Ne
  (* let ( ==?@ ) = sem_eq_untyped *)
  let ( >@ ) = mk_binop Gt
  let ( >=@ ) = mk_binop Ge
  let ( <@ ) = mk_binop Lt
  let ( <=@ ) = mk_binop Le
end

module Syntax = struct
  module Sym_int_syntax = struct
    let mk_nonzero = SInt.nonzero
    let[@inline] zero () = SInt.zero
    let[@inline] one () = SInt.one
  end
end
