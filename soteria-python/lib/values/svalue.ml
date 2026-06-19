open Soteria
open Soteria_std
open Hc
open Symbolics
include Definitions

module SBool = SBool
module SInt = SInt
module SFloat = SFloat

let v_true = SBool.v_true
let v_false = SBool.v_false
let to_bool = SBool.to_bool
let of_bool = SBool.of_bool
let ite = SBool.ite

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

let rec split_ands (sv : t) (f : t -> unit) : unit =
  match sv.node.kind with
  | Binop (And, s1, s2) ->
      split_ands s1 f;
      split_ands s2 f
  | _ -> f sv

let sem_eq_untyped v1 v2 =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.sem_eq v1 v2
  | TInt, TInt -> SInt.sem_eq v1 v2
  | _ -> SBool.v_false

(** {2 Unop functions} *)
let neg (v : t) : t =
  match v.node.ty with
  | TInt -> SInt.neg v
  | _ -> failwith "Negative only implemented fot TInt (ToDo)"

let not_ (v : t) : t =
  match v.node.ty with
  | TBool -> SBool.not_ v
  | _ -> failwith "Not only implemented fot TBool (ToDo)"

let invert (v : t) : t =
  match v.node.ty with
  | TInt -> SInt.invert v
  | _ -> failwith "Negative only implemented fot TInt (ToDo)"

let cast_to_bool (v : t) : t =
  match v.node.ty with
  | TBool -> v
  | TInt -> SInt.cast_to_bool v
  | _ -> failwith "Negative only implemented fot TInt (ToDo)"

(** {2 Binop functions} *)
let add (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.add v1 v2
  | _ -> failwith "Add only implemented fot TInt (ToDo)"

let sub (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.sub v1 v2
  | _ -> failwith "Sub only implemented fot TInt (ToDo)"

let mul (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.mul v1 v2
  | _ -> failwith "Mul only implemented fot TInt (ToDo)"

let div (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.div v1 v2
  | _ -> failwith "Div only implemented fot TInt (ToDo)"

let floor_div (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.floor_div v1 v2
  | _ -> failwith "Floor_div only implemented fot TInt (ToDo)"

let mod_ (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.mod_ v1 v2
  | _ -> failwith "Mod only implemented fot TInt (ToDo)"

let pow (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TInt, TInt -> SInt.pow v1 v2
  | _ -> failwith "Pow only implemented fot TInt (ToDo)"

let mat_mul (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "Matmul not implemented yet (ToDo)"

(* Bool *)
let and_ (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.and_ v1 v2
  | _ -> failwith "Pow only implemented fot TBool (ToDo)"

let or_ (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.or_ v1 v2
  | _ -> failwith "Pow only implemented fot TBool (ToDo)"
(* Bit *)

let bit_and (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "BitAnd not implemented yet (ToDo)"

let bit_or (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "BitOr not implemented yet (ToDo)"

let bit_xor (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "BitXor not implemented yet (ToDo)"

let bit_lshift (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "BitLshift not implemented yet (ToDo)"

let bit_rshift (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "BitRshift not implemented yet (ToDo)"

(* Comp *)
let sem_eq (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.sem_eq v1 v2
  | TInt, TInt -> SInt.sem_eq v1 v2
  | _ -> failwith "Eq only implemented fot TBool and TInt (ToDo)"

let sem_ne (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.sem_ne v1 v2
  | TInt, TInt -> SInt.sem_ne v1 v2
  | _ -> failwith "Ne only implemented fot TBool and TInt (ToDo)"

let lt (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.lt v1 v2
  | TInt, TInt -> SInt.lt v1 v2
  | _ -> failwith "Lt only implemented fot TBool and TInt (ToDo)"

let le (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.le v1 v2
  | TInt, TInt -> SInt.le v1 v2
  | _ -> failwith "Le only implemented fot TBool and TInt (ToDo)"

let gt (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.gt v1 v2
  | TInt, TInt -> SInt.gt v1 v2
  | _ -> failwith "Gt only implemented fot TBool and TInt (ToDo)"

let ge (v1 : t) (v2 : t) : t =
  match (v1.node.ty, v2.node.ty) with
  | TBool, TBool -> SBool.ge v1 v2
  | TInt, TInt -> SInt.ge v1 v2
  | _ -> failwith "Ge only implemented fot TBool and TInt (ToDo)"

(** {2 General constructors} *)

let mk_unop : Unop.t -> t -> t = function
  | Negative -> neg
  | Not -> not_
  | Invert -> invert
  | To_bool -> cast_to_bool

let mk_binop : Binop.t -> t -> t -> t = function
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

let mk_nop : Nop.t -> t list -> t = function Distinct -> distinct
let not v = mk_unop Unop.Not v

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
