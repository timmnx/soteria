open Soteria
open Soteria_std
open Hc
module Var = Symex.Var
(* module Int_map = Stdlib.Map.Make (Int) *)

type ty = TBool | TInt | TFloat | TStr | TOthers | TNotYetImplemented
(* | TStr | TTuple of ty list | TNull *)
[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt
let t_float = TFloat
let t_str = TStr
let t_others = TOthers
let t_not_yet_implemented = TNotYetImplemented
let is_int = function TInt -> true | _ -> false
let is_float = function TFloat -> true | _ -> false

(* TODO : other types *)

module Nop = struct
  (* ToDo *)
  type t = Distinct [@@deriving eq, show { with_path = false }, ord]
end

module Unop = struct
  let equal_fpclass = ( = )
  let compare_fpclass = compare

  type t = Negative | Not | Invert | To_bool [@@deriving eq, ord]

  let pp_signed ft b = Fmt.string ft (if b then "s" else "u")

  let pp ft = function
    | Negative -> Fmt.string ft "-"
    | Not -> Fmt.string ft "!"
    | Invert -> Fmt.string ft "~"
    | To_bool -> Fmt.string ft "bool_of"
end

module Binop = struct
  type t =
    (* Math *)
    | Add
    | Sub
    | Mul
    | Div
    | Floor_div
    | Mod
    | Pow
    | Mat_mul
    (* Bool *)
    | And
    | Or
    (* Bit *)
    | BitAnd
    | BitOr
    | BitXor
    | BitLshift
    | BitRshift
    (* Comp *)
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
  [@@deriving eq, show { with_path = false }, ord]

  let pp ft = function
    | Add -> Fmt.string ft "+"
    | Sub -> Fmt.string ft "-"
    | Mul -> Fmt.string ft "*"
    | Div -> Fmt.string ft "/"
    | Floor_div -> Fmt.string ft "//"
    | Mod -> Fmt.string ft "%"
    | Pow -> Fmt.string ft "**"
    | Mat_mul -> Fmt.string ft "@"
    | And -> Fmt.string ft "&&"
    | Or -> Fmt.string ft "||"
    | BitAnd -> Fmt.string ft "&"
    | BitOr -> Fmt.string ft "|"
    | BitXor -> Fmt.string ft "^"
    | BitLshift -> Fmt.string ft "<<"
    | BitRshift -> Fmt.string ft ">>"
    | Eq -> Fmt.string ft "=="
    | Ne -> Fmt.string ft "!="
    | Lt -> Fmt.string ft "<"
    | Gt -> Fmt.string ft ">"
    | Le -> Fmt.string ft "<="
    | Ge -> Fmt.string ft ">="
end

let pp_hash_consed pp_node ft t = pp_node ft t.node
let equal_hash_consed _ t1 t2 = Int.equal t1.tag t2.tag
let compare_hash_consed _ t1 t2 = Int.compare t1.tag t2.tag

type t_kind =
  | Var of Var.t
  | None_
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Float of float
  (* ref: Language Reference 3.2.4.3 Complex (numbers.Complex) *)
  | Complex of float * float (* real, imaginary *)
  | Str of string (* UTF-8 *)
  | Bytes of string (* an immutable byte string (raw bytes) *)
  | Tuple of t list
  | Slice of t * t * t (* start, stop, step (None_ = absent) *)
  | Range of Z.t * Z.t * Z.t (* start, stop, step *)
  | Builtin of string (* builtin function, by name *)
  | Bound of t * t (* callable, self — a bound method *)
  (* | Code_obj of Phir.code (* a code constant (consumed by Make_function) *) *)
  | Ref of int (* heap address *)
  | Null (* CPython's NULL stack sentinel *)
  (* ref: Language Reference 3.2.2 NotImplemented *)
  | Not_implemented (* the NotImplemented singleton *)
  (* ref: Language Reference 3.2.3 Ellipsis (the [...] literal) *)
  | Ellipsis
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
  | Nop of Nop.t * t list
  | Ite of t * t * t

and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let unique_tag t = t.tag
let hash t = t.tag
let kind t = t.node.kind
let is_bool_ty = function TBool -> true | _ -> false

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ | Float _ -> ()
  | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) -> List.iter (fun sv -> iter_vars sv f) l
  | Ite (c, t, e) ->
      iter_vars c f;
      iter_vars t f;
      iter_vars e f
  | _ -> failwith "Don't know what to do (iter_vars in svalue.ml)"

let pp_full ft t = pp_t_node ft t.node

let rec pp ft t =
  let open Fmt in
  match t.node.kind with
  | Var v -> pf ft "V%a" Var.pp v
  | Bool b -> pf ft "%b" b
  | Int z when Z.(z > of_int 2048 || z < of_int (-2048)) ->
      pf ft "%s" (Z.format "%#x" z)
  | Int z -> pf ft "%a" Z.pp_print z
  | Ite (c, t, e) -> pf ft "(%a ? %a : %a)" pp c pp t pp e
  | Unop (Not, { node = { kind = Binop (Eq, v1, v2); _ }; _ }) ->
      pf ft "(%a != %a)" pp v1 pp v2
  | Unop (op, v) -> pf ft "%a(%a)" Unop.pp op pp v
  | Binop (op, v1, v2) -> pf ft "(%a %a %a)" pp v1 Binop.pp op pp v2
  | Nop (op, l) -> (
      let rec aux = function
        | acc, [] -> acc
        | Some l, { node = { kind = Var v; _ }; _ } :: rest ->
            aux (Some (Var.to_int v :: l), rest)
        | _, _ -> None
      in
      let range =
        aux (Some [], l)
        |> Option.bind (fun l ->
            let l = List.sort Int.compare l in
            let min = List.hd l in
            let max = List.hd @@ List.rev l in
            if max - min + 1 = List.length l then Some (min, max) else None)
      in
      match range with
      | Some (min, max) -> pf ft "%a(V|%d-%d|)" Nop.pp op min max
      | None -> pf ft "%a(%a)" Nop.pp op (list ~sep:comma pp) l)
| None_ -> pf ft "None_"
| Null -> pf ft "Null"
| Float f -> pf ft "%f" f
| Str s -> pf ft "%s" s
| Ref i -> pf ft "Ref(%d)" i
| _ -> failwith "ToDo (in Svalue.pp)"

let rec sure_neq (a : t) (b : t) =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Bool a, Bool b -> a <> b
  | Int a, Int b -> not (Z.equal a b)
  | Float a, Float b -> a <> b
  (* | Str a, Str b -> a <> b *)
  (* | Tuple a, Tuple b -> (
      try List.for_all2 sure_neq a b with Invalid_argument _ -> false)
  | Ref _, Ref _ -> failwith "ToDo" *)
  | Float f, Int i | Int i, Float f -> failwith "Determine what to do (in Svalue.sure_neq)"
  | _ -> false (* [None_] and [Null] cases are included here *)

module Hcons = Hc.Make (struct
  type t = t_node

  let equal = equal_t_node

  (* We could do a lot more efficient in terms of hashing probably, if this ever
     becomes a bottleneck. *)
  let hash { kind; ty } =
    let hty = Hashtbl.hash ty in
    match kind with
    | Var _ | Bool _ | Int _ -> Hashtbl.hash (kind, hty)
    | Unop (op, v) -> Hashtbl.hash (op, v.tag, hty)
    | Binop (op, l, r) -> Hashtbl.hash (op, l.tag, r.tag, hty)
    | Nop (op, l) -> Hashtbl.hash (op, List.map (fun sv -> sv.tag) l, hty)
    | Ite (c, t, e) -> Hashtbl.hash (c.tag, t.tag, e.tag, hty)
    | _ -> Hashtbl.hash (kind, hty)
    (* | _ -> failwith "Todo (in Hcons)" *)
end)

let ( <| ) kind ty : t = Hcons.hashcons { kind; ty }
let mk_var v ty = Var v <| ty

(** We put commutative binary operators in some sort of normal form where
    element with the smallest id is on the LHS, to increase cache hits. *)
let mk_commut_binop op l r =
  if l.tag <= r.tag then Binop (op, l, r) else Binop (op, r, l)

module SBool = struct
  (** {2 Booleans} *)

  let v_true = Bool true <| TBool
  let v_false = Bool false <| TBool

  let[@inline] to_bool t =
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

  let and_lazy v1 v2 =
    match v1.node.kind with Bool false -> v_false | _ -> and_ v1 (v2 ())

  let or_lazy v1 v2 =
    match v1.node.kind with Bool true -> v_true | _ -> or_ v1 (v2 ())

  (* let conj l = List.fold_left and_ v_true l *)
  let conj = function
    | [] -> v_true
    | h :: [] -> h
    | h :: t -> List.fold_left and_ h t

  let rec split_ands (sv : t) (f : t -> unit) : unit =
    match sv.node.kind with
    | Binop (And, s1, s2) ->
        split_ands s1 f;
        split_ands s2 f
    | _ -> f sv

  let distinct l =
    (* [Distinct l] when l is empty or of size 1 is always true *)
    match l with
    | [] | [ _ ] -> v_true
    | l ->
        let cross_product = List.to_seq l |> Seq.self_cross_product in
        let sure_distinct =
          Seq.for_all (fun (a, b) -> sure_neq a b) cross_product
        in
        if sure_distinct then v_true else Nop (Distinct, l) <| TBool

  let distinct_seq s = distinct (List.of_seq s)
end

module SNumeric = struct
  (** {2 Integers, Float, Complex} *)
  (** Integers *)
  let int_z z = Int z <| TInt

  let int_float f = int_z (Z.of_float f)
  let int i = int_z (Z.of_int i)

  let nonzero_z z =
    if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else int_z z

  let nonzero x = if x = 0 then raise (Invalid_argument "nonzero") else int x
  let zero = int_z Z.zero
  let one = int_z Z.one

  (** Float *)
  let float f = Float f <| TFloat

  (** Helpers *)
  let as_int (v:t) =
    match v.node.kind with
    | Int z -> Some z
    | Bool b -> Some (if b then Z.one else Z.zero)
    | _ -> None

  let as_float (v:t) =
    match v.node.kind with
    | Float f -> Some f
    | Int z -> Some (Z.to_float z)
    | Bool b -> Some (if b then 1. else 0.)
    | _ -> None

  let is_number (v:t) =
    match v.node.ty with
    | TFloat | TInt | TBool -> true
    | _ -> false

  (* let is_complex v =
    match v.node.ty with
    | TComplex -> true
    | _ -> false *)

  let rec add v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v1 zero -> v2
    | _, _ when equal v2 zero -> v1
    | Int i1, Int i2 -> int_z (Z.add i1 i2)
    | Binop (Add, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
        add v1 (int_z (Z.add i2 i3))
    | Binop (Add, { node = { kind = Int i1; _ }; _ }, v2), Int i3 ->
        add (int_z (Z.add i1 i3)) v2
    | Int i1, Binop (Add, v1, { node = { kind = Int i2; _ }; _ }) ->
        add (int_z (Z.add i1 i2)) v1
    | Int i1, Binop (Add, { node = { kind = Int i2; _ }; _ }, v2) ->
        add (int_z (Z.add i1 i2)) v2
    (* | _ -> mk_commut_binop Add v1 v2 <| TInt *)
    | _ -> failwith "Not sure what to do, typing int might be wrong"

  let rec sub v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v2 zero -> v1
    | Int i1, Int i2 -> int_z (Z.sub i1 i2)
    | Var v1, Var v2 when Var.equal v1 v2 -> zero
    | Binop (Sub, { node = { kind = Int i2; _ }; _ }, v1), Int i1 ->
        sub (int_z (Z.sub i2 i1)) v1
    | Binop (Sub, v1, { node = { kind = Int i2; _ }; _ }), Int i1 ->
        sub v1 (int_z (Z.add i2 i1))
    | Int i1, Binop (Sub, { node = { kind = Int i2; _ }; _ }, v1) ->
        add (int_z (Z.sub i1 i2)) v1
    | Int i1, Binop (Sub, v1, { node = { kind = Int i2; _ }; _ }) ->
        sub (int_z (Z.add i1 i2)) v1
    | Binop (Add, x, y), _ when equal x v2 -> y
    | Binop (Add, x, y), _ when equal y v2 -> x
    | _, Binop (Add, x, y) when equal x v1 -> sub zero y
    | _, Binop (Add, x, y) when equal y v1 -> sub zero x
    | _ -> Binop (Sub, v1, v2) <| TInt

  let mul v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v1 zero || equal v2 zero -> zero
    | _, _ when equal v1 one -> v2
    | _, _ when equal v2 one -> v1
    | Int i1, Int i2 -> int_z (Z.mul i1 i2)
    | _ -> mk_commut_binop Mul v1 v2 <| TInt

  (* let div v1 v2 = match (v1.node.kind, v2.node.kind) with | _, _ when equal
     v2 one -> v1 | Int i1, Int i2 when Z.divisible i1 i2 -> int_z (Z.div i1 i2)
     | Int i1, Int i2 -> failwith "ToDo: SFloat.float (Z.to_float i1 /.
     Z.to_float i2)" | Binop (Mul, v, { node = { kind = Int i; _ }; _ }), Int j
     | Int j, Binop (Mul, v, { node = { kind = Int i; _ }; _ }) when Z.equal i j
     -> v | Binop (Mul, { node = { kind = Int i; _ }; _ }, v), Int j | Int j,
     Binop (Mul, { node = { kind = Int i; _ }; _ }, v) when Z.equal i j -> v | _
     -> Binop (Div, v1, v2) <| TInt *)

  let floor_div v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v2 one -> v1
    | Int i1, Int i2 -> int_z (Z.div i1 i2)
    | Binop (Mul, v, { node = { kind = Int i; _ }; _ }), Int j
    | Int j, Binop (Mul, v, { node = { kind = Int i; _ }; _ })
      when Z.equal i j ->
        v
    | Binop (Mul, { node = { kind = Int i; _ }; _ }, v), Int j
    | Int j, Binop (Mul, { node = { kind = Int i; _ }; _ }, v)
      when Z.equal i j ->
        v
    | _ -> Binop (Floor_div, v1, v2) <| TInt

  let rec is_mod v n =
    match v.node.kind with
    (* | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero *)
    | Int i1 -> Z.divisible i1 n
    | Binop (Add, v2, v3) -> is_mod v2 n && is_mod v3 n
    | Binop (Sub, v2, v3) -> is_mod v2 n && is_mod v3 n
    | Binop (Mul, v2, v3) -> is_mod v2 n || is_mod v3 n
    | _ -> false

  (* let rec rem v1 v2 = match (v1.node.kind, v2.node.kind) with | _, _ when
     equal v2 one -> zero | _, Int i2 when is_mod v1 i2 -> zero | Int i1, Int i2
     -> int_z (Z.rem i1 i2) | Binop (Mul, v1, n), Binop (Mul, v2, m) when equal
     n m -> mul n (rem v1 v2) | Binop (Mul, n, v1), Binop (Mul, m, v2) when
     equal n m -> mul n (rem v1 v2) | _ -> Binop (Floor_div, v1, v2) <|
     v1.node.ty *)

  let rec mod_ v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | _, _ when equal v2 one -> zero
    | _, Int i2 when is_mod v1 i2 -> zero
    | Int i1, Int i2 ->
        (* OCaml's mod computes the remainer... *)
        let rem = Z.( mod ) i1 i2 in
        if Z.lt rem Z.zero then int_z (Z.add rem i2) else int_z rem
    | Binop (Mod, x, { node = { kind = Int i1; _ }; _ }), Int i2
      when Z.geq i1 i2 && Z.divisible i1 i2 ->
        mod_ x v2
    (* (x + (y % n)) % m when n | m <=> (x + y) % m *)
    | ( Binop
          ( Add,
            x,
            {
              node =
                { kind = Binop (Mod, l, { node = { kind = Int m1; _ }; _ }); _ };
              _;
            } ),
        Int m2 )
      when Z.geq m1 m2 && Z.divisible m1 m2 ->
        mod_ (add x l) v2
    | _ -> Binop (Mod, v1, v2) <| TInt

  let rec pow v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | Int i1, Int i2 -> Z.pow i1 (Z.to_int i2) |> int_z
    | _ -> Binop (Pow, v1, v2) <| TInt

  let neg v =
    match v.node.kind with Int i -> int_z (Z.neg i) | _ -> sub zero v

  let invert v =
    match v.node.kind with
    | Int i -> int_z (Z.neg (Z.( + ) i Z.one))
    | _ -> sub (-1 |> int) v

  (* {2 Equality, comparison, int-bool and int-float conversions} *)
  module Static = struct
    let num_eq a b =
      match (as_int a, as_int b) with
      | Some x, Some y -> Z.equal x y
      | _ -> (
        match (as_float a), (as_float b) with
        | Some x, Some y -> x = y
        | _ -> failwith
              "This case should never happend (in Svalue.SNumeric.Static.num_eq"
      )

    let num_lt a b =
      match (as_int a, as_int b) with
      | Some x, Some y -> Z.lt x y
      | _ -> (
        match (as_float a), (as_float b) with
        | Some x, Some y -> x < y
        | _ -> failwith
              "This case should never happend (in Svalue.SNumeric.Static.num_lt"
      )

    let num_le a b =
      match (as_int a, as_int b) with
      | Some x, Some y -> Z.leq x y
      | _ -> (
        match (as_float a), (as_float b) with
        | Some x, Some y -> x <= y
        | _ -> failwith
              "This case should never happend (in Svalue.SNumeric.Static.num_le"
      )
  end

  let rec sem_eq v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | (Bool _|Int _|Float _ ), (Bool _|Int _|Float _ ) ->
      SBool.of_bool @@ Static.num_eq v1 v2
    | _, Binop (Add, v2, v3) when equal v1 v2 -> sem_eq v3 zero
    | _, Binop (Add, v2, v3) when equal v1 v3 -> sem_eq v2 zero
    | Binop (Add, v1, v3), _ when equal v1 v2 -> sem_eq v3 zero
    | Binop (Add, v1, v3), _ when equal v3 v2 -> sem_eq v1 zero
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 ->
        sem_eq v2 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 ->
        sem_eq v2 v3
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 ->
        sem_eq v1 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 ->
        sem_eq v1 v3
    | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y
    | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
        sem_eq v1 (int_z @@ Z.sub y x)
    | Binop (Sub, z, { node = { kind = Int x; _ }; _ }), Int y
    | Int y, Binop (Sub, z, { node = { kind = Int x; _ }; _ }) ->
        sem_eq z (int_z @@ Z.add y x)
    | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1)
    | Int y, Binop (Mul, v1, { node = { kind = Int x; _ }; _ })
    | Binop (Mul, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        if Z.equal Z.zero x then SBool.of_bool (Z.equal Z.zero y)
        else if Z.(equal zero (rem y x)) then sem_eq v1 (int_z Z.(y / x))
        else SBool.v_false
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

  let rec lt v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | (Bool _|Int _|Float _ ), (Bool _|Int _|Float _ ) ->
      SBool.of_bool @@ Static.num_lt v1 v2
    | _, _ when equal v1 v2 -> SBool.v_false
    | _, Binop (Add, v2, v3) when equal v1 v2 -> lt zero v3
    | _, Binop (Add, v2, v3) when equal v1 v3 -> lt zero v2
    | Binop (Add, v1, v3), _ when equal v1 v2 -> lt v3 zero
    | Binop (Add, v1, v3), _ when equal v3 v2 -> lt v1 zero
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 -> lt v2 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 -> lt v1 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 -> lt v2 v3
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 -> lt v1 v3
    | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        lt v1 (int_z @@ Z.sub y x)
    | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
        lt (int_z @@ Z.sub y x) v1
    | Binop (Sub, v1, { node = { kind = Int x; _ }; _ }), Int y ->
        lt v1 (int_z @@ Z.add y x)
    | Binop (Sub, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        lt (int_z @@ Z.sub x y) v1
    | Int y, Binop (Sub, v1, { node = { kind = Int x; _ }; _ }) ->
        lt (int_z @@ Z.add y x) v1
    | Int y, Binop (Sub, { node = { kind = Int x; _ }; _ }, v1) ->
        lt v1 (int_z @@ Z.sub x y)
    | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1')
    | Int y, Binop (Mul, v1', { node = { kind = Int x; _ }; _ }) ->
        if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
        else
          let op = if Z.divisible y x || Z.(y > zero) then lt else le in
          if Z.(zero < x) then op (int_z Z.(y / x)) v1'
          else op v1' (int_z Z.(y / x))
    | Binop (Mul, v1', { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
        if Z.equal Z.zero x then SBool.of_bool (Z.lt Z.zero y)
        else
          let op = if Z.divisible y x || Z.(y < zero) then lt else le in
          if Z.(zero < x) then op v1' (int_z Z.(y / x))
          else op (int_z Z.(y / x)) v1'
    | Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ }), Int y
      when Z.leq x y ->
        SBool.v_true
    | Int y, Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ })
      when Z.lt y (Z.neg (Z.abs x)) ->
        SBool.v_true
    | Int _, Ite (b, t, e) -> SBool.ite b (lt v1 t) (lt v1 e)
    | Ite (b, t, e), Int _ -> SBool.ite b (lt t v2) (lt e v2)
    | _ -> Binop (Lt, v1, v2) <| TBool

  and le v1 v2 =
    match (v1.node.kind, v2.node.kind) with
    | (Bool _|Int _|Float _ ), (Bool _|Int _|Float _ ) ->
      SBool.of_bool @@ Static.num_le v1 v2
    | _, _ when equal v1 v2 -> SBool.v_true
    | _, Binop (Add, v2, v3) when equal v1 v2 -> le zero v3
    | _, Binop (Add, v2, v3) when equal v1 v3 -> le zero v2
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 -> le v2 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 -> le v1 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 -> le v2 v3
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 -> le v1 v3
    | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        le v1 (int_z @@ Z.sub y x)
    | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
        le (int_z @@ Z.sub y x) v1
    | Binop (Sub, v1, { node = { kind = Int x; _ }; _ }), Int y ->
        le v1 (int_z @@ Z.add y x)
    | Binop (Sub, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        le (int_z @@ Z.sub x y) v1
    | Int y, Binop (Sub, v1, { node = { kind = Int x; _ }; _ }) ->
        le (int_z @@ Z.add y x) v1
    | Int y, Binop (Sub, { node = { kind = Int x; _ }; _ }, v1) ->
        le v1 (int_z @@ Z.sub x y)
    | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1')
    | Int y, Binop (Mul, v1', { node = { kind = Int x; _ }; _ }) ->
        if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
        else
          let op = if Z.divisible y x || Z.(y < zero) then le else lt in
          if Z.(zero < x) then op (int_z Z.(y / x)) v1'
          else op v1' (int_z Z.(y / x))
    | Binop (Mul, v1', { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
        if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
        else
          let op = if Z.divisible y x || Z.(y > zero) then le else lt in
          if Z.(zero < x) then op v1' (int_z Z.(y / x))
          else op (int_z Z.(y / x)) v1'
    | Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ }), Int y
      when Z.leq x y ->
        SBool.v_true
    | Int y, Binop (Floor_div, _, { node = { kind = Int x; _ }; _ })
      when Z.leq y (Z.neg (Z.abs x)) ->
        SBool.v_true
    | Int y, Binop (Mod, _, _) when Z.leq y Z.zero -> SBool.v_true
    | Int _, Ite (b, t, e) -> SBool.ite b (le v1 t) (le v1 e)
    | Ite (b, t, e), Int _ -> SBool.ite b (le t v2) (le e v2)
    | _ -> Binop (Le, v1, v2) <| TBool

  (* {Conversion} *)
  let of_bool b =
    if equal SBool.v_true b then one
    else if equal SBool.v_false b then zero
    else Unop (To_bool, b) <| TInt

  let to_bool v =
    match v.node.kind with
    | Int i -> if Z.equal Z.zero i then SBool.v_false else SBool.v_true
    | _ -> Binop (Ne, v, zero) <| TBool

  let of_float (v : t) =
    match v.node.kind with Float f -> int_float f | _ -> failwith "ToDo (in Svalue.of_float)"

  let to_float (v : t) =
    match v.node.kind with
    | Int i -> Float (Z.to_float i) <| TFloat
    | _ -> failwith "ToDo (in Svalue.to_float)"
end

module SStr = struct
  (** {2 Integers + Float + Str} *)
  let str s = Str s <| TStr
  (* let is_static_str (v : t) : bool =
    match v.node.kind with Str _ -> true | _ -> false *)
  let is_str (v : t) : bool =
    match v.node.ty with TStr -> true | _ -> false
  let get_str (v : t) : string option =
    match v.node.kind with Str s -> Some s | _ -> None
  (* ToDo *)
  let sem_eq v1 v2 =
    match v1.node.kind, v2.node.kind with
    | Str s1, Str s2 -> SBool.of_bool (s1 = s2)
    | _ -> mk_commut_binop Eq v1 v2 <| TBool
end

module SOthers = struct
  (* None_ *)
  let none_ = None_ <| TOthers
  let is_none_ (v : t) : bool = equal none_ v

  (* Null *)
  let null = Null <| TOthers
  let is_null (v : t) : bool = equal null v

  (* Ref *)
  let mk_ref i = Ref i <| TOthers
  let is_ref (v : t) : bool =
    match v.node.kind with Ref _ -> true | _ -> false
  let get_ref (v : t) : int option =
    match v.node.kind with Ref i -> Some i | _ -> None

  (* Builtin *)
  let mk_builtin s = Builtin s <| TOthers
  let is_builtin (v : t) : bool =
    match v.node.kind with Builtin _ -> true | _ -> false
  let get_builtin (v : t) : string option =
    match v.node.kind with Builtin s -> Some s | _ -> None

  (* Bound *)
  let mk_bound (a, b) = Bound (a,b) <| TOthers
  let is_bound (v : t) : bool =
    match v.node.kind with Bound _ -> true | _ -> false
  let get_bound (v : t) : (t * t) option =
    match v.node.kind with Bound (a,b) -> Some (a,b) | _ -> None

  (* Tuple *)
  let mk_tuple l = Tuple l <| TOthers
  let is_tuple (v : t) : bool =
    match v.node.kind with Tuple _ -> true | _ -> false
  let get_tuple (v : t) : t list option =
    match v.node.kind with Tuple l -> Some l | _ -> None

  (* Not_implemented *)
  let not_implemented = Not_implemented <| TOthers

  (* Ellipsis *)
  let ellipsis = Ellipsis <| TOthers


end

let to_bool = SBool.to_bool
let of_bool = SBool.of_bool
let rec not sv =
  if equal sv SBool.v_true then SBool.v_false
  else if equal sv SBool.v_false then SBool.v_true
  else
    match sv.node.kind with
    | Unop (Not, sv) -> sv
    | Binop (Lt, v1, v2) -> Binop (Le, v2, v1) <| TBool
    | Binop (Le, v1, v2) -> Binop (Lt, v2, v1) <| TBool
    | Binop (Gt, v1, v2) -> Binop (Ge, v2, v1) <| TBool
    | Binop (Ge, v1, v2) -> Binop (Gt, v2, v1) <| TBool
    | Binop (Or, v1, v2) -> mk_commut_binop And (not v1) (not v2) <| TBool
    | Binop (And, v1, v2) -> mk_commut_binop Or (not v1) (not v2) <| TBool
    | _ -> Unop (Not, sv) <| TBool

(** {2 Unop functions} *)
let neg (v : t) : t =
  match v.node.ty with
  | _ when SNumeric.is_number v -> SNumeric.neg v
  | _ -> failwith "'neg' not implemented yet (ToDo in Svalue.neg)"

let not_ (v : t) : t = failwith "'not_' not implemented yet (ToDo in Svalue.not_)"

let invert (v : t) : t = failwith "'invert' not implemented yet (ToDo in Svalue.invert)"

let var_to_bool_ (v : t) : t =
  match v.node.ty with
  | TBool -> v
  | TInt | TFloat -> SNumeric.to_bool v
  | _ -> failwith "ToDo Svalue.var_to_bool_"

let to_bool_ (v : t) : t =
  match v.node.kind with
  | Var _ -> var_to_bool_ v
  | None_ -> SBool.v_false
  | Bool _ -> v
  | Int _ | Float _ -> SNumeric.to_bool v
  (* | Complex _ -> failwith "ToDo Svalue.to_bool_.Complex"
  | Str _ -> failwith "ToDo Svalue.to_bool_.Str"
  | Bytes _ -> failwith "ToDo Svalue.to_bool_.Bytes"
  | Tuple _ -> failwith "ToDo Svalue.to_bool_.Tuple"
  | Range _ -> failwith "ToDo Svalue.to_bool_.Range"
  | Ref _ -> failwith "ToDo Svalue.to_bool_.Ref" *)
  (* | _ -> Unop (To_bool, v) <| TBool *)
  | _ -> v

(** {2 Binop functions} *)
let add (v1 : t) (v2 : t) : t =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> SNumeric.int_z (Z.div i1 i2)
  | _ -> Binop (Add, v1, v2) <| TInt
let sub (v1 : t) (v2 : t) : t =
  match v1.node.ty, v2.node.kind with
  | _ when SNumeric.is_number v1 && SNumeric.is_number v2 ->
    SNumeric.sub v1 v2
  | _ ->
    failwith "Sub not implemented for other than SNumerics (ToDo)"

let mul (v1 : t) (v2 : t) : t = failwith "Mul not implemented yet (ToDo)"

let div (v1 : t) (v2 : t) : t =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 SNumeric.one -> v1
  | Int i1, Int i2 -> SNumeric.int_z (Z.div i1 i2)
  | _ -> Binop (Div, v1, v2) <| TInt

let floor_div (v1 : t) (v2 : t) : t = failwith "Floor_div not implemented yet (ToDo)"

let mod_ (v1 : t) (v2 : t) : t = failwith "Mod not implemented yet (ToDo)"

let pow (v1 : t) (v2 : t) : t = failwith "Pow not implemented yet (ToDo)"

let mat_mul (v1 : t) (v2 : t) : t = failwith "Matmul not implemented yet (ToDo)"

(* Bool *)
let and_ (v1 : t) (v2 : t) : t =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 v2 -> v1
  | Bool false, _ | _, Bool false -> SBool.v_false
  | Bool true, _ -> v2
  | _, Bool true -> v1
  | _ -> mk_commut_binop And v1 v2 <| TBool

let or_ (v1 : t) (v2 : t) : t = failwith "Or not implemented yet (ToDo)"
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
  if equal v1 v2 then SBool.v_true
  else
  match (v1.node.ty, v2.node.ty) with
  | _, _ when SNumeric.(is_number v1 && is_number v2) -> SNumeric.sem_eq v1 v2
  | TStr, TStr -> SStr.sem_eq v1 v2
  | _ -> failwith "Eq only implemented fot TBool and TInt (ToDo)"


let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else SBool.v_false

let sem_ne (v1 : t) (v2 : t) : t =
  if equal v1 v2 then SBool.v_false
  else
  not @@ sem_eq v1 v2
  (* match (v1.node.ty, v2.node.ty) with
  | _ -> failwith "Ne not implemented yet (ToDo in Svalue.sem_ne)" *)

let lt (v1 : t) (v2 : t) : t =
  if equal v1 v2 then SBool.v_false else
  match (v1.node.ty, v2.node.ty) with
  | _ when SNumeric.is_number v1 && SNumeric.is_number v2 -> SNumeric.lt v1 v2
  | _ -> failwith "Lt not implemented yet (ToDo in Svalue.lt)"

let leq (v1 : t) (v2 : t) : t =
  if equal v1 v2 then SBool.v_true else
  match (v1.node.ty, v2.node.ty) with
  | _ when SNumeric.is_number v1 && SNumeric.is_number v2 -> SNumeric.le v1 v2
  | _ -> failwith "Le not implemented yet (ToDo in Svalue.leq)"

let gt (v1 : t) (v2 : t) : t =
  failwith "Gt not implemented yet (ToDo in Svalue.gt)"

let geq (v1 : t) (v2 : t) : t =
  failwith "Ge not implemented yet (ToDo in Svalue.geq)"

(** {2 General constructors} *)

let mk_unop : Unop.t -> t -> t = function
  | Negative -> neg
  | Not -> not_
  | Invert -> invert
  | To_bool -> to_bool_

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
  | Le -> leq
  | Gt -> gt
  | Ge -> geq

let mk_nop : Nop.t -> t list -> t = function Distinct -> SBool.distinct
(* let not v = mk_unop Unop.Not v *)

(** {2 Infix operators} *)

let are_addable (x:t) (y:t) =
  match (x.node.ty, y.node.ty) with
  | TInt, TInt -> Some (x, y)
  | TInt, TFloat -> None
  | _ -> failwith "ToDo (in Svalue.are_addable)"

module Infix = struct
  let int_z = SNumeric.int_z
  let int = SNumeric.int

  (* Math *)
  let ( +@ ) = mk_binop Add
  let ( -@ ) = mk_binop Sub
  let ( ~- ) = mk_unop Negative
  let ( ~! ) = mk_unop Not
  let ( ~~ ) = mk_unop Invert
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
    let mk_nonzero = SNumeric.nonzero
    let[@inline] zero () = SNumeric.zero
    let[@inline] one () = SNumeric.one
  end
end
