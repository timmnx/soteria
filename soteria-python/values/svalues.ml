open Soteria
open Soteria_std
open Hc
module Var = Symex.Var
module Int_map = Stdlib.Map.Make (Int)

type ty =
  | TNone_
  | TBool
  | TInt
  | TFloat of float
  | TStr of string (* UTF-8 *)
  | TTuple of ty list

[@@deriving eq, show { with_path = false }, ord]

let t_bool = TBool
let t_int = TInt

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

type scode = {
  filename : string;
  name : string;
  qualname : string;
  docstring : string option;
  firstlineno : int;
  argcount : int;
  posonlyargcount : int;
  kwonlyargcount : int;
  nlocals : int;
  stacksize : int;
  flags : int;
  (* localsplus : (string * Ast.local_kind) array; *)
  (* instrs : instr array; *)
  (* exn_table : Ast.exn_entry array; *)
  lines : int array; (* positions : Pytecode.Ast.positions array; *)
}
[@@deriving show { with_path = false }, eq, ord]

type svalue =
  | Var of Var.t
  | None_
  | Bool of bool
  | Int of Z.t [@printer Fmt.of_to_string Z.to_string]
  | Float of float
  | Str of string (* UTF-8 *)
  | Tuple of t list
(* 
  | Slice of svalue * svalue * svalue (* start, stop, step (None_ = absent) *)
  | Range of Z.t * Z.t * Z.t (* start, stop, step *)
  | Builtin of string (* builtin function, by name *)
  | Bound of svalue * svalue (* callable, self — a bound method *)
  | Code_obj of scode (* a code constant (consumed by Make_function) *)
   *)
  | Null (* CPython's NULL stack sentinel *)
  | Ref of sobj
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
  | Nop of Nop.t * t list
  | Ite of t * t * t

and sobj =
  | List of svalue list
  | Dict of (svalue * svalue) list (* insertion-ordered *)
  | Set of svalue list (* insertion-ordered *)
  | Cell of svalue option

(* closure cell; None = empty *)
(* | Func of sfunc | Class of scls | Instance of { cls : int; dict : int (* a
   heap Dict with Str keys *) } | Gen of sgen | Super of { cls : int; self :
   svalue } (* bound super object *) | Property of { fget : svalue; fset :
   svalue option } | Classmethod of svalue | Staticmethod of svalue | Iter of
   siter *)

(* and func = { code : Phir.code; globals : int; (* module globals Dict address
   *) defaults : value list; kwdefaults : (value * value) list; (* Str name ->
   default *) closure : value list; (* Cell refs *) fdict : int; (* function
   attributes (f.x = 1), a heap Dict *) }

   and cls = { cname : string; bases : int list; (* class addresses *) mro : int
   list; (* C3 linearization, self first *) cdict : int; (* class namespace, a
   heap Dict with Str keys *) builtin : string option; (* Some "int" for builtin
   types like int/str/... *) }

   and gen = { gframe : frame option; (* None once exhausted *) gstarted : bool;
   gkind : [ `Gen | `Coroutine | `Async_gen ]; }

   (* Builtin iterators. Each step is a functional heap update. *) and iter = |
   It_list of int * int (* list address (read live), next index *) | It_seq of
   value list (* remaining items: tuples, dict-key snapshots, ... *) | It_str of
   string * int (* UTF-8 byte offset *) | It_range of Z.t * Z.t * Z.t (* next,
   stop, step *) | It_zip of value list (* component iterators *) | It_map of
   value * value list (* function, component iterators *) | It_filter of value *
   value (* predicate (or None_), iterator *) | It_enum of Z.t * value (* next
   index, iterator *)

   and frame = { code : Phir.code; globals : int; (* module globals Dict address
   *) ns : int; (* namespace Dict for Name ops (= globals except class bodies)
   *) slots : value Int_map.t; (* localsplus; absent = unbound *) stack : value
   list; (* operand stack, top first *) idx : int; (* next instruction *)
   closure : value list; (* the function's closure cells (Copy_free_vars) *)
   } *)
and t_kind = svalue
and t_node = { kind : t_kind; ty : ty }
and t = t_node hash_consed [@@deriving show { with_path = false }, eq, ord]

let unique_tag t = t.tag
let hash t = t.tag
let kind t = t.node.kind
let is_bool_ty = function TBool -> true | _ -> false

let rec iter_vars (sv : t) (f : Var.t * ty -> unit) : unit =
  match sv.node.kind with
  | Var v -> f (v, sv.node.ty)
  | Bool _ | Int _ -> ()
  | Binop (_, l, r) ->
      iter_vars l f;
      iter_vars r f
  | Unop (_, sv) -> iter_vars sv f
  | Nop (_, l) -> List.iter (fun sv -> iter_vars sv f) l
  | Ite (c, t, e) ->
      iter_vars c f;
      iter_vars t f;
      iter_vars e f
  | _ -> failwith "ToDo"

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
  | _ -> failwith "ToDo"

let rec sure_neq a b =
  (not (equal_ty a.node.ty b.node.ty))
  ||
  match (a.node.kind, b.node.kind) with
  | Bool a, Bool b -> a <> b
  | Int a, Int b -> not (Z.equal a b)
  | Float a, Float b -> a <> b
  | Str a, Str b -> a <> b
  | Tuple a, Tuple b -> (
    try
      List.for_all2 sure_neq a b
    with Invalid_argument _ -> false
  )
  | Ref _, Ref _ -> failwith "ToDo"
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
    | _ -> failwith "Todo"
end)

let ( <| ) kind ty : t = Hcons.hashcons { kind; ty }
let mk_var v ty = Var v <| ty

(** We put commutative binary operators in some sort of normal form where
    element with the smallest id is on the LHS, to increase cache hits. *)
let mk_commut_binop op l r =
  if l.tag <= r.tag then Binop (op, l, r) else Binop (op, r, l)

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
| h::[] -> h
| h::t -> List.fold_left and_ h t

let rec not sv =
  if equal sv v_true then v_false
  else if equal sv v_false then v_true
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

let ite guard if_ else_ =
  match (guard.node.kind, if_.node.kind, else_.node.kind) with
  | Bool true, _, _ -> if_
  | Bool false, _, _ -> else_
  | _, Bool true, Bool false -> guard
  | _, Bool false, Bool true -> not guard
  | _, Bool false, _ -> and_ (not guard) else_
  | _, Bool true, _ -> or_ guard else_
  | _, _, Bool false -> and_ guard if_
  | _, _, Bool true -> or_ (not guard) if_
  | _ when equal if_ else_ -> if_
  | _ -> Ite (guard, if_, else_) <| if_.node.ty
