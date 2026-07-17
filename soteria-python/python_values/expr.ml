open Soteria
open Soteria_std
open Svalue

(* module Garbage : sig type t val pp : Ppx_deriving_runtime.Format.formatter ->
   t -> Ppx_deriving_runtime.unit val show : t -> Ppx_deriving_runtime.string
   val of_value : 'a t -> t (* Obtain a syntactic representation from a semantic
   value. This implicitly uses an identity substitution. *)

   val ty : t -> 'a ty (* Gets the type associated to a syntactic values. *)

   val subst : (t -> 'a t) -> t -> 'b t (* Convenience function *)

   module Subst : sig type t end end *)

type t = Svalue.t [@@deriving show { with_path = false }]

(* let pp = failwith "Ignore (ToDo in Expr.pp)" *)
(* let show = failwith "Ignore (ToDo in Expr.show)" *)
let ty (s : t) : Svalue.ty = s.node.ty
let of_value v = v
let subst f v = f v

module Subst = struct
  module Raw_map = PatriciaTree.MakeMap (struct
    type t = Svalue.t

    let to_int = Svalue.unique_tag
    let pp = Svalue.pp
  end)

  (* type expr = t *)
  type t = Svalue.t Raw_map.t

  let pp = Raw_map.pp Svalue.pp
  let extend s v subst = Raw_map.add_assert_new s v subst
  let find_opt s subst = Raw_map.find_opt s subst
  let empty = Raw_map.empty

  let rec apply ~missing_var (s : t) (v : Svalue.t) =
    match find_opt v s with
    | Some v -> (v, s)
    | None -> (
        match v.node.kind with
        | Var x ->
            let v' = missing_var x v.node.ty in
            let s = extend v v' s in
            (v', s)
        | Bool _ | Int _ -> (v, s)
        | Unop (unop, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Svalue.mk_unop unop v1, s)
        | Binop (binop, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.mk_binop binop v1 v2, s)
        | Ite (cond, v1, v2) ->
            let cond, s = apply ~missing_var s cond in
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.SBool.ite cond v1 v2, s)
        | Nop (nop, vs) ->
            let vs, s = apply_list ~missing_var s vs in
            (Svalue.mk_nop nop vs, s)
        | _ -> failwith "ToDo in Expr.Subst.apply")

  and apply_list ~missing_var s vs =
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list ~missing_var s vs in
        (v :: vs, s)

  let rec learn (s : t) (e : Svalue.t) (v : Svalue.t) : t option =
    match e.node.kind with
    | Var _ -> if Raw_map.mem e s then Some s else Some (extend e v s)
    | Unop (Unop.Not, e') -> learn s e' (not v)
    (* This pattern matching can be extended for any reversible operation. *)
    | _ -> None
end
