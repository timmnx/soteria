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

type t = Svalue.t

let pp = failwith "Ignore (ToDo)"
let show = failwith "Ignore (ToDo)"
let ty (s : t) : Svalue.ty = s.node.ty
let of_value v = v
let subst f v = f v

module Subst = struct
  type expr = t
  type t = unit

  let pp = failwith "Ignore (ToDo)"
  let empty = failwith "Ignore (ToDo)"
  let apply = failwith "Ignore (ToDo)"
  let learn = failwith "Ignore (ToDo)"
end
