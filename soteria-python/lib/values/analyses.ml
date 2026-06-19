open Soteria
open Soteria_std
open Symex
open Svalue.Infix

(* let log = Soteria.Logging.Logs.L.warn *)
let log _ = ()

module type S = sig
  include Soteria_std.Reversible.Mutable

  val simplify : t -> Svalue.t -> Svalue.t
  val add_constraint : t -> Svalue.t -> Svalue.t * Var.Set.t
  val encode : ?vars:Var.Hashset.t -> t -> Typed.sbool Typed.t Iter.t
end

module None : S = struct
  type t = unit

  let init () = ()
  let backtrack_n () _ = ()
  let save () = ()
  let reset () = ()
  let simplify () v = v
  let add_constraint () v = (v, Var.Set.empty)
  let encode ?vars:_ () = Iter.empty
end
