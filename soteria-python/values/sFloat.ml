open Definitions
open Hc
open Soteria
open Soteria_std

(** {2 Integers + Float + Str} *)

let float_int i = Float (float_of_int i) <| TFloat
let float_int_z z = Float (Z.to_float z) <| TFloat
let float f = Float f <| TFloat

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else float_int_z z

let nonzero_int x = if x = 0 then raise (Invalid_argument "nonzero") else float_of_int x

let nonzero x = if x = 0. then raise (Invalid_argument "nonzero") else float x
let zero = float 0.
let one = float 1.

(* ToDo *)