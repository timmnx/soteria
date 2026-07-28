open Aux
open Py_value

type err = [ `ErrorString of string ]
type miss = [ `Miss of unit ]
type 'a res = ('a * state, err, miss) Symex.Result.t
type 'a r = state -> 'a res

let string_to_err (x : string) : err = `ErrorString x
let get_state : state r = fun st -> Symex.Result.ok (st, st)
let set_state (st : state) : unit r = fun _ -> Symex.Result.ok ((), st)
let modify_state g : unit r = fun st -> Symex.Result.ok ((), g st)


let to_err (x: 'a) : err = failwith "ToDo : Implement function PySymex.to_err"
let to_miss (x: 'a) : miss = failwith "ToDo : Implement function PySymex.to_miss"
let wrap (v : ('a, 'b, 'c) Symex.Result.t) : 'a r =
  fun st ->
    Symex.Result.map (fun x -> x, st) v
    |> Symex.Result.map_error to_err
    |> Symex.Result.map_missing to_miss


module Syntax = struct
  let return (x : 'a) : 'a r = fun st -> Symex.Result.ok (x, st)
  let fail e : 'a r = fun st -> Symex.Result.error (string_to_err e)
  let raise_py a b : 'a r = fun _ -> Symex.Result.error (string_to_err (a ^ b))

  let ( let** ) (m : 'a r) (f : 'a -> 'b r) : 'b r =
   fun st -> Symex.Result.bind (fun (x, st') -> f x st') (m st)

  let ( let++ ) (m : 'a r) (f : 'a -> 'b) : 'b r =
   fun st -> Symex.Result.map (fun (x, st') -> (f x, st')) (m st)
end
