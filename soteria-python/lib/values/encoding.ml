open Soteria
open Soteria_std
open Soteria_smt

type t = Svalue.t
type ty = Svalue.ty

let t_ptr, mk_ptr, get_loc, get_ofs, init_commands =
  let ptr = "Ptr" in
  let mk_ptr = "mk-ptr" in
  let loc = "loc" in
  let ofs = "ofs" in
  let cmd =
    declare_datatype ptr [] [ (mk_ptr, [ (loc, t_int); (ofs, t_int) ]) ]
  in

  ( atom ptr,
    (fun l o -> atom mk_ptr $$ [ l; o ]),
    (fun p -> atom loc $$ [ p ]),
    (fun p -> atom ofs $$ [ p ]),
    [ cmd ] )

let sort_of_ty : ty -> sexp = function
  | TBool -> t_bool
  | TInt -> t_int
  | _ -> failwith "Ignore (ToDo)"

let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function
  | Not -> bool_not
  | Negative -> fun x -> a_neg $ x
  | Invert -> failwith "Ignore (ToDo)"
  | To_bool -> failwith "Ignore (ToDo)"

let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
  | Eq -> eq
  | Ne -> fun x y -> a_not $ eq x y
  | Le -> fun x y -> a_leq $$ [ x; y ]
  | Lt -> fun x y -> a_lt $$ [ x; y ]
  | Ge -> fun x y -> a_not $ a_lt $$ [ x; y ]
  | Gt -> fun x y -> a_not $ a_leq $$ [ x; y ]
  | And -> fun x y -> a_and $$ [ x; y ]
  | Or -> fun x y -> a_or $$ [ x; y ]
  | Add -> fun x y -> a_add $$ [ x; y ]
  | Sub -> fun x y -> a_sub $$ [ x; y ]
  | Mul -> fun x y -> a_mul $$ [ x; y ]
  | Div -> fun x y -> a_div $$ [ x; y ]
  | Floor_div -> fun x y -> a_rem $$ [ x; y ]
  | Mod -> fun x y -> a_mod $$ [ x; y ]
  | Pow -> failwith "Does not exist yet (ToDo)"
  | Mat_mul -> failwith "Does not exist yet (ToDo)"
  | BitAnd -> failwith "Does not exist yet (ToDo)"
  | BitOr -> failwith "Does not exist yet (ToDo)"
  | BitXor -> failwith "Does not exist yet (ToDo)"
  | BitLshift -> failwith "Does not exist yet (ToDo)"
  | BitRshift -> failwith "Does not exist yet (ToDo)"

let rec encode_value (v : Svalue.t) =
  match v.node.kind with
  | Var v -> atom (Svalue.Var.to_string v)
  | Int z -> int_zk z
  | Bool b -> bool_k b
  | Ite (c, t, e) ->
      ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
  | Unop (unop, v1) ->
      let v1 = encode_value_memo v1 in
      smt_of_unop unop v1
  | Binop (binop, v1, v2) ->
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      smt_of_binop binop v1 v2
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs
  | None_ | Null | Float _ | Str _ | Tuple _ | Ref _ ->
      failwith "Does not exist yet (ToDo)"

and encode_value_memo v =
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hc.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Hc.tag k;
      k

let encode_value (v : Svalue.t) =
  Svalue.split_ands v |> Iter.map encode_value_memo |> Iter.to_list |> bool_ands
