include Aux
open Soteria.Soteria_std.Syntaxes.FunctionWrap
open Symex
open Symex.Syntax
open S_val.Infix

(* Python AST *)
open Boot
open Py_value
module Phir = Pytecode.Phir
module Ast = Pytecode.Ast

(* module InterpM (State : State_intf.S) = struct module StateM = State.SM
   module SSM = Soteria.Sym_states.State_monad.Make (StateM) (Store) open SSM

   type 'a t = ('a, Error.with_trace, State.syn list) SSM.Result.t end *)

type frame_outcome = Returned of value | Yielded of value * frame

(* What executing one instruction does to the frame. *)
type istep = Next of frame | Goto of frame * int | Fin of frame_outcome

let push f v = { f with stack = v :: f.stack }

let pop f =
  match f.stack with
  | v :: rest -> (v, { f with stack = rest })
  | [] -> invalid_arg "pop: empty operand stack"

let advance f = { f with idx = f.idx + 1 }

(* ------------------------------------------------------------------ *)
(* The interpreter proper: one big recursive knot                      *)
(* ------------------------------------------------------------------ *)

(* ---------- dictionaries (insertion-ordered association lists) ----- *)
module rec Dictionaries : sig
  val dict_find :
    state ->
    (value * value) list ->
    'b ->
    (value option * state, 'err, 'a) Result.t

  val check_hashable : state -> value -> (unit * state, 'err, 'a) Result.t

  val dict_set :
    state -> int -> value -> value -> (unit * state, 'err, 'a) Result.t

  val dict_del : state -> int -> value -> (bool * state, 'err, 'a) Result.t
  val dget : state -> int -> value -> (value option * state, 'err, 'a) Result.t
end = struct
  let dict_find = failwith "ToDo"
  let check_hashable = failwith "ToDo"
  let dict_set = failwith "ToDo"
  let dict_del = failwith "ToDo"
  let dget = failwith "ToDo"
end

(* ---------- equality ----------------------------------------------- *)
and Equality : sig
  val cmp_unwrap :
    state -> value -> string list -> (value * state, 'err, 'a) Result.t

  val py_eq : state -> value -> value -> (bool * state, 'err, 'a) Result.t
end = struct
  let cmp_unwrap st v (dunders : string list) :
      (value * state, 'err, 'a) Symex.Result.t =
    match deref st v with
    | Some (Instance { cls; native; _ })
      when not (S_val.SOthers.is_none_ native) ->
        let rec ov st = function
          | [] -> Result.ok (false, st)
          | d :: rest ->
              let** m, st = ClassesAndAttributes.type_lookup st cls d in
              if m <> None then Result.ok (true, st) else ov st rest
        in
        let** overridden, st = ov st dunders in
        Result.ok ((if overridden then v else native), st)
    | _ -> Result.ok (v, st)

  let eq_dunders = [ "__eq__"; "__ne__" ]
  let order_dunders = [ "__lt__"; "__gt__"; "__le__"; "__ge__" ]

  let py_eq st a b : (bool * state, 'err, 'a) Result.t =
    let** a, st = cmp_unwrap st a eq_dunders in
    let** b, st = cmp_unwrap st b eq_dunders in
    failwith "ToDo"
end
(* ---------- ordering ----------------------------------------------- *)

(* TODO *)

(* ---------- truthiness and length ---------------------------------- *)

(* TODO *)

(* ---------- repr and str ------------------------------------------- *)

(* TODO *)

(* ---------- iteration ---------------------------------------------- *)

(* TODO *)

(* ---------- classes and attributes --------------------------------- *)

and ClassesAndAttributes : sig
  val type_lookup :
    state -> int -> string -> (value option * state, 'err, 'a) Result.t
end = struct
  let type_lookup = failwith "ToDo"
end

(* ---------- isinstance / issubclass -------------------------------- *)

(* TODO *)

(* ---------- class creation ----------------------------------------- *)

(* TODO *)

(* ---------- generators --------------------------------------------- *)

(* TODO *)

(* ---------- operators ----------------------------------------------- *)
and Operators : sig
  val are_addable : value -> value -> (value * value, string, 'a) Result.t
  val cast_to_bool : value -> (value, string, 'a) Result.t

  val binary :
    Phir.binop ->
    inplace:'b ->
    value ->
    value ->
    state ->
    (value * state, string, 'a) Result.t

  val unary :
    Phir.unop -> value -> state -> (value * state, string, 'a) Result.t
end = struct
  let are_addable (v1 : value) (v2 : value) :
      (value * value, string, 'a) Result.t =
    match S_val.are_addable v1 v2 with
    | Some (v1', v2') -> Result.ok (v1', v2')
    | None -> Result.error "Type error in add"

  let cast_to_bool v =
    match S_val.cast_checked v S_val.t_bool with
    | Some v -> Result.ok v
    | None -> Result.error "Type error"

  let binary (op : Phir.binop) ~inplace (a : value) (b : value) st :
      (S_val.t * state, 'err, 'a) Result.t =
    let** v =
      match op with
      | Add ->
          let++ v1, v2 = are_addable a b in
          v1 +@ v2
      | And -> failwith "ToDo: a &@ b"
      | Floor_div -> failwith "ToDo: a //@ b"
      | Lshift -> failwith "ToDo: a <<@ b"
      | Mat_mul -> failwith "ToDo: a @@ b"
      | Mul -> failwith "ToDo: a *@ b"
      | Mod -> failwith "ToDo: a %@ b"
      | Or -> failwith "ToDo: a |@ b"
      | Pow -> failwith "ToDo: a ^@ b"
      | Rshift -> failwith "ToDo: a >>@ b"
      | Sub -> failwith "ToDo: a -@ b"
      | Div -> failwith "ToDo: a /@ b"
      | Xor -> failwith "ToDo: a ^@ b"
    in
    Result.ok (v, st)

  let rec unary (op : Phir.unop) (v : value) st :
      (value * state, 'err, 'a) Result.t =
    let** v' =
      match op with
      | Negative -> ~-v |> Result.ok
      | Not -> ~!v |> Result.ok
      | Invert -> ~~v |> Result.ok
      | To_bool -> cast_to_bool v
    in
    Result.ok (v', st)
end
(* ---------- subscripts and slices ----------------------------------- *)

(* TODO *)

(* ---------- sorting (monadic merge sort: comparisons run user code) -- *)

(* TODO *)

(* ---------- variables ---------------------------------------------- *)
and Variables : sig
  val store_var :
    state -> frame -> Phir.var -> value -> (frame * state, 'err, 'a) Result.t
end = struct
  (* ref: 4.2.2 Resolution of names — search the namespace chain in order (e.g.
   local/enclosing, then global, then builtins), raising NameError if absent. *)
  (* let name_chain_lookup st (f : frame) chain s : value r =
    let rec go st = function
      | [] ->
          raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)
      | d :: rest -> (
          let* found, st = dget st d (Str s) in
          match found with Some v -> Ok (v, st) | None -> go st rest)
    in
    go st (chain f) *)

  (* ref: 4.2 Naming and binding — read a variable. A local (Fast) slot, a closure
   cell (Deref, a free/cell variable), or a Name/Global resolved through the
   namespace chain (4.2.2); an unset local raises UnboundLocalError (4.2.1). *)
  (* let load_var st (f : frame) (v : Phir.var) : value r =
    match v with
    | Fast i -> (
        match Int_map.find_opt i f.slots with
        | Some v -> Ok (v, st)
        | None ->
            raise_py st "UnboundLocalError"
              (Printf.sprintf
                "cannot access local variable '%s' where it is not associated \
                  with a value"
                (fst f.code.localsplus.(i))))
    | Deref i -> (
        match Int_map.find_opt i f.slots with
        | Some (Ref ca) -> (
            match heap_get st ca with
            | Cell (Some v) -> Ok (v, st)
            | Cell None ->
                raise_py st "NameError"
                  (Printf.sprintf
                    "free variable '%s' referenced before assignment"
                    (fst f.code.localsplus.(i)))
            | _ -> raise_py st "RuntimeError" "deref of non-cell")
        | _ -> raise_py st "RuntimeError" "deref of unbound slot")
    | Name s ->
        name_chain_lookup st f (fun f -> [ f.ns; f.globals; st.builtins ]) s
    | Global s -> name_chain_lookup st f (fun f -> [ f.globals; st.builtins ]) s *)

  (* ref: 4.2.1 Binding of names / 7.2 Assignment statements — bind a variable:
     write a local slot, a closure cell, or a namespace (ns/globals) entry. *)
  let store_var st (f : frame) (x : Phir.var) v :
      (frame * state, 'err, 'a) Result.t =
    match x with
    | Fast i ->
        let slots =
          if S_val.SOthers.is_null v then Int_map.remove i f.slots
          else Int_map.add i v f.slots
        in
        Result.ok ({ f with slots }, st)
    | Deref i ->
        failwith
          {|(
      match Int_map.find_opt i f.slots with
      | Some (Ref ca) -> Ok (f, heap_set st ca (Cell (Some v)))
      | _ -> raise_py st "RuntimeError" "store to non-cell slot")|}
    | Name s ->
        failwith
          {|
      let* (), st = dict_set st f.ns (Str s) v in
      Ok (f, st)|}
    | Global s ->
        failwith
          {|
      let* (), st = dict_set st f.globals (Str s) v in
      Ok (f, st)|}

  (* ref: 7.5 The del statement / 4.2.1 — unbind a variable; deleting an unbound
   local raises UnboundLocalError, an unbound global/name a NameError. *)
  (* let del_var st (f : frame) (x : Phir.var) : frame r =
    match x with
    | Fast i ->
        if Int_map.mem i f.slots then
          Ok ({ f with slots = Int_map.remove i f.slots }, st)
        else
          raise_py st "UnboundLocalError"
            (Printf.sprintf
              "cannot access local variable '%s' where it is not associated \
                with a value"
              (fst f.code.localsplus.(i)))
    | Deref i -> (
        match Int_map.find_opt i f.slots with
        | Some (Ref ca) -> Ok (f, heap_set st ca (Cell None))
        | _ -> raise_py st "RuntimeError" "delete of non-cell slot")
    | Name s ->
        let* removed, st = dict_del st f.ns (Str s) in
        if removed then Ok (f, st)
        else raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)
    | Global s ->
        let* removed, st = dict_del st f.globals (Str s) in
        if removed then Ok (f, st)
        else raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s) *)
end

(* ---------- operand evaluation -------------------------------------- *)
and OperandEvaluation : sig
  val const_value : Ast.const -> (value, 'err, 'a) Result.t

  val eval_operands :
    state ->
    frame ->
    Phir.value list ->
    ((value list * frame) * state, 'err, 'a) Result.t
end = struct
  let const_value (c : Ast.const) : (value, 'err, 'a) Result.t =
    match c with
    | None_ -> failwith "ToDo"
    (* | Bool b -> Result.ok (S_val.of_bool b, st) *)
    | Bool b -> S_val.of_bool b |> Result.ok
    | Int i -> S_val.SInt.int_z i |> Result.ok
    | Float f -> S_val.SFloat.float f |> Result.ok
    | Complex { re; im } -> failwith "ToDo"
    | Str _ -> failwith "ToDo"
    | Bytes _ -> failwith "ToDo"
    | Tuple _ -> failwith "ToDo"
    | Frozenset _ -> failwith "ToDo"
    | Code _ -> failwith "ToDo"
    | Ellipsis -> failwith "ToDo"

  let eval_operands st (f : frame) (ops : Phir.value list) :
      ((value list * frame) * state, 'err, 'a) Result.t =
    let n_stack =
      List.length (List.filter (function Phir.Stack -> true | _ -> false) ops)
    in
    let popped, rest = take n_stack f.stack in
    let f = { f with stack = rest } in
    let rec go st stacked acc = function
      | [] -> Result.ok (List.rev acc, st)
      | (op : Phir.value) :: more -> (
          match op with
          | Stack -> (
              match stacked with
              | v :: tl -> go st tl (v :: acc) more
              | [] -> assert false)
          | Null -> go st stacked (S_val.SOthers.null :: acc) more
          | Const c ->
              let** v = const_value c in
              go st stacked (v :: acc) more
          | Code c -> failwith "ToDo: go st stacked (Code_obj c :: acc) more"
          | Var v ->
              failwith
                {|let* x, st = load_var st f v in
              go st stacked (x :: acc) more|}
          )
    in
    let** vals, st = go st (List.rev popped) [] ops in
    Result.ok ((vals, f), st)
end
(* ---------- calls ---------------------------------------------------- *)

(* TODO *)

(* ---------- frame execution ----------------------------------------- *)
and FrameExecution : sig
  val exec_instr :
    state -> frame -> Phir.instr -> (istep * state, 'err, 'a) Symex.Result.t

  val run_frame :
    state -> frame -> (frame_outcome * state, 'err, 'a) Symex.Result.t
end = struct
  let exec_instr st (f : frame) (ins : Phir.instr) :
      (istep * state, 'err, 'a) Symex.Result.t =
    let op1 st f v =
      let** (vals, f), st = OperandEvaluation.eval_operands st f [ v ] in
      Result.ok ((List.hd vals, f), st)
    in
    match ins with
    | Assign (x, v) ->
        let** (v, f), st = op1 st f v in
        let** f, st = Variables.store_var st f x v in
        Result.ok (Next f, st)
    | _ -> failwith "ToDo"

  let rec run_frame st (f : frame) :
      (frame_outcome * state, 'err, 'a) Symex.Result.t =
    let** v = exec_instr st f f.code.instrs.(f.idx) in
    match v with
    | Next f', st -> run_frame st (advance f')
    | Goto (f', t), st -> run_frame st { f' with idx = t }
    | Fin out, st -> Result.ok (out, st)
end
(* ---------- builtin type constructors ------------------------------- *)

(* TODO *)

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

(* TODO *)

let run_module (code : Phir.code) =
  let st = boot () in
  let globals, st =
    alloc st (Dict [ (S_val.SStr.str "__name__", S_val.SStr.str "__main__") ])
  in
  let frame =
    {
      code;
      globals = addr globals;
      ns = addr globals;
      slots = Int_map.empty;
      stack = [];
      idx = 0;
      closure = [];
    }
  in
  let process = FrameExecution.run_frame st frame in
  let results = Symex.Result.run ~mode:OX process in
  results
(* let go () = match run_frame st frame with | Ok (Returned _, st) -> Ok
   (collected_output st) | Ok (Yielded _, _) -> Error "module-level yield?" |
   Error (exc, st) -> let msg = match py_str st exc with Ok (s, _) -> s | Error
   _ -> "<unprintable>" in Error (Printf.sprintf "Uncaught %s: %s" (type_name st
   exc) msg) in match handle go with | result -> result | exception
   Stack_overflow -> Error "OCaml stack overflow" | exception e -> Error
   ("interpreter bug: " ^ Printexc.to_string e) *)
