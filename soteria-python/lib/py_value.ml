(* Core types of the definitional interpreter.

   Everything is pure: the whole interpreter state is one immutable record
   threaded explicitly, mutable Python entities live in a persistent heap
   keyed by integer addresses, and "mutation" is a functional map update. *)

module Phir = Pytecode.Phir
module Ast = Pytecode.Ast
module Int_map = Map.Make (Int)

(* ------------------------------------------------------------------ *)
(* Values                                                              *)
(* ------------------------------------------------------------------ *)

(* Immutable values are immediate; every mutable Python entity (and
   everything with observable identity) is a [Ref] into the heap. *)
type value = Aux.S_val.t

and obj =
  | List of value list
  | Dict of (value * value) list (* insertion-ordered *)
  | Set of value list (* insertion-ordered *)
  | Frozenset of value list (* immutable, hashable set (insertion-ordered) *)
  | Bytearray of string (* a mutable byte array *)
  | Cell of value option (* closure cell; None = empty *)
  | Func of func
  | Class of cls
  | Instance of {
      cls : int;
      dict : int; (* a heap Dict with Str keys *)
      native : value;
          (* for a subclass of a built-in type, the underlying payload value
             (a Ref to a Dict/List/…, or an immediate); None_ otherwise *)
    }
  | Gen of gen
  | Super of { cls : int; self : value } (* bound super object *)
  | Property of { fget : value; fset : value option }
  | Classmethod of value
  | Staticmethod of value
  | Iter of iter
  (* ref: 3.3.5 — types.GenericAlias, e.g. list[int] (origin class + args) *)
  | Generic_alias of { ga_origin : value; ga_args : value list }
  (* ref: 6.7 (PEP 604) — types.UnionType, e.g. int | str (member types) *)
  | Union_type of value list
  (* ref: 7.14 (PEP 695) — typing.TypeAliasType from a `type X = ...` statement;
     ta_value is the lazily-evaluated value-computing function *)
  | Type_alias of {
      ta_name : string;
      ta_value : value;
      ta_type_params : value; (* a tuple of TypeVars, () when non-generic *)
    }
  (* ref: 8.10 (PEP 695) — a typing.TypeVar from a `[T]` type-parameter list.
     bound/constraints are lazily-evaluated functions (None_ when absent). *)
  | Typevar of { tv_name : string; tv_bound : value; tv_constraints : value }

and func = {
  code : Phir.code;
  globals : int; (* module globals Dict address *)
  defaults : value list;
  kwdefaults : (value * value) list; (* Str name -> default *)
  closure : value list; (* Cell refs *)
  fdict : int; (* function attributes (f.x = 1), a heap Dict *)
}

and cls = {
  cname : string;
  bases : int list; (* class addresses *)
  mro : int list; (* C3 linearization, self first *)
  cdict : int; (* class namespace, a heap Dict with Str keys *)
  builtin : string option; (* Some "int" for builtin types like int/str/... *)
  meta : int option;
      (* metaclass address; None means the default [type] (ref: 3.3.3) *)
}

and gen = {
  gframe : frame option; (* None once exhausted *)
  gstarted : bool;
  gkind : [ `Gen | `Coroutine | `Async_gen ];
}

(* Builtin iterators. Each step is a functional heap update. *)
and iter =
  | It_list of int * int (* list address (read live), next index *)
  | It_seq of value list (* remaining items: tuples, dict-key snapshots, ... *)
  | It_str of string * int (* UTF-8 byte offset *)
  | It_range of Z.t * Z.t * Z.t (* next, stop, step *)
  | It_zip of value list (* component iterators *)
  | It_map of value * value list (* function, component iterators *)
  | It_filter of value * value (* predicate (or None_), iterator *)
  | It_enum of Z.t * value (* next index, iterator *)

and frame = {
  code : Phir.code;
  globals : int; (* module globals Dict address *)
  ns : int; (* namespace Dict for Name ops (= globals except class bodies) *)
  slots : value Int_map.t; (* localsplus; absent = unbound *)
  stack : value list; (* operand stack, top first *)
  idx : int; (* next instruction *)
  closure : value list; (* the function's closure cells (Copy_free_vars) *)
}

(* ------------------------------------------------------------------ *)
(* Interpreter state                                                   *)
(* ------------------------------------------------------------------ *)

type state = {
  heap : obj Int_map.t;
  next : int; (* next free address *)
  out : string list; (* program stdout, reversed chunks *)
  cur_exc : value; (* "current exception" (sys.exc_info), None_ if none *)
  builtins : int; (* address of the builtins Dict *)
}

(* The error monad: [Error] carries a raised Python exception object.
   State changes made before a raise persist (Python does not roll back). *)
(* type 'a r = ('a * state, value * state) result

let ( let* ) = Result.bind
let return st v : 'a r = Ok (v, st) *)

let alloc st o : value * state =
  ( Aux.S_val.SOthers.mk_ref st.next,
    { st with heap = Int_map.add st.next o st.heap; next = st.next + 1 } )

let heap_get st addr = Int_map.find addr st.heap
let heap_set st addr o = { st with heap = Int_map.add addr o st.heap }
let deref st v =
  match Aux.S_val.SOthers.get_ref v with
  | Some a -> Some (heap_get st a)
  | _ -> None
let output st s = { st with out = s :: st.out }
let collected_output st = String.concat "" (List.rev st.out)


(* ------------------------------------------------------------------ *)
(* Shared pure helpers (no recursion into the interpreter knot)        *)
(* ------------------------------------------------------------------ *)

let addr v =
  match Aux.S_val.SOthers.get_ref v with
  | Some i -> i
  | _ -> invalid_arg "addr"

let cls_of st a =
  match heap_get st a with Class c -> c | _ -> invalid_arg "cls_of"

let dict_pairs st a =
  match heap_get st a with Dict ps -> ps | _ -> invalid_arg "dict_pairs"

(* monadic list combinators (state threaded, short-circuit on Error) *)
(* let rec map_m st f = function
  | [] -> Ok ([], st)
  | x :: xs ->
      let* y, st = f st x in
      let* ys, st = map_m st f xs in
      Ok (y :: ys, st) *)

(* let rec fold_m st f acc = function
  | [] -> Ok (acc, st)
  | x :: xs ->
      let* acc, st = f st acc x in
      fold_m st f acc xs *)

let rec take n = function
  | xs when n = 0 -> ([], xs)
  | x :: xs ->
      let a, b = take (n - 1) xs in
      (x :: a, b)
  | [] -> invalid_arg "take"

let rec drop n xs =
  if n <= 0 then xs else match xs with [] -> [] | _ :: t -> drop (n - 1) t
