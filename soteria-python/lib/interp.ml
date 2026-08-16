include Aux
open Symex
open Symex.Syntax
open S_val.Infix
open Soteria.Soteria_std.Syntaxes.FunctionWrap

(* Python AST *)
open Boot
open Py_value
module Phir = Pytecode.Phir
module Ast = Pytecode.Ast
open Soteria.Logs.Import

type err = string

let raise_py _ a b = Result.error (a ^ b)
let raise_exc (e : value) = Result.error (S_val.show e)

type frame_outcome = Returned of value | Yielded of value * frame

(* What executing one instruction does to the frame. *)
type istep = Next of frame | Goto of frame * int | Fin of frame_outcome

let push f v = { f with stack = v :: f.stack }

let pop f =
  match f.stack with
  | v :: rest -> (v, { f with stack = rest })
  | [] -> invalid_arg "pop: empty operand stack"

let advance f = { f with idx = f.idx + 1 }

(* the underlying payload of a built-in-subclass instance, if any *)
let native_of st v : value option =
  match deref st v with
  | Some (Instance { native; _ }) when not (S_val.SOthers.is_none_ native) -> Some native
  | _ -> None

let rec map_m st (f: 'a -> state -> ('b * state, 'err, 'c) Result.t) = function
  | [] -> Result.ok ([], st)
  | x :: xs ->
      let** y, st = f x st in
      let** ys, st = map_m st f xs in
      Result.ok (y :: ys, st)

(* ------------------------------------------------------------------ *)
(* The interpreter proper: one big recursive knot                      *)
(* ------------------------------------------------------------------ *)

(* ---------- dictionaries (insertion-ordered association lists) ----- *)
module rec Dictionaries : sig
  val dict_find :
    state ->
    (value * value) list ->
    value ->
    (value option * state, err, 'a) Result.t

  val check_hashable : state -> value -> (unit * state, err, 'a) Result.t

  val dict_set :
    state -> int -> value -> value -> (unit * state, err, 'a) Result.t

  val dict_del : state -> int -> value -> (bool * state, err, 'a) Result.t
  val dget : state -> int -> value -> (value option * state, err, 'a) Result.t
end = struct
  let rec dict_find st pairs key : (value option * state, err, 'a) Result.t =
    match pairs with
    | [] -> Result.ok (None, st)
    | (k, v) :: rest -> (
        let** v_eq, st = Equality.py_eq st k key in
        match S_val.to_bool v_eq with
        | Some eq ->
            if eq then Result.ok (Some v, st) else dict_find st rest key
        | None ->
            failwith "Don't know what to do in Interp.Dictionaries.dict_find")

  let check_hashable st key : (unit * state, err, 'a) Result.t =
    match deref (st : state) (key : value) with
    | Some (List _) -> raise_py st "TypeError" "unhashable type: 'list'"
    | Some (Dict _) -> raise_py st "TypeError" "unhashable type: 'dict'"
    | Some (Set _) -> raise_py st "TypeError" "unhashable type: 'set'"
    | Some (Bytearray _) ->
        raise_py st "TypeError" "unhashable type: 'bytearray'"
    | Some (Instance { cls; _ }) ->
        raise_py st "TypeError"
          "unhashable type: ToDo in Interp.Dictionnaries.check_hashable"
    | _ -> Result.ok ((), st)

  let dict_set st a key v : (unit * state, err, 'a) Result.t =
    let** (), st = check_hashable st key in
    let rec go st acc = function
      | [] -> Result.ok (List.rev_append acc [ (key, v) ], st)
      | (k, v0) :: rest -> (
          let** v_eq, st = Equality.py_eq st k key in
          match S_val.to_bool v_eq with
          | Some eq ->
              if eq then Result.ok (List.rev_append acc ((k, v) :: rest), st)
              else go st ((k, v0) :: acc) rest
          | None ->
              failwith
                "Don't know what to do in Interp.Dictionaries.dict_set.go")
    in
    let** pairs, st = go st [] (dict_pairs st a) in
    Result.ok ((), heap_set st a (Dict pairs))

  let dict_del _ = failwith "ToDo (in Lib.Interp.Dictionaries.dict_del)"

  let dget (st : state) (a : int) (key : value) :
      (value option * state, err, 'a) Result.t =
    dict_find st (dict_pairs st a) key
end

(* ---------- equality ----------------------------------------------- *)
and Equality : sig
  val cmp_unwrap :
    state -> value -> string list -> (value * state, err, 'a) Result.t

  val eq_dunders : string list
  val order_dunders : string list
  val py_eq : state -> value -> value -> (value * state, err, 'a) Result.t
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

  let py_eq st a b : (value * state, err, 'a) Result.t =
    let** a, st = cmp_unwrap st a eq_dunders in
    let** b, st = cmp_unwrap st b eq_dunders in
    Result.ok (a ==@ b, st)
end
(* ---------- ordering ----------------------------------------------- *)

and Ordering : sig
  val py_compare_value :
    Phir.cmpop -> value -> value -> state -> (value * state, err, 'a) Result.t
end = struct
  let py_lt a b st : (value * state, 'a, 'b) Result.t =
    let** a, st = Equality.cmp_unwrap st a Equality.order_dunders in
    let** b, st = Equality.cmp_unwrap st b Equality.order_dunders in
    Result.ok (a <@ b, st)

  let py_le a b st : (value * state, 'a, 'b) Result.t =
    let** a, st = Equality.cmp_unwrap st a Equality.order_dunders in
    let** b, st = Equality.cmp_unwrap st b Equality.order_dunders in
    Result.ok (a <=@ b, st)

  let py_compare (op : Phir.cmpop) a b st : (value * state, 'a, 'b) Result.t =
    match op with
    | Eq -> Equality.py_eq st a b
    | Ne ->
        if is_instance_value st a || is_instance_value st b then
          failwith "ToDo: Interp.Ordering.py_compare (if branch)"
        else
          let** e, st = Equality.py_eq st a b in
          Result.ok (S_val.not e, st)
    | Lt -> py_lt a b st
    | Gt -> py_lt b a st
    | Le -> py_le a b st
    | Ge -> py_le b a st

  let py_compare_value (op : Phir.cmpop) a b st :
      (value * state, err, 'a) Result.t =
    let dunders =
      match op with
      | Eq | Ne -> Equality.eq_dunders
      | _ -> Equality.order_dunders
    in
    let** a, st = Equality.cmp_unwrap st a dunders in
    let** b, st = Equality.cmp_unwrap st b dunders in
    if is_instance_value st a || is_instance_value st b then
      failwith "ToDo: Interp.Ordering.py_compare_value (if branch)"
    else py_compare op a b st
end

(* ---------- truthiness and length ---------------------------------- *)
and TruthinessAndLength : sig
  val py_truth : value -> state -> (value * state, err, 'a) Result.t
end = struct
  let py_truth (v : value) st : (value * state, err, 'a) Result.t =
    Result.ok (S_val.to_bool_ v, st)
end

(* ---------- repr and str ------------------------------------------- *)

and ReprAndStr : sig
  val py_str : value -> state -> (string * state, err, 'a) Result.t
end = struct

  let py_repr (v : value) st : (string * state, err, 'a) Result.t =
    failwith "Interp.ReprAndStr.py_repr ToDo"

  let rec py_str (v : value) st : (string * state, err, 'a) Result.t =
    match v with
    | _ when S_val.SStr.is_str v -> Result.ok (S_val.SStr.get_str v |> Option.get, st)
    | _ when S_val.SOthers.is_ref v -> (
      let a = S_val.SOthers.get_ref v |> Option.get in
        match heap_get st a with
        | Instance _ -> (
          let** m, st = ClassesAndAttributes.find_dunder v "__str__" st in
            match m with
            | Some f -> (
                let** r, st = Call.call st f [] [] in
                match r with
                | _ when S_val.SStr.is_str r -> Result.ok (S_val.SStr.get_str r |> Option.get, st)
                | _ ->
                    raise_py st "TypeError"
                      (Printf.sprintf "__str__ returned non-string (type %s)"
                         "(type_name st r)"))
            | None -> (
                match native_of st v with Some p -> py_str p st | None -> py_repr v st))
        | _ -> py_repr v st)
    | _ -> py_repr v st
end

(* ---------- iteration ---------------------------------------------- *)

(* TODO *)

(* ---------- classes and attributes --------------------------------- *)
and ClassesAndAttributes : sig
  val type_lookup :
    state -> int -> string -> (value option * state, err, 'a) Result.t

  val getattr_value :
    state -> value -> string -> (value * state, err, 'a) Result.t

  val bind_class_value :
    value ->
    inst:value ->
    cls_addr:int ->
    state ->
    (value * state, err, 'a) Result.t

  val find_dunder : value -> err -> state -> (value option * state, err, 'a) Result.t

end = struct
  let type_lookup st cls_addr name =
    let rec go st = function
      | [] -> Result.ok (None, st)
      | c :: rest -> (
          let** f, st =
            Dictionaries.dget st (cls_of st c).cdict (S_val.SStr.str name)
          in
          match f with Some v -> Result.ok (Some v, st) | None -> go st rest)
    in
    go st (cls_of st cls_addr).mro

  let attribute_error =
   fun st v name ->
    raise_py st "AttributeError"
      (Printf.sprintf "'%s' object has no attribute '%s'" "(type_name st v)"
         name)

  let getattr_value st (v : value) name =
    (* let bound_builtin tag = if List.mem name (builtin_method_names tag) then
       Result.ok (S_val.SOthers.mk_bound (S_val.SOthers.mk_builtin (tag ^ "." ^
       name), v), st) else attribute_error st v name in *)
    failwith (S_val.show v)
  (* match v with | Str _ -> bound_builtin "str" | Bytes _ -> bound_builtin
     "bytes" | Int z -> ( (* ref: 3.2.4.1 — an int's numeric-tower attributes *)
     match name with | "real" | "numerator" -> Ok (v, st) | "imag" -> Ok (Int
     Z.zero, st) | "denominator" -> Ok (Int Z.one, st) | _ -> ignore z;
     bound_builtin "int") | Bool _ -> ( match name with | "real" | "numerator"
     -> Ok (Int (if v = Bool true then Z.one else Z.zero), st) | "imag" -> Ok
     (Int Z.zero, st) | "denominator" -> Ok (Int Z.one, st) | _ -> bound_builtin
     "int") | Float f -> ( (* ref: 3.2.4.2 — a float's real/imag *) match name
     with | "real" -> Ok (Float f, st) | "imag" -> Ok (Float 0., st) | _ ->
     bound_builtin "float") | Complex (re, im) -> ( (* ref: 3.2.4.3 Complex —
     read-only real/imag, conjugate() *) match name with | "real" -> Ok (Float
     re, st) | "imag" -> Ok (Float im, st) | _ -> bound_builtin "complex") |
     Slice (start, stop, step) -> ( (* ref: 3.2.13 Internal types — slice
     objects expose start/stop/step *) match name with | "start" -> Ok (start,
     st) | "stop" -> Ok (stop, st) | "step" -> Ok (step, st) | _ ->
     attribute_error st v name) | Tuple _ -> bound_builtin "tuple" | Ref a -> (
     match heap_get st a with | List _ -> bound_builtin "list" | Dict _ ->
     bound_builtin "dict" | Set _ -> bound_builtin "set" | Frozenset _ ->
     bound_builtin "frozenset" | Bytearray _ -> bound_builtin "bytearray" |
     Instance { cls; dict; _ } -> instance_getattr st v cls dict name | Class _
     -> class_getattr st a name | Func fn -> ( match name with | "__name__" ->
     Ok (Str fn.code.name, st) | "__qualname__" -> Ok (Str fn.code.qualname, st)
     | "__doc__" -> Ok ( (match fn.code.docstring with | Some s -> Str s | None
     -> None_), st ) | "__defaults__" -> Ok ((if fn.defaults = [] then None_
     else Tuple fn.defaults), st) | _ -> ( let* own, st = dget st fn.fdict (Str
     name) in match own with | Some v -> Ok (v, st) | None -> (* ref: 8.7/8.10 —
     __annotations__ defaults to {}, and __type_params__ to () *) if name =
     "__annotations__" then Ok (alloc st (Dict [])) else if name =
     "__type_params__" then Ok (Tuple [], st) else attribute_error st v name)) |
     Gen _ -> if List.mem name gen_methods then Ok (Bound (Builtin ("generator."
     ^ name), v), st) else attribute_error st v name | Super { cls; self } ->
     super_getattr st ~cls ~self name | Property ({ fget; _ } as p) -> ( match
     name with | "setter" -> Ok (Bound (Builtin "property.setter", v), st) |
     "getter" -> Ok (Bound (Builtin "property.getter", v), st) | "fget" -> Ok
     (fget, st) | "fset" -> Ok (Option.value p.fset ~default:None_, st) | _ ->
     attribute_error st v name) (* ref: 3.3.5 — a GenericAlias exposes
     __origin__/__args__ and otherwise delegates attribute access to its origin
     class *) | Generic_alias { ga_origin; ga_args } -> ( match name with |
     "__origin__" -> Ok (ga_origin, st) | "__args__" -> Ok (Tuple ga_args, st) |
     _ -> getattr_value st ga_origin name) (* ref: 6.7 — a UnionType exposes its
     members as __args__ *) | Union_type members -> ( match name with |
     "__args__" -> Ok (Tuple members, st) | _ -> attribute_error st v name) (*
     ref: 7.14 — __value__ lazily evaluates the alias's value expression *) |
     Type_alias { ta_name; ta_value; ta_type_params } -> ( match name with |
     "__name__" -> Ok (Str ta_name, st) | "__value__" -> call st ta_value [] []
     | "__type_params__" -> Ok (ta_type_params, st) | _ -> attribute_error st v
     name) (* ref: 8.10 — a TypeVar exposes __name__/__bound__/__constraints__;
     bound and constraints are evaluated lazily on access *) | Typevar {
     tv_name; tv_bound; tv_constraints } -> ( match name with | "__name__" -> Ok
     (Str tv_name, st) | "__bound__" -> ( match tv_bound with | None_ -> Ok
     (None_, st) | f -> call st f [] []) | "__constraints__" -> ( match
     tv_constraints with | None_ -> Ok (Tuple [], st) | f -> call st f [] []) |
     _ -> attribute_error st v name) | _ -> attribute_error st v name) | Bound
     (func, self) -> ( (* ref: 3.2.8 Instance methods — a bound method exposes
     __self__/__func__ and otherwise delegates to the underlying function object
     (so m.__name__, m.tag, ... work). *) match name with | "__self__" -> Ok
     (self, st) | "__func__" -> Ok (func, st) | _ -> getattr_value st func name)
     | _ -> attribute_error st v name *)

  let bind_class_value found ~inst ~cls_addr st =
    match deref st found with
    | Some (Func _) ->
        failwith
          "ToDo Interp.ClassesAndAttributes.bind_class_value -> Some (Func _)"
    | Some (Classmethod m) ->
        failwith
          "ToDo Interp.ClassesAndAttributes.bind_class_value -> Some \
           (Classmethod m)"
    | Some (Staticmethod m) -> Result.ok (m, st)
    | _ -> (
        match found with
        | _ when S_val.SOthers.is_builtin found ->
            Result.ok (S_val.SOthers.mk_bound (found, inst), st)
        | _ -> Result.ok (found, st))

  let find_dunder (v: value) name st : (value option * state, err, 'a) Result.t =
    match deref st v with
    | Some (Instance { cls; _ }) -> (
        let** found, st = type_lookup st cls name in
        match found with
        | Some f ->
            let** b, st = bind_class_value f ~inst:v ~cls_addr:cls st in
            Result.ok (Some b, st)
        | None -> Result.ok (None, st))
    | _ -> Result.ok (None, st)
end

(* ---------- isinstance / issubclass -------------------------------- *)
and IsinstanceIssubclass : sig
  val isinstance_value :
    value -> value -> state -> (bool * state, err, 'a) Result.t
end = struct
  let rec isinstance_value (v : value) (cls_v : value) st =
    match cls_v with
    | _ when S_val.SOthers.is_ref cls_v -> (
        let ca = S_val.SOthers.get_ref cls_v |> Option.get in
        match heap_get st ca with
        | Union_type _ ->
            failwith
              "ToDo Interp.IsinstanceIssubclass.isinstance_value case ref -> \
               Union_type"
        | Class { builtin = Some _; _ } -> Result.ok (true, st)
        | Class _ -> (
            match deref st v with
            | Some (Instance { cls; _ }) ->
                Result.ok (List.mem ca (cls_of st cls).mro, st)
            | Some (Class _) ->
                let** ma, st = ClassCreation.metaclass_addr (addr v) st in
                Result.ok (List.mem ca (cls_of st ma).mro, st)
            | _ -> Result.ok (false, st))
        | _ ->
            failwith
              "ToDo Interp.IsinstanceIssubclass.isinstance_value case ref")
    | _ -> failwith "ToDo Interp.IsinstanceIssubclass.isinstance_value"
end

(* ---------- class creation ----------------------------------------- *)
and ClassCreation : sig
  val instantiate :
    int ->
    value list ->
    (string * value) list ->
    state ->
    (value * state, err, 'a) Result.t

  val metaclass_addr : int -> state -> (int * state, 'err, 'a) Result.t
end = struct
  let builtin_base_tag cls_addr st =
    Result.ok
      ( List.find_map
          (fun a ->
            match (cls_of st a).builtin with Some "object" -> None | t -> t)
          (cls_of st cls_addr).mro,
        st )

  let is_native_tag = function
    | "dict" | "list" | "set" | "frozenset" | "int" | "float" | "str" | "tuple"
    | "bytes" ->
        true
    | _ -> false

  let instantiate_plain cls_addr args kwargs st =
    (* ref: 3.3.1 __new__ / __init__ — __new__ creates the instance (implicitly
       a staticmethod, receiving the class); __init__ then initialises it, but
       only when __new__ returned an instance of cls, and __init__ must return
       None. *)
    let** new_m, st = ClassesAndAttributes.type_lookup st cls_addr "__new__" in
    let** inst, st =
      match new_m with
      | Some f ->
          let f = match deref st f with Some (Staticmethod m) -> m | _ -> f in
          Call.call st f (S_val.SOthers.mk_ref cls_addr :: args) kwargs
      | None ->
          let d, st = alloc st (Dict []) in
          alloc st
            (Instance
               { cls = cls_addr; dict = addr d; native = S_val.SOthers.none_ })
          |> Result.ok
    in
    let** is_inst, st =
      IsinstanceIssubclass.isinstance_value inst
        (S_val.SOthers.mk_ref cls_addr)
        st
    in
    if not is_inst then Result.ok (inst, st)
    else
      let** init, st =
        ClassesAndAttributes.type_lookup st cls_addr "__init__"
      in
      match init with
      | Some f -> (
          let** bound, st =
            ClassesAndAttributes.bind_class_value f ~inst ~cls_addr st
          in
          let** rv, st = Call.call st bound args kwargs in
          match rv with
          | _ when S_val.SOthers.is_none_ rv -> Result.ok (inst, st)
          | _ ->
              raise_py st "TypeError"
                (Printf.sprintf "__init__() should return None, not '%s'"
                   "(type_name st rv)"))
      | None -> Result.ok (inst, st)

  let instantiate cls_addr args kwargs st =
    let** tag_opt, st = builtin_base_tag cls_addr st in
    match tag_opt with
    | Some tag when is_native_tag tag -> (
        let** init, st =
          ClassesAndAttributes.type_lookup st cls_addr "__init__"
        in
        let user_init =
          match Option.map (deref st) init with
          | Some (Some (Func _)) -> init
          | _ -> None
        in
        let mutable_container =
          match tag with "list" | "dict" | "set" -> true | _ -> false
        in
        let** payload, st =
          if mutable_container && user_init <> None then
            BuiltinTypeConstructors.builtin_class_call tag [] [] st
          else BuiltinTypeConstructors.builtin_class_call tag args kwargs st
        in
        let d, st = alloc st (Dict []) in
        let inst, st =
          alloc st
            (Instance { cls = cls_addr; dict = addr d; native = payload })
        in
        match user_init with
        | Some f -> (
            let** bound, st =
              ClassesAndAttributes.bind_class_value f ~inst ~cls_addr st
            in
            let** rv, st = Call.call st bound args kwargs in
            match rv with
            | _ when S_val.SOthers.is_none_ rv -> Result.ok (inst, st)
            | _ ->
                raise_py st "TypeError"
                  (Printf.sprintf "__init__() should return None, not '%s'"
                     "(type_name st rv)"))
        | None -> Result.ok (inst, st))
    | _ -> instantiate_plain cls_addr args kwargs st

  let metaclass_addr cls_addr st =
    match (cls_of st cls_addr).meta with
    | Some m -> Result.ok (m, st)
    | None -> Result.ok (builtin_class_addr st "type", st)
end

(* ---------- generators --------------------------------------------- *)

(* TODO *)

(* ---------- operators ----------------------------------------------- *)
and Operators : sig
  val are_addable : value -> value -> (value * value, err, 'a) Result.t
  val cast_to_bool : value -> (value, err, 'a) Result.t

  val binary :
    Phir.binop ->
    inplace:'b ->
    value ->
    value ->
    state ->
    (value * state, err, 'a) Result.t

  val unary : Phir.unop -> value -> state -> (value * state, err, 'a) Result.t
end = struct
  let are_addable (v1 : value) (v2 : value) : (value * value, err, 'a) Result.t
      =
    match S_val.are_addable v1 v2 with
    | Some (v1', v2') -> Result.ok (v1', v2')
    | None -> Result.error "Type error in add"
  (* let are_divisable (v1 : value) (v2 : value) : (value * value, err, 'a)
     Result.t = match S_val.are_divisable v1 v2 with | Some (v1', v2') ->
     Result.ok (v1', v2') | None -> Result.error "Type error in add" *)

  let cast_to_bool v = S_val.to_bool_ v |> Result.ok
  (* match S_val.cast_checked v S_val.t_bool with | Some v -> Result.ok v | None
     -> Result.error "Type error here" *)

  let binary (op : Phir.binop) ~inplace (a : value) (b : value) st :
      (S_val.t * state, err, 'a) Result.t =
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
      | Div ->
          let** v1, v2 = are_addable a b in
          let v2 = S_val.cast_int v2 |> Option.get in
          let++ v2 = S_val.check_nonzero v2 in
          a /@ v2
      | Xor -> failwith "ToDo: a ^@ b"
    in
    Result.ok (v, st)

  let rec unary (op : Phir.unop) (v : value) st :
      (value * state, err, 'a) Result.t =
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
    state -> frame -> Phir.var -> value -> (frame * state, err, 'a) Result.t

  val load_var : state -> frame -> Phir.var -> (value * state, err, 'a) Result.t
end = struct
  (* ref: 4.2.2 Resolution of names — search the namespace chain in order (e.g.
     local/enclosing, then global, then builtins), raising NameError if
     absent. *)
  let name_chain_lookup st (f : frame) chain s :
      (value * state, err, 'a) Result.t =
    let rec go st = function
      | [] ->
          raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)
      | d :: rest -> (
          let** found, st = Dictionaries.dget st d (S_val.SStr.str s) in
          match found with Some v -> Result.ok (v, st) | None -> go st rest)
    in
    go st (chain f)

  (* ref: 4.2 Naming and binding — read a variable. A local (Fast) slot, a
     closure cell (Deref, a free/cell variable), or a Name/Global resolved
     through the namespace chain (4.2.2); an unset local raises
     UnboundLocalError (4.2.1). *)
  let load_var st (f : frame) (v : Phir.var) : (value * state, err, 'a) Result.t
      =
    match v with
    | Fast i -> (
        match Int_map.find_opt i f.slots with
        | Some v -> Result.ok (v, st)
        | None ->
            raise_py st "UnboundLocalError"
              (Printf.sprintf
                 "cannot access local variable '%s' where it is not associated \
                  with a value"
                 (fst f.code.localsplus.(i))))
    | Deref i -> (
        match Int_map.find_opt i f.slots with
        | Some ref_ca when None <> S_val.SOthers.get_ref ref_ca -> (
            let ca = S_val.SOthers.get_ref ref_ca |> Option.get in
            match heap_get st ca with
            | Cell (Some v) -> Result.ok (v, st)
            | Cell None ->
                raise_py st "NameError"
                  (Printf.sprintf
                     "free variable '%s' referenced before assignment"
                     (fst f.code.localsplus.(i)))
            | _ -> raise_py st "RuntimeError" "deref of non-cell")
        | _ -> raise_py st "RuntimeError" "deref of unbound slot")
    | Name s ->
        name_chain_lookup st f (fun f -> [ f.ns; f.globals; st.builtins ]) s
    | Global s -> name_chain_lookup st f (fun f -> [ f.globals; st.builtins ]) s

  (* ref: 4.2.1 Binding of names / 7.2 Assignment statements — bind a variable:
     write a local slot, a closure cell, or a namespace (ns/globals) entry. *)
  let store_var st (f : frame) (x : Phir.var) v :
      (frame * state, err, 'a) Result.t =
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
        let** (), st = Dictionaries.dict_set st f.ns (S_val.SStr.str s) v in
        Result.ok (f, st)
    | Global s ->
        let** (), st =
          Dictionaries.dict_set st f.globals (S_val.SStr.str s) v
        in
        Result.ok (f, st)

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
  val const_value : Ast.const -> (value, err, 'a) Result.t

  val eval_operands :
    state ->
    frame ->
    Phir.value list ->
    ((value list * frame) * state, err, 'a) Result.t
end = struct
  let const_value (c : Ast.const) : (value, err, 'a) Result.t =
    match c with
    | None_ -> S_val.SOthers.none_ |> Result.ok
    (* | Bool b -> Result.ok (S_val.of_bool b, st) *)
    | Bool b -> S_val.SBool.of_bool b |> Result.ok
    | Int i -> S_val.SNumeric.int_z i |> Result.ok
    | Float f -> S_val.SNumeric.float f |> Result.ok
    | Complex { re; im } ->
        failwith "ToDo: Complex (in Interp.OperandEvaluation.const_value)"
    | Str _ -> failwith "ToDo: Str (in Interp.OperandEvaluation.const_value)"
    | Bytes _ ->
        failwith "ToDo: Bytes (in Interp.OperandEvaluation.const_value)"
    | Tuple _ ->
        failwith "ToDo: Tuple (in Interp.OperandEvaluation.const_value)"
    | Frozenset _ ->
        failwith "ToDo: Frozenset (in Interp.OperandEvaluation.const_value)"
    | Code _ -> failwith "ToDo: Code (in Interp.OperandEvaluation.const_value)"
    | Ellipsis ->
        failwith "ToDo: Ellipsis (in Interp.OperandEvaluation.const_value)"

  let eval_operands st (f : frame) (ops : Phir.value list) :
      ((value list * frame) * state, err, 'a) Result.t =
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
              let** x, st = Variables.load_var st f v in
              go st stacked (x :: acc) more)
    in
    let** vals, st = go st (List.rev popped) [] ops in
    Result.ok ((vals, f), st)
end
(* ---------- calls ---------------------------------------------------- *)

and Call : sig
  val call :
    state ->
    value ->
    value list ->
    (string * value) list ->
    (value * state, err, 'a) Result.t
end = struct
  let rec call st (callee : value) (args : value list) kwargs :
      (value * state, 'err, 'a) Result.t =
    match callee with
    | _ when S_val.SOthers.is_builtin callee ->
        let name = S_val.SOthers.get_builtin callee |> Option.get in
        FrameExecution.call_builtin st name args kwargs
    | _ when S_val.SOthers.is_ref callee -> (
        let a = S_val.SOthers.get_ref callee |> Option.get in
        match heap_get st a with
        | Func _ -> failwith "ToDo Interp.Call.call -> case Ref -> case Func"
        | Class { builtin = Some tag; _ } ->
            failwith "ToDo Interp.Call.call -> case Ref -> case Class {tag}"
        | Class { meta = Some meta; _ } ->
            failwith "ToDo Interp.Call.call -> case Ref -> case Class {meta}"
        | Class _ -> ClassCreation.instantiate a args kwargs st
        | Instance _ ->
            failwith "ToDo Interp.Call.call -> case Ref -> case Instance"
        | _ -> failwith "ToDo Interp.Call.call -> case Ref -> not callable")
    | _ when S_val.SOthers.is_bound callee ->
        let g, self = S_val.SOthers.get_bound callee |> Option.get in
        call st g (self :: args) kwargs
    | _ -> failwith "ToDo in Interp.Call.call"
end

(* ---------- frame execution ----------------------------------------- *)
and FrameExecution : sig
  val exec_instr :
    state -> frame -> Phir.instr -> (istep * state, err, 'a) Result.t

  val run_frame : state -> frame -> (frame_outcome * state, err, 'a) Result.t

  val call_builtin :
    state ->
    string ->
    value list ->
    (string * value) list ->
    (value * state, err, 'a) Result.t
end = struct
  let exception_instance (v : value) st =
    match deref st v with
    | Some (Class _) -> Call.call st v [] []
    | Some (Instance _) -> Result.ok (v, st)
    | _ -> raise_py st "TypeError" "exceptions must derive from BaseException"


  let set_exc_attr exc name v st : (unit * state, 'err, 'a) Result.t =
    match deref st exc with
    | Some (Instance { dict; _ }) -> Dictionaries.dict_set st dict (S_val.SStr.str name) v
    | _ -> Result.ok ((), st)

  let set_exc_chain excv ~cause ~suppress st : (unit * state, 'a, 'b) Result.t =
    let ctx = if st.cur_exc = excv then S_val.SOthers.none_ else st.cur_exc in
    let** (), st = set_exc_attr excv "__context__" ctx st in
    let** (), st = set_exc_attr excv "__cause__" cause st in
    set_exc_attr excv "__suppress_context__" (S_val.SBool.of_bool suppress) st

  let exec_instr st (f : frame) (ins : Phir.instr) :
      (istep * state, err, 'a) Result.t =
    [%l.debug "exec_instr on {%a}" (Phir.pp_instr [||]) ins];
    let op1 st f v =
      let** (vals, f), st = OperandEvaluation.eval_operands st f [ v ] in
      Result.ok ((List.hd vals, f), st)
    in
    match ins with
    | Assign (x, v) ->
        let** (v, f), st = op1 st f v in
        let** f, st = Variables.store_var st f x v in
        Result.ok (Next f, st)
    | Return v ->
        let** (v, f), st = op1 st f v in
        ignore f;
        Result.ok (Fin (Returned v), st)
    | Call { f = fv; self; args } -> (
        let** (vals, f), st =
          OperandEvaluation.eval_operands st f (fv :: self :: Array.to_list args)
        in
        match vals with
        | callee :: selfv :: argv ->
            let argv =
              if S_val.SOthers.is_null selfv then argv else selfv :: argv
            in
            let** v, st = Call.call st callee argv [] in
            Result.ok (Next (push f v), st)
        | _ -> assert false)
    | Pop_top v ->
        let** (_, f), st = op1 st f v in
        Result.ok (Next f, st)
    | Load_attr { obj; name; meth } ->
        let** (obj, f), st = op1 st f obj in
        let** v, st = ClassesAndAttributes.getattr_value st obj name in
        let f = push f v in
        Result.ok (Next (if meth then push f S_val.SOthers.null else f), st)
    | Push v ->
        let** (v, f), st = op1 st f v in
        Result.ok (Next (push f v), st)
    | Unary (op, v) ->
        let op_str =
          match op with
          | Negative -> "neg"
          | Not -> "not"
          | Invert -> "invert"
          | To_bool -> "to_bool"
        in
        let** (v, f), st = op1 st f v in
        let** r, st = Operators.unary op v st in
        [%l.debug "Unary (%s, %a): %a" op_str Aux.S_val.pp v Aux.S_val.pp r];
        Result.ok (Next (push f r), st)
    | Binary_op { op; inplace; l; r } -> (
        let** (vals, f), st = OperandEvaluation.eval_operands st f [ l; r ] in
        match vals with
        | [ a; b ] ->
            let** v, st = Operators.binary op ~inplace a b st in
            Result.ok (Next (push f v), st)
        | _ -> assert false)
    | Compare { op; coerce_bool; l; r } -> (
        (* ref: 6.10.1 — keep the raw comparison result unless the bytecode asks
           for bool coercion (boolean context / chained comparison). *)
        let** (vals, f), st = OperandEvaluation.eval_operands st f [ l; r ] in
        match vals with
        | [ a; b ] ->
            let** v, st = Ordering.py_compare_value op a b st in
            let** v, st =
              if coerce_bool then TruthinessAndLength.py_truth v st
              else Result.ok (v, st)
            in
            Result.ok (Next (push f v), st)
        | _ -> assert false)
    | Load_assertion_error ->
        Result.ok
          ( Next
              (push f
                 (S_val.SOthers.mk_ref (builtin_class_addr st "AssertionError"))),
            st )
    | Raise { exc; cause } -> (
        let** (vals, f), st =
          OperandEvaluation.eval_operands st f
            (Option.to_list exc @ Option.to_list cause)
        in
        ignore f;
        match (exc, vals) with
        | None, _ ->
            if S_val.SOthers.is_none_ st.cur_exc then
              raise_py st "RuntimeError" "No active exception to reraise"
            else raise_exc st.cur_exc
        | Some _, [ v ] ->
            let** excv, st = exception_instance v st in
            let** (), st =
              set_exc_chain excv ~cause:S_val.SOthers.none_ ~suppress:false st
            in
            raise_exc excv
        | Some _, [ v; c ] ->
            let** excv, st = exception_instance v st in
            let** cv, st =
              if S_val.SOthers.is_none_ c then
                Result.ok (S_val.SOthers.none_, st)
              else exception_instance c st
            in
            let** (), st = set_exc_chain excv ~cause:cv ~suppress:true st in
            raise_exc excv
        | _ -> assert false)
    | Jump t -> Result.ok (Goto (f, t), st)
    | Cond_jump { cond; v; target } ->
        let** (v, f), st = op1 st f v in
        let b =
          match cond with
          | If_true -> S_val.to_bool_ v
          | If_false -> S_val.not v
          | If_none -> v ==@ S_val.SOthers.none_
          | If_not_none -> v ==@ S_val.SOthers.none_ |> S_val.not
          (* | If_true -> [%l.debug "Cond_jump(If_true)"];
             TruthinessAndLength.py_truth v st | If_false -> [%l.debug
             "Cond_jump(If_false)"]; TruthinessAndLength.py_truth (S_val.not v)
             st (* let** t, st = TruthinessAndLength.py_truth v st in Result.ok
             (S_val.not t, st)) *) | If_none -> [%l.debug "Cond_jump(If_none)"];
             Result.ok (S_val.SOthers.is_none_ v |> S_val.SBool.of_bool, st) |
             If_not_none -> [%l.debug "Cond_jump(If_not_none)"]; Result.ok
             (S_val.SOthers.is_none_ v |> not |> S_val.SBool.of_bool, st) *)
        in
        [%l.debug "Evaluated guard %a -> %a" Aux.S_val.pp v Aux.S_val.pp b];
        if%sat b then Result.ok (Goto (f, target), st)
        else Result.ok (Next f, st)
    | _ ->
        failwith
          ("ToDo in Interp.FrameExecution.exec_instr for "
          ^ Py_value.strinf_of_intr ins)

  let rec run_frame st (f : frame) :
      (frame_outcome * state, 'err, 'a) Symex.Result.t =
    let** v = exec_instr st f f.code.instrs.(f.idx) in
    match v with
    | Next f', st -> run_frame st (advance f')
    | Goto (f', t), st -> run_frame st { f' with idx = t }
    | Fin out, st -> Result.ok (out, st)
  and call_builtin st name (args : value list) (kwargs : (string * value) list)
      : (value * state, 'err, 'a) Symex.Result.t =
    (* let kw k = List.assoc_opt k kwargs in let arity_error () = raise_py st
       "TypeError" (name ^ "(): wrong number of arguments") in *)
    match (name, args) with
    | "Soteria_randint", [] ->
        let* v = S_numeric.fresh_int () in
        Result.ok (v, st)
    | "object.__new__", cls :: _ -> (
        (* create a fresh, empty instance of the given class; extra args are
           ignored (as CPython does when __init__ is overridden) *)
        match deref st cls with
        | Some (Class _) ->
            let d, st = alloc st (Dict []) in
            alloc st
              (Instance
                 { cls = addr cls; dict = addr d; native = S_val.SOthers.none_ })
            |> Result.ok
        | _ ->
            raise_py st "TypeError" "object.__new__(X): X is not a type object")
    | "BaseException.__init__", self :: rest ->
        let** (), st = set_exc_attr self "args" (S_val.SOthers.mk_tuple rest) st in
        Result.ok (S_val.SOthers.none_, st)
    | "print", _ ->
      let** _, st = map_m st ReprAndStr.py_str args in
      Result.ok (S_val.SOthers.none_, st)
    | _ -> failwith ("ToDo Interp.FrameExecution.call_builtin for: " ^ name)
end
(* ---------- builtin type constructors ------------------------------- *)

and BuiltinTypeConstructors : sig
  val builtin_class_call :
    string ->
    value list ->
    (string * value) list ->
    state ->
    (value * state, err, 'a) Result.t
end = struct
  let rec builtin_class_call tag args kwargs st =
    failwith
      ("ToDo Interp.BuiltinTypeConstructors.builtin_class_call fot the tag: "
     ^ tag)
end

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
  let** process = FrameExecution.run_frame st frame in
  match process with
  | Returned v, st -> Result.ok (v, st)
  | _ -> failwith "ToDo (in run_module)"

let pp_results ft
    (v :
      ((value * state, string, 'b) Soteria.Soteria_std.Compo_res.t
      * S_numeric.syn list)
      list) =
  let ( @@ ) = Stdlib.( @@ ) in
  let pp =
    Fmt.list
    @@ Fmt.pair
         (Soteria.Soteria_std.Compo_res.pp
            ~ok:(Fmt.pair ~sep:Fmt.nop S_val.ppa Fmt.nop)
            ~err:Fmt.string ~miss:Fmt.nop)
         (* (Fmt.list S_numeric.pp_syn) *)
         (fun fmt x ->
           Format.fprintf fmt "  @[<v>%a@]" (Fmt.list S_numeric.pp_syn) x)
  in
  pp ft v

let () =
  Soteria.Logs.Config.set_and_lock
    (* (Soteria.Logs.Config.make ~level:(Some Debug) ~kind:Html ~no_color:true
       ()) *)
    (Soteria.Logs.Config.make ~level:(Some Debug) ~kind:Stderr ~no_color:true ())

let run (code : Phir.code) =
  (* Fmt.pr "@[Test@ @]@?"; *)
  let results = Symex.run ~mode:OX (run_module code) in
  Fmt.pr "@[<v 2>Program executed with result:@ %a@]@?" pp_results results
