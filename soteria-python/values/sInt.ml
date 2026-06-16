open Definitions
open Hc
open Soteria
open Soteria_std

(** {2 Integers} *)

let int_z z = Int z <| TInt
let int i = int_z (Z.of_int i)

let nonzero_z z =
  if Z.equal Z.zero z then raise (Invalid_argument "nonzero_z") else int_z z

let nonzero x = if x = 0 then raise (Invalid_argument "nonzero") else int x
let zero = int_z Z.zero
let one = int_z Z.one

let rec add v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero -> v2
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.add i1 i2)
  | Binop (Add, v1, { node = { kind = Int i2; _ }; _ }), Int i3 ->
      add v1 (int_z (Z.add i2 i3))
  | Binop (Add, { node = { kind = Int i1; _ }; _ }, v2), Int i3 ->
      add (int_z (Z.add i1 i3)) v2
  | Int i1, Binop (Add, v1, { node = { kind = Int i2; _ }; _ }) ->
      add (int_z (Z.add i1 i2)) v1
  | Int i1, Binop (Add, { node = { kind = Int i2; _ }; _ }, v2) ->
      add (int_z (Z.add i1 i2)) v2
  (* | _ -> mk_commut_binop Add v1 v2 <| TInt *)
  | _ -> failwith "Not sure what to do, typing int might be wrong"

let rec sub v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 zero -> v1
  | Int i1, Int i2 -> int_z (Z.sub i1 i2)
  | Var v1, Var v2 when Var.equal v1 v2 -> zero
  | Binop (Sub, { node = { kind = Int i2; _ }; _ }, v1), Int i1 ->
      sub (int_z (Z.sub i2 i1)) v1
  | Binop (Sub, v1, { node = { kind = Int i2; _ }; _ }), Int i1 ->
      sub v1 (int_z (Z.add i2 i1))
  | Int i1, Binop (Sub, { node = { kind = Int i2; _ }; _ }, v1) ->
      add (int_z (Z.sub i1 i2)) v1
  | Int i1, Binop (Sub, v1, { node = { kind = Int i2; _ }; _ }) ->
      sub (int_z (Z.add i1 i2)) v1
  | Binop (Add, x, y), _ when equal x v2 -> y
  | Binop (Add, x, y), _ when equal y v2 -> x
  | _, Binop (Add, x, y) when equal x v1 -> sub zero y
  | _, Binop (Add, x, y) when equal y v1 -> sub zero x
  | _ -> Binop (Sub, v1, v2) <| TInt

let mul v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v1 zero || equal v2 zero -> zero
  | _, _ when equal v1 one -> v2
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.mul i1 i2)
  | _ -> mk_commut_binop Mul v1 v2 <| TInt

let div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 when Z.divisible i1 i2 -> int_z (Z.div i1 i2)
  | Int i1, Int i2 -> SFloat.float (Z.to_float i1 /. Z.to_float i2)
  | Binop (Mul, v, { node = { kind = Int i; _ }; _ }), Int j
  | Int j, Binop (Mul, v, { node = { kind = Int i; _ }; _ })
    when Z.equal i j ->
      v
  | Binop (Mul, { node = { kind = Int i; _ }; _ }, v), Int j
  | Int j, Binop (Mul, { node = { kind = Int i; _ }; _ }, v)
    when Z.equal i j ->
      v
  | _ -> Binop (Div, v1, v2) <| TInt

let floor_div v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> v1
  | Int i1, Int i2 -> int_z (Z.div i1 i2)
  | Binop (Mul, v, { node = { kind = Int i; _ }; _ }), Int j
  | Int j, Binop (Mul, v, { node = { kind = Int i; _ }; _ })
    when Z.equal i j ->
      v
  | Binop (Mul, { node = { kind = Int i; _ }; _ }, v), Int j
  | Int j, Binop (Mul, { node = { kind = Int i; _ }; _ }, v)
    when Z.equal i j ->
      v
  | _ -> Binop (Floor_div, v1, v2) <| TInt

let rec is_mod v n =
  match v.node.kind with
  (* | Int i1 -> Z.equal (Z.( mod ) i1 n) Z.zero *)
  | Int i1 -> Z.divisible i1 n
  | Binop (Add, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Sub, v2, v3) -> is_mod v2 n && is_mod v3 n
  | Binop (Mul, v2, v3) -> is_mod v2 n || is_mod v3 n
  | _ -> false

(* let rec rem v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> zero
  | _, Int i2 when is_mod v1 i2 -> zero
  | Int i1, Int i2 -> int_z (Z.rem i1 i2)
  | Binop (Mul, v1, n), Binop (Mul, v2, m) when equal n m -> mul n (rem v1 v2)
  | Binop (Mul, n, v1), Binop (Mul, m, v2) when equal n m -> mul n (rem v1 v2)
  | _ -> Binop (Floor_div, v1, v2) <| v1.node.ty *)

let rec mod_ v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | _, _ when equal v2 one -> zero
  | _, Int i2 when is_mod v1 i2 -> zero
  | Int i1, Int i2 ->
      (* OCaml's mod computes the remainer... *)
      let rem = Z.( mod ) i1 i2 in
      if Z.lt rem Z.zero then int_z (Z.add rem i2) else int_z rem
  | Binop (Mod, x, { node = { kind = Int i1; _ }; _ }), Int i2
    when Z.geq i1 i2 && Z.divisible i1 i2 ->
      mod_ x v2
  (* (x + (y % n)) % m when n | m <=> (x + y) % m *)
  | ( Binop
        ( Add,
          x,
          {
            node =
              { kind = Binop (Mod, l, { node = { kind = Int m1; _ }; _ }); _ };
            _;
          } ),
      Int m2 )
    when Z.geq m1 m2 && Z.divisible m1 m2 ->
      mod_ (add x l) v2
  | _ -> Binop (Mod, v1, v2) <| TInt

let rec pow v1 v2 =
  match v1.node.kind, v2.node.kind with
  | Int i1, Int i2 -> Z.pow i1 (Z.to_int i2) |> int_z
  | _ -> Binop (Pow, v1, v2) <| TInt

let neg v = match v.node.kind with Int i -> int_z (Z.neg i) | _ -> sub zero v

let invert v = match v.node.kind with
  | Int i -> int_z (Z.neg (Z.(+) i Z.one))
  | _ -> sub (-1 |> int) v

(* {2 Equality, comparison, int-bool and int-float conversions} *)

let rec lt v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> SBool.of_bool (Z.lt i1 i2)
  | _, _ when equal v1 v2 -> SBool.v_false
  | _, Binop (Add, v2, v3) when equal v1 v2 -> lt zero v3
  | _, Binop (Add, v2, v3) when equal v1 v3 -> lt zero v2
  | Binop (Add, v1, v3), _ when equal v1 v2 -> lt v3 zero
  | Binop (Add, v1, v3), _ when equal v3 v2 -> lt v1 zero
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 -> lt v2 v4
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 -> lt v1 v4
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 -> lt v2 v3
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 -> lt v1 v3
  | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
      lt (int_z @@ Z.sub y x) v1
  | Binop (Sub, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      lt v1 (int_z @@ Z.add y x)
  | Binop (Sub, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      lt (int_z @@ Z.sub x y) v1
  | Int y, Binop (Sub, v1, { node = { kind = Int x; _ }; _ }) ->
      lt (int_z @@ Z.add y x) v1
  | Int y, Binop (Sub, { node = { kind = Int x; _ }; _ }, v1) ->
      lt v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Mul, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then lt else le in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Mul, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then SBool.of_bool (Z.lt Z.zero y)
      else
        let op = if Z.divisible y x || Z.(y < zero) then lt else le in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ }), Int y
    when Z.leq x y ->
      SBool.v_true
  | Int y, Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ })
    when Z.lt y (Z.neg (Z.abs x)) ->
      SBool.v_true
  | Int _, Ite (b, t, e) -> SBool.ite b (lt v1 t) (lt v1 e)
  | Ite (b, t, e), Int _ -> SBool.ite b (lt t v2) (lt e v2)
  | _ -> Binop (Lt, v1, v2) <| TBool

and le v1 v2 =
  match (v1.node.kind, v2.node.kind) with
  | Int i1, Int i2 -> SBool.of_bool (Z.leq i1 i2)
  | _, _ when equal v1 v2 -> SBool.v_true
  | _, Binop (Add, v2, v3) when equal v1 v2 -> le zero v3
  | _, Binop (Add, v2, v3) when equal v1 v3 -> le zero v2
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 -> le v2 v4
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 -> le v1 v4
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 -> le v2 v3
  | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 -> le v1 v3
  | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      le v1 (int_z @@ Z.sub y x)
  | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
  | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
      le (int_z @@ Z.sub y x) v1
  | Binop (Sub, v1, { node = { kind = Int x; _ }; _ }), Int y ->
      le v1 (int_z @@ Z.add y x)
  | Binop (Sub, { node = { kind = Int x; _ }; _ }, v1), Int y ->
      le (int_z @@ Z.sub x y) v1
  | Int y, Binop (Sub, v1, { node = { kind = Int x; _ }; _ }) ->
      le (int_z @@ Z.add y x) v1
  | Int y, Binop (Sub, { node = { kind = Int x; _ }; _ }, v1) ->
      le v1 (int_z @@ Z.sub x y)
  | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1')
  | Int y, Binop (Mul, v1', { node = { kind = Int x; _ }; _ }) ->
      if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y < zero) then le else lt in
        if Z.(zero < x) then op (int_z Z.(y / x)) v1'
        else op v1' (int_z Z.(y / x))
  | Binop (Mul, v1', { node = { kind = Int x; _ }; _ }), Int y
  | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1'), Int y ->
      if Z.equal Z.zero x then SBool.of_bool (Z.lt y Z.zero)
      else
        let op = if Z.divisible y x || Z.(y > zero) then le else lt in
        if Z.(zero < x) then op v1' (int_z Z.(y / x))
        else op (int_z Z.(y / x)) v1'
  | Binop ((Mod | Floor_div), _, { node = { kind = Int x; _ }; _ }), Int y
    when Z.leq x y ->
      SBool.v_true
  | Int y, Binop (Floor_div, _, { node = { kind = Int x; _ }; _ })
    when Z.leq y (Z.neg (Z.abs x)) ->
      SBool.v_true
  | Int y, Binop (Mod, _, _) when Z.leq y Z.zero -> SBool.v_true
  | Int _, Ite (b, t, e) -> SBool.ite b (le v1 t) (le v1 e)
  | Ite (b, t, e), Int _ -> SBool.ite b (le t v2) (le e v2)
  | _ -> Binop (Le, v1, v2) <| TBool

let ge v1 v2 = le v2 v1
let gt v1 v2 = lt v2 v1

let rec sem_eq v1 v2 =
  if equal v1 v2 then SBool.v_true
  else
    match (v1.node.kind, v2.node.kind) with
    | Int z1, Int z2 -> SBool.of_bool (Z.equal z1 z2)
    | Bool b1, Bool b2 -> SBool.of_bool (b1 = b2)
    | _, Binop (Add, v2, v3) when equal v1 v2 -> sem_eq v3 zero
    | _, Binop (Add, v2, v3) when equal v1 v3 -> sem_eq v2 zero
    | Binop (Add, v1, v3), _ when equal v1 v2 -> sem_eq v3 zero
    | Binop (Add, v1, v3), _ when equal v3 v2 -> sem_eq v1 zero
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v3 -> sem_eq v2 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v1 v4 -> sem_eq v2 v3
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v3 -> sem_eq v1 v4
    | Binop (Add, v1, v2), Binop (Add, v3, v4) when equal v2 v4 -> sem_eq v1 v3
    | Binop (Add, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Add, { node = { kind = Int x; _ }; _ }, v1), Int y
    | Int y, Binop (Add, v1, { node = { kind = Int x; _ }; _ })
    | Int y, Binop (Add, { node = { kind = Int x; _ }; _ }, v1) ->
        sem_eq v1 (int_z @@ Z.sub y x)
    | Binop (Sub, z, { node = { kind = Int x; _ }; _ }), Int y
    | Int y, Binop (Sub, z, { node = { kind = Int x; _ }; _ }) ->
        sem_eq z (int_z @@ Z.add y x)
    | Int y, Binop (Mul, { node = { kind = Int x; _ }; _ }, v1)
    | Int y, Binop (Mul, v1, { node = { kind = Int x; _ }; _ })
    | Binop (Mul, v1, { node = { kind = Int x; _ }; _ }), Int y
    | Binop (Mul, { node = { kind = Int x; _ }; _ }, v1), Int y ->
        if Z.equal Z.zero x then SBool.of_bool (Z.equal Z.zero y)
        else if Z.(equal zero (rem y x)) then sem_eq v1 (int_z Z.(y / x))
        else SBool.v_false
    | _ -> mk_commut_binop Eq v1 v2 <| TBool

let sem_eq_untyped v1 v2 =
  if equal_ty v1.node.ty v2.node.ty then sem_eq v1 v2 else SBool.v_false

let sem_ne v1 v2 =
  if equal v1 v2 then SBool.v_false
  else
    match (v1.node.kind, v2.node.kind) with
    | Int z1, Int z2 -> SBool.of_bool (Z.equal z1 z2 |> not)
    | _ -> sem_eq v1 v2 |> SBool.not

(* {Conversion} *)

let to_bool v =
  match v.node.kind with
  | Int i -> if Z.equal Z.zero i then SBool.v_false else SBool.v_true
  | _ -> Binop (Ne, v, zero) <| TBool

let to_float v =
  match v.node.kind with
  | Int i -> Float (Z.to_float i) <| TFloat
  | _ -> failwith "ToDo"