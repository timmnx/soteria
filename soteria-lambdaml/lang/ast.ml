module BinOp = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Floor_Div
    | Eq
    | Lt
    | Leq
    | And
    | Or

  let show = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Floor_Div -> "//"
    | Eq -> "=="
    | Lt -> "<"
    | Leq -> "<="
    | And -> "&&"
    | Or -> "||"
  ;;
end

module Type = struct
  type t =
    | TBool
    | TInt
    | TFloat

  let show = function
    | TBool -> "bool"
    | TInt -> "int"
    | TFloat -> "float"
  ;;
end

module Const = struct
  type t =
    | Var of string
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Rand of Type.t

  let rec show = function
    | Var x -> x
    | Unit -> "()"
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Rand ty -> "Rand(" ^ Type.show ty ^ ")"
  ;;
end

module Expr = struct
  type t =
    | Cst of Const.t
    | Let of string * t * t
    | Fun of string * t
    | FixFun of string * string * t
    | App of t * t
    | If of t * t * t
    | Binop of BinOp.t * t * t

  let rec show = function
    | Cst v -> Const.show v
    | Let (x, e, m) -> "let " ^ x ^ " = " ^ show e ^ " in " ^ show m
    | Fun (x, e) -> "fun " ^ x ^ " -> " ^ show e
    | FixFun (f, x, e) -> "fixfun " ^ f ^ " " ^ x ^ " -> " ^ show e
    | App (e1, e2) -> "(" ^ show e1 ^ ") (" ^ show e2 ^ ")"
    | If (cond, e_then, e_else) ->
      "if " ^ show cond ^ " then " ^ show e_then ^ " else " ^ show e_else
    | Binop (op, l, r) -> "(" ^ show l ^ ") " ^ BinOp.show op ^ " (" ^ show r ^ ")"
  ;;
end
