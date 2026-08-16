type instr =
  | PUSH_UNIT
  | PUSH_BOOL of bool
  | PUSH_INT of int
  | PUSH_FLOAT of float
  | PUSH_RAND of Ast.Type.t
  | BINOP of Ast.BinOp.t
  | LOAD_NAME of string
  | STORE_NAME of string
  | DELETE_NAME of string
  | MAKE_FUNCTION of int
  | MAKE_REC_FUNCTION of string * int
  | CALL
  | JUMP of int
  | JUMP_IF_FALSE of int
  | POP

let show_instr = function
  | PUSH_UNIT -> "PUSH_UNIT"
  | PUSH_BOOL b -> "PUSH_BOOL         \t" ^ string_of_bool b
  | PUSH_INT i -> "PUSH_INT          \t" ^ string_of_int i
  | PUSH_FLOAT f -> "PUSH_FLOAT        \t" ^ string_of_float f
  | PUSH_RAND ty -> "PUSH_RAND         \t" ^ Ast.Type.show ty
  | BINOP op -> "BINOP             \t" ^ Ast.BinOp.show op
  | LOAD_NAME name -> "LOAD_NAME         \t" ^ name
  | STORE_NAME name -> "STORE_NAME        \t" ^ name
  | DELETE_NAME name -> "DELETE_NAME       \t" ^ name
  | MAKE_FUNCTION i -> "MAKE_FUNCTION     \t" ^ string_of_int i
  | MAKE_REC_FUNCTION (f, i) -> "MAKE_REC_FUNCTION \t" ^ f ^ " " ^ string_of_int i
  | CALL -> "CALL"
  | JUMP i -> "JUMP              \t" ^ string_of_int i
  | JUMP_IF_FALSE i -> "JUMP_IF_FALSE     \t" ^ string_of_int i
  | POP -> "POP"
;;

type code_obj =
  { param : string
  ; body : instr array
  }

type program =
  { consts : code_obj array
  ; code : instr array
  }

let pp_instr fmt instr = Format.fprintf fmt "%s" (show_instr instr)

let pp_array pp fmt body =
  Format.fprintf fmt "@[<v>";
  Array.iteri (fun i instr -> Format.fprintf fmt "%d \t %a @;" i pp instr) body;
  Format.fprintf fmt "@]"
;;

let pp_code_obj fmt c =
  Format.fprintf fmt "@[<v>param: %s @;body : %a @]" c.param (pp_array pp_instr) c.body
;;

let pp_program fmt p =
  Format.fprintf
    fmt
    "@[<v>consts: %a @;main  : %a @]"
    (pp_array pp_code_obj)
    p.consts
    (pp_array pp_instr)
    p.code
;;
