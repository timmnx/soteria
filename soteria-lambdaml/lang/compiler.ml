open Ast
open Bytecode

type error = string
type state = { consts : code_obj list; next_idx : int }

module StateMonad : sig
  type 'a res = 'a * state
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a * state
  val add_const : code_obj -> int t
end = struct
  type 'a res = 'a * state
  type 'a t = state -> 'a res

  let return x = fun st -> (x, st)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun st ->
    let s', st' = x st in
    f s' st'

  let ( let* ) = bind

  (* Execution *)
  let run (x : 'a t) =
    let empty = { consts = []; next_idx = 0 } in
    x empty

  let add_const co : int t =
   fun st ->
    let idx = st.next_idx in
    let st' = { consts = co :: st.consts; next_idx = st.next_idx + 1 } in
    (idx, st')
end

open StateMonad

let rec compile_const (c : Const.t) : instr =
  match c with
  | Var x -> LOAD_NAME x
  | Unit -> PUSH_UNIT
  | Bool b -> PUSH_BOOL b
  | Int i -> PUSH_INT i
  | Float f -> PUSH_FLOAT f
  | Rand ty -> PUSH_RAND ty

let rec compile (e : Expr.t) : instr list t =
  match e with
  | Cst c -> return [ compile_const c ]
  | Let (x, e1, e2) ->
      let* e1' = compile e1 in
      let* e2' = compile e2 in
      return (e1' @ (STORE_NAME x :: e2') @ [ DELETE_NAME x ])
  | Fun (x, body) ->
      let* body' = compile body in
      let* idx = add_const { param = x; body = Array.of_list body' } in
      return [ MAKE_FUNCTION idx ]
  | FixFun (f, x, body) ->
      let* body' = compile body in
      let* idx = add_const { param = x; body = Array.of_list body' } in
      return [ MAKE_REC_FUNCTION (f, idx) ]
  | App (e1, e2) ->
      let* e1' = compile e1 in
      let* e2' = compile e2 in
      return (e1' @ e2' @ [ CALL ])
  | If (c, t, e) ->
      let* c' = compile c in
      let* t' = compile t in
      let* e' = compile e in
      (* if false, jump past then-branch (+1 for the JUMP that follows it) *)
      return
        (c'
        @ (JUMP_IF_FALSE (List.length t' + 1) :: t')
        @ (JUMP (List.length e') :: e'))
  | Binop (op, l, r) ->
      let* l' = compile l in
      let* r' = compile r in
      return (l' @ r' @ [ BINOP op ])

let compile_program (e : Expr.t) : program =
  let code, st = compile e |> run in
  { consts = Array.of_list (List.rev st.consts); code = Array.of_list code }
