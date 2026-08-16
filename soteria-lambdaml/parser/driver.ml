open Soteria_lambdaml_lang

let position_str (pos : Lexing.position) =
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
;;

let parse_with_errors (lexbuf : Lexing.lexbuf) : (Ast.Expr.t, string) result =
  try Ok (Parser.program Lexer.token lexbuf) with
  | Lexer.Lexing_error msg ->
    Error (Printf.sprintf "Lexing error at %s: %s" (position_str lexbuf.lex_curr_p) msg)
  | Parser.Error ->
    Error (Printf.sprintf "Syntax error at %s" (position_str lexbuf.lex_curr_p))
;;

let parse_string (s : string) : (Ast.Expr.t, string) result =
  parse_with_errors (Lexing.from_string s)
;;

let parse_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
       let lexbuf = Lexing.from_channel ic in
       Lexing.set_filename lexbuf path;
       parse_with_errors lexbuf)
;;
