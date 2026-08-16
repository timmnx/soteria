{
open Parser

exception Lexing_error of string

let keyword_table : (string, token) Hashtbl.t = Hashtbl.create 16

let () =
  List.iter
    (fun (kw, tok) -> Hashtbl.add keyword_table kw tok)
    [ "let", LET
    ; "in", IN
    ; "fun", FUN
    ; "fixfun", FIXFUN
    ; "if", IF
    ; "then", THEN
    ; "else", ELSE
    ; "true", TRUE
    ; "false", FALSE
    ; "Rand", RAND
    ; "bool", TY_BOOL
    ; "int", TY_INT
    ; "float", TY_FLOAT
    ]
;;
}

let digit = ['0'-'9']
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let ident = ident_start ident_char*
let int_lit = digit+
let float_lit = digit+ '.' digit+ (('e' | 'E') ('+' | '-')? digit+)?

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | float_lit as f { FLOAT (float_of_string f) }
  | int_lit as i { INT (int_of_string i) }
  | ident as id {
    match Hashtbl.find_opt keyword_table id with
    | Some tok -> tok
    | None -> IDENT id
    }
  | "->" { ARROW }
  | "<=" { LEQ }
  | "==" { EQEQ }
  | "//" { SLASHSLASH }
  | "&&" { ANDAND }
  | "||" { OROR }
  | '=' { EQUAL }
  | '<' { LT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ as c {Lexing_error (Printf.sprintf "unexpected character %C" c) |> raise}

and comment = parse
  | "*)" { () }
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }
  | eof { Lexing_error "unterminated comment" |> raise}
  | _ { comment lexbuf }
