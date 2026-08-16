%{
  open Soteria_lambdaml_lang.Ast
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token TRUE FALSE
%token LET IN EQUAL
%token FUN FIXFUN ARROW
%token IF THEN ELSE
%token RAND
%token TY_BOOL TY_INT TY_FLOAT
%token LPAREN RPAREN
%token PLUS MINUS STAR SLASH SLASHSLASH
%token EQEQ LT LEQ ANDAND OROR
%token EOF

%left OROR
%left ANDAND
%nonassoc EQEQ LT LEQ
%left PLUS MINUS
%left STAR SLASH SLASHSLASH

%start <Expr.t> program

%%

program:
  | e = expr; EOF { e }
  ;

expr:
  | LET; x = IDENT; EQUAL; e1 = expr; IN; e2 = expr { Expr.Let (x, e1, e2) }
  | FUN; x = IDENT; ARROW; e = expr { Expr.Fun (x, e) }
  | FIXFUN; f = IDENT; x = IDENT; ARROW; e = expr { Expr.FixFun (f, x, e) }
  | IF; c = expr; THEN; t = expr; ELSE; e = expr { Expr.If (c, t, e) }
  | e = op_expr { e }
  ;

op_expr:
  | e1 = op_expr; PLUS; e2 = op_expr { Expr.Binop (BinOp.Add, e1, e2) }
  | e1 = op_expr; MINUS; e2 = op_expr { Expr.Binop (BinOp.Sub, e1, e2) }
  | e1 = op_expr; STAR; e2 = op_expr { Expr.Binop (BinOp.Mul, e1, e2) }
  | e1 = op_expr; SLASH; e2 = op_expr { Expr.Binop (BinOp.Div, e1, e2) }
  | e1 = op_expr; SLASHSLASH; e2 = op_expr { Expr.Binop (BinOp.Floor_Div, e1, e2) }
  | e1 = op_expr; EQEQ; e2 = op_expr { Expr.Binop (BinOp.Eq, e1, e2) }
  | e1 = op_expr; LT; e2 = op_expr { Expr.Binop (BinOp.Lt, e1, e2) }
  | e1 = op_expr; LEQ; e2 = op_expr { Expr.Binop (BinOp.Leq, e1, e2) }
  | e1 = op_expr; ANDAND; e2 = op_expr { Expr.Binop (BinOp.And, e1, e2) }
  | e1 = op_expr; OROR; e2 = op_expr { Expr.Binop (BinOp.Or, e1, e2) }
  | e = app_expr { e }
  ;

app_expr:
  | e1 = app_expr; e2 = atom_expr { Expr.App (e1, e2) }
  | e = atom_expr { e }
  ;

atom_expr:
  | LPAREN; RPAREN { Expr.Cst Const.Unit }
  | LPAREN; e = expr; RPAREN { e }
  | x = IDENT { Expr.Cst (Const.Var x) }
  | i = INT { Expr.Cst (Const.Int i) }
  | f = FLOAT { Expr.Cst (Const.Float f) }
  | TRUE { Expr.Cst (Const.Bool true) }
  | FALSE { Expr.Cst (Const.Bool false) }
  | RAND; LPAREN; t = ty; RPAREN { Expr.Cst (Const.Rand t) }
  ;

ty:
  | TY_BOOL { Type.TBool }
  | TY_INT { Type.TInt }
  | TY_FLOAT { Type.TFloat }
  ;
