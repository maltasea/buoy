%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token <string * string> REGEX
%token <string * string * string> REGEX_REPLACE

%token LET FN IF ELSE WHILE FOR IN RETURN
%token TRUE FALSE NIL NOT
%token AND OR
%token PLUS MINUS STAR SLASH PERCENT
%token PLUSPLUS
%token EQEQ NEQ LT GT LE GE
%token EQUALS
%token MATCH_OP NOT_MATCH_OP
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA COLON
%token NEWLINE
%token EOF

/* Precedence: lowest to highest */
%left OR
%left AND
%nonassoc EQEQ NEQ LT GT LE GE
%nonassoc MATCH_OP NOT_MATCH_OP
%left PLUS MINUS
%left PLUSPLUS
%left STAR SLASH PERCENT
%nonassoc UMINUS UNOT

%start <Ast.program> program

%%

program:
  | sep_lines stmts = terminated_stmts EOF { stmts }
  ;

sep_lines:
  | /* empty */        { }
  | sep_lines NEWLINE  { }
  ;

sep:
  | NEWLINE            { }
  | sep NEWLINE        { }
  ;

terminated_stmts:
  | /* empty */                               { [] }
  | s = stmt sep_lines                        { [s] }
  | s = stmt sep rest = terminated_stmts      { s :: rest }
  ;

block:
  | LBRACE sep_lines stmts = terminated_stmts RBRACE { stmts }
  ;

stmt:
  | LET name = IDENT EQUALS e = expr            { Let (name, e) }
  | name = IDENT EQUALS e = expr                 { Assign (name, e) }
  | target = postfix_expr LBRACKET idx = expr RBRACKET EQUALS e = expr
    { IndexAssign (target, idx, e) }
  | e = postfix_expr r = REGEX_REPLACE
    { let (pat, repl, flags) = r in ExprStmt (RegexReplace (e, pat, repl, flags)) }
  | IF cond = expr body = block                  { If (cond, body, []) }
  | IF cond = expr body = block ELSE els = block { If (cond, body, els) }
  | IF cond = expr body = block ELSE elif = else_if { If (cond, body, [elif]) }
  | WHILE cond = expr body = block               { While (cond, body) }
  | FOR name = IDENT IN e = expr body = block    { For (name, e, body) }
  | FN name = IDENT LPAREN params = param_list RPAREN body = block
    { FnDef (name, params, body) }
  | RETURN e = expr                              { Return e }
  | e = expr                                     { ExprStmt e }
  ;

else_if:
  | IF cond = expr body = block                   { If (cond, body, []) }
  | IF cond = expr body = block ELSE els = block  { If (cond, body, els) }
  | IF cond = expr body = block ELSE elif = else_if { If (cond, body, [elif]) }
  ;

param_list:
  | /* empty */                            { [] }
  | p = IDENT                              { [p] }
  | p = IDENT COMMA rest = param_list      { p :: rest }
  ;

expr:
  | e = or_expr  { e }
  ;

or_expr:
  | e = and_expr                          { e }
  | l = or_expr OR r = and_expr           { BinOp (Or, l, r) }
  ;

and_expr:
  | e = cmp_expr                          { e }
  | l = and_expr AND r = cmp_expr         { BinOp (And, l, r) }
  ;

cmp_expr:
  | e = match_expr                        { e }
  | l = cmp_expr EQEQ r = match_expr     { BinOp (Eq, l, r) }
  | l = cmp_expr NEQ r = match_expr       { BinOp (Neq, l, r) }
  | l = cmp_expr LT r = match_expr        { BinOp (Lt, l, r) }
  | l = cmp_expr GT r = match_expr        { BinOp (Gt, l, r) }
  | l = cmp_expr LE r = match_expr        { BinOp (Le, l, r) }
  | l = cmp_expr GE r = match_expr        { BinOp (Ge, l, r) }
  ;

match_expr:
  | e = add_expr                            { e }
  | e = match_expr MATCH_OP r = REGEX
    { let (pat, flags) = r in RegexMatch (e, pat, flags) }
  | e = match_expr NOT_MATCH_OP r = REGEX
    { let (pat, flags) = r in UnaryOp (Not, RegexMatch (e, pat, flags)) }
  ;

add_expr:
  | e = concat_expr                       { e }
  | l = add_expr PLUS r = concat_expr     { BinOp (Add, l, r) }
  | l = add_expr MINUS r = concat_expr    { BinOp (Sub, l, r) }
  ;

concat_expr:
  | e = mul_expr                            { e }
  | l = concat_expr PLUSPLUS r = mul_expr   { BinOp (Concat, l, r) }
  ;

mul_expr:
  | e = unary_expr                        { e }
  | l = mul_expr STAR r = unary_expr      { BinOp (Mul, l, r) }
  | l = mul_expr SLASH r = unary_expr     { BinOp (Div, l, r) }
  | l = mul_expr PERCENT r = unary_expr   { BinOp (Mod, l, r) }
  ;

unary_expr:
  | e = postfix_expr                      { e }
  | MINUS e = unary_expr %prec UMINUS     { UnaryOp (Neg, e) }
  | NOT e = unary_expr %prec UNOT         { UnaryOp (Not, e) }
  ;

postfix_expr:
  | e = primary_expr                                  { e }
  | f = postfix_expr LPAREN args = arg_list RPAREN    { Call (f, args) }
  | e = postfix_expr LBRACKET idx = expr RBRACKET     { Index (e, idx) }
  ;

arg_list:
  | /* empty */                          { [] }
  | e = expr                             { [e] }
  | e = expr COMMA rest = arg_list       { e :: rest }
  ;

primary_expr:
  | i = INT                              { IntLit i }
  | f = FLOAT                            { FloatLit f }
  | s = STRING                           { StringLit s }
  | TRUE                                 { BoolLit true }
  | FALSE                                { BoolLit false }
  | NIL                                  { Nil }
  | name = IDENT                         { Var name }
  | LPAREN e = expr RPAREN               { e }
  | LBRACKET elems = array_elems RBRACKET { ArrayLit elems }
  | LBRACE pairs = hash_pairs RBRACE     { HashLit pairs }
  | FN LPAREN params = param_list RPAREN body = block
    { Lambda (params, body) }
  | r = REGEX                            { let (pat, flags) = r in RegexLit (pat, flags) }
  ;

array_elems:
  | /* empty */                                { [] }
  | e = expr                                   { [e] }
  | e = expr COMMA rest = array_elems          { e :: rest }
  ;

hash_pairs:
  | /* empty */                                          { [] }
  | k = expr COLON v = expr                              { [(k, v)] }
  | k = expr COLON v = expr COMMA rest = hash_pairs      { (k, v) :: rest }
  ;
