{
  open Parser

  exception Lexer_error of string

  let keywords = Hashtbl.create 16
  let () = List.iter (fun (k, v) -> Hashtbl.add keywords k v) [
    "let", LET;
    "fn", FN;
    "if", IF;
    "else", ELSE;
    "while", WHILE;
    "for", FOR;
    "in", IN;
    "return", RETURN;
    "true", TRUE;
    "false", FALSE;
    "nil", NIL;
    "not", NOT;
    "and", AND;
    "or", OR;
  ]

  (* Track whether we just saw a token that could end an expression,
     to disambiguate '/' as division vs regex start *)
  let prev_was_expr_end = ref false

  let set_expr_end b tok =
    prev_was_expr_end := b;
    tok
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident_char = alpha | digit | ['-' '?']
let ws = [' ' '\t']

rule token = parse
  | ws+                { token lexbuf }
  | '#' [^ '\n']*     { token lexbuf }
  | '\n'               { Lexing.new_line lexbuf; set_expr_end false NEWLINE }
  | digit+ '.' digit+  { set_expr_end true (FLOAT (float_of_string (Lexing.lexeme lexbuf))) }
  | digit+             { set_expr_end true (INT (int_of_string (Lexing.lexeme lexbuf))) }
  | '"'                { set_expr_end true (STRING (read_string (Buffer.create 64) lexbuf)) }
  | "=~" ws* "s/"     { let pat = read_regex_body (Buffer.create 64) lexbuf in
                         let repl = read_regex_body (Buffer.create 64) lexbuf in
                         let flags = read_regex_flags (Buffer.create 8) lexbuf in
                         set_expr_end true (REGEX_REPLACE (pat, repl, flags)) }
  | "=~"               { set_expr_end false MATCH_OP }
  | "!~"               { set_expr_end false NOT_MATCH_OP }
  | "++"               { set_expr_end false PLUSPLUS }
  | "=="               { set_expr_end false EQEQ }
  | "!="               { set_expr_end false NEQ }
  | "<="               { set_expr_end false LE }
  | ">="               { set_expr_end false GE }
  | '+'                { set_expr_end false PLUS }
  | '-'                { set_expr_end false MINUS }
  | '*'                { set_expr_end false STAR }
  | '/'                { if !prev_was_expr_end then
                           set_expr_end false SLASH
                         else
                           let pat = read_regex_body (Buffer.create 64) lexbuf in
                           let flags = read_regex_flags (Buffer.create 8) lexbuf in
                           set_expr_end true (REGEX (pat, flags)) }
  | '%'                { set_expr_end false PERCENT }
  | '='                { set_expr_end false EQUALS }
  | '<'                { set_expr_end false LT }
  | '>'                { set_expr_end false GT }
  | '('                { set_expr_end false LPAREN }
  | ')'                { set_expr_end true RPAREN }
  | '['                { set_expr_end false LBRACKET }
  | ']'                { set_expr_end true RBRACKET }
  | '{'                { set_expr_end false LBRACE }
  | '}'                { set_expr_end true RBRACE }
  | ','                { set_expr_end false COMMA }
  | ':'                { set_expr_end false COLON }
  | alpha ident_char*  { let s = Lexing.lexeme lexbuf in
                         match Hashtbl.find_opt keywords s with
                         | Some tok -> set_expr_end (match tok with TRUE | FALSE | NIL -> true | _ -> false) tok
                         | None -> set_expr_end true (IDENT s) }
  | eof                { EOF }
  | _ as c             { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c)) }

and read_string buf = parse
  | '"'                { Buffer.contents buf }
  | "\\n"              { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t"              { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\\\"             { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | "\\\""             { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+      { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof                { raise (Lexer_error "Unterminated string") }
  | _                  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }

and read_regex_body buf = parse
  | "\\/"             { Buffer.add_string buf "\\/"; read_regex_body buf lexbuf }
  | '/'               { Buffer.contents buf }
  | [^ '/' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf); read_regex_body buf lexbuf }
  | '\\'              { Buffer.add_char buf '\\'; read_regex_body buf lexbuf }
  | eof               { raise (Lexer_error "Unterminated regex") }

and read_regex_flags buf = parse
  | ['g' 'i' 'm' 's' 'x']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); Buffer.contents buf }
  | ""                       { Buffer.contents buf }
