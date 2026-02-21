type target =
  | Perl
  | Ocaml
  | Go

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

let target_of_string = function
  | "perl" -> Ok Perl
  | "ocaml" -> Ok Ocaml
  | "go" -> Ok Go
  | t -> Error (Errors.Unknown_target t)

let parse_legacy ?(module_name = "") (source : string) :
  (Ast.program, Errors.compile_error) result =
  let source = Reader.rewrite_source source in
  let source =
    if String.length source > 0 && source.[String.length source - 1] <> '\n'
    then source ^ "\n"
    else source
  in
  let lexbuf = Lexing.from_string source in
  Lexer.prev_was_expr_end := false;
  try
    Ok (Parser.program Lexer.token lexbuf)
  with
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Error (Errors.Parse_error {
      line = pos.pos_lnum;
      column = pos.pos_cnum - pos.pos_bol;
      char_pos = pos.pos_cnum;
      module_name;
      message = "Parse error";
    })
  | Lexer.Lexer_error msg ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Error (Errors.Parse_error {
      line = pos.pos_lnum;
      column = pos.pos_cnum - pos.pos_bol;
      char_pos = pos.pos_cnum;
      module_name;
      message = msg;
    })

let parse (source : string) : Ast.program =
  match parse_legacy source with
  | Ok ast -> ast
  | Error e -> failwith (Errors.format_error e)

let read_ast ?(module_name = "") (source : string) :
  (Tuple_ast.program, Errors.compile_error) result =
  let* legacy = parse_legacy ~module_name source in
  let tuple_program = Tuple_ast.of_program ~module_name legacy in
  Ok (Normalize.normalize_program tuple_program)

let compile_ast (target : target) (program : Tuple_ast.program) :
  (string, Errors.compile_error) result =
  let* expanded = Macro_expand.expand_program program in
  let* legacy = Tuple_ast.program_of_node expanded in
  let code = match target with
    | Perl -> Codegen_perl.generate legacy
    | Ocaml -> Codegen_ocaml.generate legacy
    | Go -> Codegen_go.generate legacy
  in
  Ok code

let compile_source ?(module_name = "") (target : target) (source : string) :
  (string, Errors.compile_error) result =
  let* tuple_program = read_ast ~module_name source in
  compile_ast target tuple_program

let compile_source_target ?(module_name = "") (target : string) (source : string) :
  (string, Errors.compile_error) result =
  let* parsed_target = target_of_string target in
  compile_source ~module_name parsed_target source

let compile_file (target : target) (filename : string) :
  (string, Errors.compile_error) result =
  try
    let source = In_channel.with_open_text filename In_channel.input_all in
    compile_source ~module_name:filename target source
  with
  | Sys_error msg -> Error (Errors.Io_error msg)

let compile_file_target (target : string) (filename : string) :
  (string, Errors.compile_error) result =
  let* parsed_target = target_of_string target in
  compile_file parsed_target filename
