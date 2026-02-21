type compile_error =
  | Io_error of string
  | Parse_error of {
      line : int;
      column : int;
      char_pos : int;
      module_name : string;
      message : string;
    }
  | Unknown_target of string
  | Invalid_ast of {
      where : string;
      message : string;
    }
  | Macro_error of {
      where : string;
      message : string;
    }

let format_error = function
  | Io_error msg -> Printf.sprintf "I/O error: %s" msg
  | Parse_error { line; column; char_pos; module_name; message } ->
    let module_part =
      if module_name = "" then ""
      else Printf.sprintf "%s:" module_name
    in
    Printf.sprintf "%s%d:%d (char %d): %s" module_part line column char_pos message
  | Unknown_target t ->
    Printf.sprintf "Unknown target: %s (use perl, ocaml, or go)" t
  | Invalid_ast { where; message } ->
    Printf.sprintf "Invalid AST at %s: %s" where message
  | Macro_error { where; message } ->
    Printf.sprintf "Macro error at %s: %s" where message
