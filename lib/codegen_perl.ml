open Ast

let buf = Buffer.create 4096
let indent_level = ref 0

let emit s = Buffer.add_string buf s
let emitln s =
  Buffer.add_string buf (String.make (!indent_level * 2) ' ');
  Buffer.add_string buf s;
  Buffer.add_char buf '\n'

let indent f =
  incr indent_level;
  f ();
  decr indent_level

(* Track declared variables for $ sigil *)
let declared = Hashtbl.create 64

(* Check if a call returns an array *)
let call_returns_array = function
  | Call (Var name, _) ->
    List.mem name ["split"; "sort"; "reverse"; "map"; "filter"; "unique";
                    "keys"; "values"; "regex_find_all"]
  | _ -> false

let perl_escape_string s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | '\\' -> Buffer.add_string b "\\\\"
    | '"' -> Buffer.add_string b "\\\""
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | '$' -> Buffer.add_string b "\\$"
    | '@' -> Buffer.add_string b "\\@"
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let rec gen_expr = function
  | IntLit i -> string_of_int i
  | FloatLit f -> Printf.sprintf "%g" f
  | StringLit s -> Printf.sprintf "\"%s\"" (perl_escape_string s)
  | BoolLit true -> "1"
  | BoolLit false -> "0"
  | Nil -> "undef"
  | Var name -> "$" ^ name
  | ArrayLit elems ->
    let parts = List.map gen_expr elems in
    "(" ^ String.concat ", " parts ^ ")"
  | HashLit pairs ->
    let parts = List.map (fun (k, v) ->
      gen_expr k ^ " => " ^ gen_expr v
    ) pairs in
    "(" ^ String.concat ", " parts ^ ")"
  | BinOp (op, l, r) -> gen_binop op l r
  | UnaryOp (Neg, e) -> "(-" ^ gen_expr e ^ ")"
  | UnaryOp (Not, e) -> "(!" ^ gen_expr e ^ ")"
  | Call (func, args) -> gen_call func args
  | Index (e, idx) -> gen_index e idx
  | Lambda (params, body) ->
    let param_decls = String.concat "; " (List.map (fun p -> "my $" ^ p ^ " = shift") params) in
    let body_str = gen_stmts_to_string body in
    Printf.sprintf "sub { %s;\n%s}" param_decls body_str
  | RegexLit (pat, flags) -> Printf.sprintf "qr/%s/%s" pat flags
  | RegexMatch (e, pat, flags) ->
    Printf.sprintf "(%s =~ /%s/%s)" (gen_expr e) pat flags
  | RegexReplace (e, pat, repl, flags) ->
    Printf.sprintf "(%s =~ s/%s/%s/%s)" (gen_expr e) pat repl flags

and gen_binop op l r =
  let ls = gen_expr l and rs = gen_expr r in
  let ops = match op with
    | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
    | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
    | And -> "&&" | Or -> "||"
    | Concat -> "."
  in
  Printf.sprintf "(%s %s %s)" ls ops rs

and gen_call func args =
  match func with
  | Var "println" ->
    let arg_strs = List.map gen_expr args in
    Printf.sprintf "print(%s, \"\\n\")" (String.concat ", " arg_strs)
  | Var "print" ->
    let arg_strs = List.map gen_expr args in
    Printf.sprintf "print(%s)" (String.concat ", " arg_strs)
  | Var "length" -> begin
    match args with
    | [Var name] ->
      if Hashtbl.mem declared (name ^ "_is_array") then
        Printf.sprintf "scalar(@%s)" name
      else if Hashtbl.mem declared (name ^ "_is_hash") then
        Printf.sprintf "scalar(keys %%%s)" name
      else
        Printf.sprintf "length(%s)" (gen_expr (Var name))
    | [e] when call_returns_array e ->
      Printf.sprintf "scalar(%s)" (gen_expr e)
    | [e] -> Printf.sprintf "length(%s)" (gen_expr e)
    | _ -> "length()"
    end
  | Var "push" -> begin
    match args with
    | Var name :: rest ->
      let arg_strs = List.map gen_expr rest in
      Printf.sprintf "push(@%s, %s)" name (String.concat ", " arg_strs)
    | _ -> "push()"
    end
  | Var "pop" -> begin
    match args with
    | [Var name] -> Printf.sprintf "pop(@%s)" name
    | _ -> "pop()"
    end
  | Var "shift" -> begin
    match args with
    | [Var name] -> Printf.sprintf "shift(@%s)" name
    | _ -> "shift()"
    end
  | Var "sort" -> begin
    match args with
    | [Var name] -> Printf.sprintf "sort(@%s)" name
    | _ -> "sort()"
    end
  | Var "reverse" -> begin
    match args with
    | [Var name] ->
      if Hashtbl.mem declared (name ^ "_is_array") then
        Printf.sprintf "reverse(@%s)" name
      else
        Printf.sprintf "scalar(reverse(%s))" (gen_expr (Var name))
    | [e] -> Printf.sprintf "scalar(reverse(%s))" (gen_expr e)
    | _ -> "reverse()"
    end
  | Var "keys" -> begin
    match args with
    | [Var name] -> Printf.sprintf "keys(%%%s)" name
    | _ -> "keys()"
    end
  | Var "values" -> begin
    match args with
    | [Var name] -> Printf.sprintf "values(%%%s)" name
    | _ -> "values()"
    end
  | Var "exists" -> begin
    match args with
    | [Index (Var name, key)] ->
      Printf.sprintf "exists($%s{%s})" name (gen_expr key)
    | _ -> "exists()"
    end
  | Var "delete" -> begin
    match args with
    | [Index (Var name, key)] ->
      Printf.sprintf "delete($%s{%s})" name (gen_expr key)
    | _ -> "delete()"
    end
  | Var "map" -> begin
    match args with
    | [Var name; func] ->
      Printf.sprintf "map { %s->($_) } @%s" (gen_expr func) name
    | [arr; func] ->
      Printf.sprintf "map { %s->($_) } @{%s}" (gen_expr func) (gen_expr arr)
    | _ -> "map()"
    end
  | Var "filter" -> begin
    match args with
    | [Var name; func] ->
      Printf.sprintf "grep { %s->($_) } @%s" (gen_expr func) name
    | _ -> "grep()"
    end
  | Var "each" -> begin
    match args with
    | [Var name; func] ->
      Printf.sprintf "do { %s->($_) for @%s }" (gen_expr func) name
    | _ -> "each()"
    end
  | Var "join" -> begin
    match args with
    | [sep; Var name] ->
      Printf.sprintf "join(%s, @%s)" (gen_expr sep) name
    | [sep; arr] ->
      Printf.sprintf "join(%s, @{%s})" (gen_expr sep) (gen_expr arr)
    | _ -> "join()"
    end
  | Var "split" -> begin
    match args with
    | [pat; str] ->
      Printf.sprintf "split(%s, %s)" (gen_expr pat) (gen_expr str)
    | _ -> "split()"
    end
  | Var "substr" -> begin
    match args with
    | [str; start] ->
      Printf.sprintf "substr(%s, %s)" (gen_expr str) (gen_expr start)
    | [str; start; len] ->
      Printf.sprintf "substr(%s, %s, %s)" (gen_expr str) (gen_expr start) (gen_expr len)
    | _ -> "substr()"
    end
  | Var "uppercase" -> begin
    match args with
    | [e] -> Printf.sprintf "uc(%s)" (gen_expr e)
    | _ -> "uc()"
    end
  | Var "lowercase" -> begin
    match args with
    | [e] -> Printf.sprintf "lc(%s)" (gen_expr e)
    | _ -> "lc()"
    end
  | Var "trim" -> begin
    match args with
    | [e] ->
      let s = gen_expr e in
      Printf.sprintf "do { my $s = %s; $s =~ s/^\\s+|\\s+$//gr }" s
    | _ -> "trim()"
    end
  | Var "replace" -> begin
    match args with
    | [str; StringLit pat; StringLit repl] ->
      let ss = gen_expr str in
      Printf.sprintf "do { (my $s = %s) =~ s/\\Q%s\\E/%s/g; $s }" ss (perl_escape_string pat) (perl_escape_string repl)
    | [str; pat; repl] ->
      let ss = gen_expr str and ps = gen_expr pat and rs = gen_expr repl in
      Printf.sprintf "do { (my $s = %s) =~ s/\\Q${\\ %s}\\E/${\\ %s}/g; $s }" ss ps rs
    | _ -> "replace()"
    end
  | Var "unique" -> begin
    match args with
    | [Var name] ->
      Printf.sprintf "do { my %%seen; grep { !$seen{$_}++ } @%s }" name
    | _ -> "unique()"
    end
  | Var "sqrt" -> begin match args with [e] -> Printf.sprintf "sqrt(%s)" (gen_expr e) | _ -> "sqrt()" end
  | Var "sin" -> begin match args with [e] -> Printf.sprintf "sin(%s)" (gen_expr e) | _ -> "sin()" end
  | Var "cos" -> begin match args with [e] -> Printf.sprintf "cos(%s)" (gen_expr e) | _ -> "cos()" end
  | Var "abs" -> begin match args with [e] -> Printf.sprintf "abs(%s)" (gen_expr e) | _ -> "abs()" end
  | Var "log" -> begin match args with [e] -> Printf.sprintf "log(%s)" (gen_expr e) | _ -> "log()" end
  | Var "floor" -> begin match args with [e] -> Printf.sprintf "int(%s)" (gen_expr e) | _ -> "int()" end
  | Var "ceil" -> begin match args with [e] -> Printf.sprintf "POSIX::ceil(%s)" (gen_expr e) | _ -> "ceil()" end
  | Var "random" -> begin match args with [] -> "rand()" | [e] -> Printf.sprintf "rand(%s)" (gen_expr e) | _ -> "rand()" end
  | Var "async" -> begin
    match args with
    | [e] ->
      Printf.sprintf
        "do { my $__slb_future = { __thunk => sub { %s }, __done => 0, __value => undef }; $__slb_future }"
        (gen_expr e)
    | _ -> "undef"
    end
  | Var "await" -> begin
    match args with
    | [e] ->
      Printf.sprintf
        "do { my $__slb_f = %s; if (ref($__slb_f) eq 'HASH' && exists($__slb_f->{__thunk})) { if (!$__slb_f->{__done}) { $__slb_f->{__value} = $__slb_f->{__thunk}->(); $__slb_f->{__done} = 1; } $__slb_f->{__value} } else { $__slb_f } }"
        (gen_expr e)
    | _ -> "undef"
    end
  | Var "int_of" -> begin match args with [e] -> Printf.sprintf "int(%s)" (gen_expr e) | _ -> "int()" end
  | Var "float_of" -> begin match args with [e] -> Printf.sprintf "(0.0 + %s)" (gen_expr e) | _ -> "0.0" end
  | Var "string_of" -> begin match args with [e] -> Printf.sprintf "(\"\" . %s)" (gen_expr e) | _ -> "\"\"" end
  | Var "open" -> begin
    match args with
    | [e] -> Printf.sprintf "do { open(my $fh, '<', %s) or die $!; $fh }" (gen_expr e)
    | [e; mode] -> Printf.sprintf "do { open(my $fh, %s, %s) or die $!; $fh }" (gen_expr mode) (gen_expr e)
    | _ -> "open()"
    end
  | Var "close" -> begin match args with [e] -> Printf.sprintf "close(%s)" (gen_expr e) | _ -> "close()" end
  | Var "readline" -> begin match args with [e] -> Printf.sprintf "scalar(<%s>)" (gen_expr e) | _ -> "readline()" end
  | Var "read_file" -> begin
    match args with
    | [e] -> Printf.sprintf "do { local $/; open(my $fh, '<', %s) or die $!; my $c = <$fh>; close($fh); $c }" (gen_expr e)
    | _ -> "read_file()"
    end
  | Var "writeln" -> begin
    match args with
    | [fh; data] -> Printf.sprintf "print(%s %s, \"\\n\")" (gen_expr fh) (gen_expr data)
    | _ -> "writeln()"
    end
  | Var "regex_match" -> begin
    match args with
    | [str; pat] -> Printf.sprintf "(%s =~ %s)" (gen_expr str) (gen_expr pat)
    | _ -> "0"
    end
  | Var "regex_replace" -> begin
    match args with
    | [str; pat; repl] ->
      Printf.sprintf "do { my $s = %s; $s =~ s/%s/%s/g; $s }" (gen_expr str) (gen_expr_raw pat) (gen_expr_raw repl)
    | _ -> "\"\""
    end
  | Var "regex_find_all" -> begin
    match args with
    | [str; pat] ->
      Printf.sprintf "do { my @m; while (%s =~ /%s/g) { push @m, $& } @m }" (gen_expr str) (gen_expr_raw pat)
    | _ -> "()"
    end
  | Var name ->
    let arg_strs = List.map gen_expr args in
    Printf.sprintf "%s(%s)" name (String.concat ", " arg_strs)
  | func ->
    let arg_strs = List.map gen_expr args in
    Printf.sprintf "%s->(%s)" (gen_expr func) (String.concat ", " arg_strs)

(* Extract raw string from a StringLit for use in regex patterns *)
and gen_expr_raw = function
  | StringLit s -> s
  | e -> gen_expr e

and gen_index e idx =
  match e with
  | Var name ->
    if Hashtbl.mem declared (name ^ "_is_hash") then
      Printf.sprintf "$%s{%s}" name (gen_expr idx)
    else
      Printf.sprintf "$%s[%s]" name (gen_expr idx)
  | _ ->
    Printf.sprintf "%s->[%s]" (gen_expr e) (gen_expr idx)

and gen_stmt = function
  | Let (name, ArrayLit elems) ->
    Hashtbl.replace declared name true;
    Hashtbl.replace declared (name ^ "_is_array") true;
    let parts = List.map gen_expr elems in
    emitln (Printf.sprintf "my @%s = (%s);" name (String.concat ", " parts))
  | Let (name, HashLit pairs) ->
    Hashtbl.replace declared name true;
    Hashtbl.replace declared (name ^ "_is_hash") true;
    let parts = List.map (fun (k, v) ->
      gen_expr k ^ " => " ^ gen_expr v
    ) pairs in
    emitln (Printf.sprintf "my %%%s = (%s);" name (String.concat ", " parts))
  | Let (name, e) when call_returns_array e ->
    Hashtbl.replace declared name true;
    Hashtbl.replace declared (name ^ "_is_array") true;
    emitln (Printf.sprintf "my @%s = %s;" name (gen_expr e))
  | Let (name, e) ->
    Hashtbl.replace declared name true;
    emitln (Printf.sprintf "my $%s = %s;" name (gen_expr e))
  | Assign (name, ArrayLit elems) ->
    if not (Hashtbl.mem declared (name ^ "_is_array")) then
      Hashtbl.replace declared (name ^ "_is_array") true;
    let parts = List.map gen_expr elems in
    emitln (Printf.sprintf "@%s = (%s);" name (String.concat ", " parts))
  | Assign (name, e) ->
    emitln (Printf.sprintf "$%s = %s;" name (gen_expr e))
  | IndexAssign (Var name, idx, v) ->
    if Hashtbl.mem declared (name ^ "_is_hash") then
      emitln (Printf.sprintf "$%s{%s} = %s;" name (gen_expr idx) (gen_expr v))
    else
      emitln (Printf.sprintf "$%s[%s] = %s;" name (gen_expr idx) (gen_expr v))
  | IndexAssign (target, idx, v) ->
    emitln (Printf.sprintf "%s->[%s] = %s;" (gen_expr target) (gen_expr idx) (gen_expr v))
  | If (cond, then_body, else_body) ->
    emitln (Printf.sprintf "if (%s) {" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt then_body);
    if else_body <> [] then begin
      emitln "} else {";
      indent (fun () -> List.iter gen_stmt else_body)
    end;
    emitln "}"
  | While (cond, body) ->
    emitln (Printf.sprintf "while (%s) {" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt body);
    emitln "}"
  | For (name, Var arr_name, body) when Hashtbl.mem declared (arr_name ^ "_is_array") ->
    emitln (Printf.sprintf "for my $%s (@%s) {" name arr_name);
    indent (fun () -> List.iter gen_stmt body);
    emitln "}"
  | For (name, e, body) ->
    emitln (Printf.sprintf "for my $%s (@{[%s]}) {" name (gen_expr e));
    indent (fun () -> List.iter gen_stmt body);
    emitln "}"
  | FnDef (name, params, body) ->
    Hashtbl.replace declared name true;
    let param_decls = String.concat "; " (List.map (fun p -> "my $" ^ p ^ " = shift") params) in
    emitln (Printf.sprintf "sub %s { %s;" name param_decls);
    indent (fun () -> List.iter gen_stmt body);
    emitln "}"
  | Return e ->
    emitln (Printf.sprintf "return %s;" (gen_expr e))
  | ExprStmt (RegexReplace (Var name, pat, repl, flags)) ->
    emitln (Printf.sprintf "$%s =~ s/%s/%s/%s;" name pat repl flags)
  | ExprStmt e ->
    emitln (Printf.sprintf "%s;" (gen_expr e))

and gen_stmts_to_string stmts =
  let saved = Buffer.contents buf in
  Buffer.clear buf;
  let saved_indent = !indent_level in
  indent_level := saved_indent + 1;
  List.iter gen_stmt stmts;
  let result = Buffer.contents buf in
  Buffer.clear buf;
  Buffer.add_string buf saved;
  indent_level := saved_indent;
  result

let generate (program : Ast.program) : string =
  Buffer.clear buf;
  indent_level := 0;
  Hashtbl.clear declared;
  emitln "use strict;";
  emitln "use warnings;";
  emitln "";
  List.iter gen_stmt program;
  Buffer.contents buf
