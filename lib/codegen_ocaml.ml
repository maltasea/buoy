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

let ocaml_escape_string s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | '\\' -> Buffer.add_string b "\\\\"
    | '"' -> Buffer.add_string b "\\\""
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let rec gen_expr = function
  | IntLit i -> Printf.sprintf "VInt %d" i
  | FloatLit f -> Printf.sprintf "VFloat %g" f
  | StringLit s -> Printf.sprintf "VString \"%s\"" (ocaml_escape_string s)
  | BoolLit true -> "VBool true"
  | BoolLit false -> "VBool false"
  | Nil -> "VNil"
  | Var name -> Printf.sprintf "!%s" name
  | ArrayLit elems ->
    let parts = List.map gen_expr elems in
    Printf.sprintf "VArray (ref [%s])" (String.concat "; " parts)
  | HashLit pairs ->
    let parts = List.map (fun (k, v) ->
      Printf.sprintf "(%s, %s)" (gen_expr k) (gen_expr v)
    ) pairs in
    Printf.sprintf "VHash (ref (List.to_seq [%s] |> Hashtbl.of_seq))" (String.concat "; " parts)
  | BinOp (op, l, r) -> gen_binop op l r
  | UnaryOp (Neg, e) -> Printf.sprintf "val_neg (%s)" (gen_expr e)
  | UnaryOp (Not, e) -> Printf.sprintf "val_not (%s)" (gen_expr e)
  | Call (func, args) -> gen_call func args
  | Index (e, idx) -> Printf.sprintf "val_index (%s) (%s)" (gen_expr e) (gen_expr idx)
  | Lambda (params, body) -> gen_lambda params body
  | RegexLit (pat, flags) -> Printf.sprintf "VRegex (\"%s\", \"%s\")" (ocaml_escape_string pat) (ocaml_escape_string flags)
  | RegexMatch (e, pat, flags) ->
    Printf.sprintf "val_regex_match (%s) \"%s\" \"%s\"" (gen_expr e) (ocaml_escape_string pat) (ocaml_escape_string flags)
  | RegexReplace (e, pat, repl, flags) ->
    Printf.sprintf "val_regex_replace_inplace %s \"%s\" \"%s\" \"%s\""
      (match e with Var name -> name | _ -> Printf.sprintf "(ref (%s))" (gen_expr e))
      (ocaml_escape_string pat) (ocaml_escape_string repl) (ocaml_escape_string flags)

and gen_binop op l r =
  let ls = gen_expr l and rs = gen_expr r in
  match op with
  | Add -> Printf.sprintf "val_add (%s) (%s)" ls rs
  | Sub -> Printf.sprintf "val_sub (%s) (%s)" ls rs
  | Mul -> Printf.sprintf "val_mul (%s) (%s)" ls rs
  | Div -> Printf.sprintf "val_div (%s) (%s)" ls rs
  | Mod -> Printf.sprintf "val_mod (%s) (%s)" ls rs
  | Eq -> Printf.sprintf "val_eq (%s) (%s)" ls rs
  | Neq -> Printf.sprintf "val_neq (%s) (%s)" ls rs
  | Lt -> Printf.sprintf "val_lt (%s) (%s)" ls rs
  | Gt -> Printf.sprintf "val_gt (%s) (%s)" ls rs
  | Le -> Printf.sprintf "val_le (%s) (%s)" ls rs
  | Ge -> Printf.sprintf "val_ge (%s) (%s)" ls rs
  | And -> Printf.sprintf "val_and (fun () -> %s) (fun () -> %s)" ls rs
  | Or -> Printf.sprintf "val_or (fun () -> %s) (fun () -> %s)" ls rs
  | Concat -> Printf.sprintf "val_concat (%s) (%s)" ls rs

and gen_call func args =
  let arg_strs = List.map gen_expr args in
  match func with
  | Var "println" ->
    Printf.sprintf "val_println [%s]" (String.concat "; " arg_strs)
  | Var "print" ->
    Printf.sprintf "val_print [%s]" (String.concat "; " arg_strs)
  | Var "length" ->
    Printf.sprintf "val_length (%s)" (List.hd arg_strs)
  | Var "push" ->
    let arr = List.hd arg_strs and rest = List.tl arg_strs in
    Printf.sprintf "val_push (%s) [%s]" arr (String.concat "; " rest)
  | Var "pop" ->
    Printf.sprintf "val_pop (%s)" (List.hd arg_strs)
  | Var "shift" ->
    Printf.sprintf "val_shift (%s)" (List.hd arg_strs)
  | Var "sort" ->
    Printf.sprintf "val_sort (%s)" (List.hd arg_strs)
  | Var "reverse" ->
    Printf.sprintf "val_reverse (%s)" (List.hd arg_strs)
  | Var "keys" ->
    Printf.sprintf "val_keys (%s)" (List.hd arg_strs)
  | Var "values" ->
    Printf.sprintf "val_values (%s)" (List.hd arg_strs)
  | Var "exists" -> begin
    match args with
    | [Index (e, key)] ->
      Printf.sprintf "val_exists (%s) (%s)" (gen_expr e) (gen_expr key)
    | _ -> "VBool false"
    end
  | Var "delete" -> begin
    match args with
    | [Index (e, key)] ->
      Printf.sprintf "val_delete (%s) (%s)" (gen_expr e) (gen_expr key)
    | _ -> "VNil"
    end
  | Var "map" ->
    let arr = List.nth arg_strs 0 and f = List.nth arg_strs 1 in
    Printf.sprintf "val_map (%s) (%s)" arr f
  | Var "filter" ->
    let arr = List.nth arg_strs 0 and f = List.nth arg_strs 1 in
    Printf.sprintf "val_filter (%s) (%s)" arr f
  | Var "each" ->
    let arr = List.nth arg_strs 0 and f = List.nth arg_strs 1 in
    Printf.sprintf "val_each (%s) (%s)" arr f
  | Var "join" ->
    Printf.sprintf "val_join (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1)
  | Var "split" ->
    Printf.sprintf "val_split (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1)
  | Var "substr" -> begin
    match arg_strs with
    | [s; start] -> Printf.sprintf "val_substr (%s) (%s) VNil" s start
    | [s; start; len] -> Printf.sprintf "val_substr (%s) (%s) (%s)" s start len
    | _ -> "VNil"
    end
  | Var "uppercase" -> Printf.sprintf "val_uppercase (%s)" (List.hd arg_strs)
  | Var "lowercase" -> Printf.sprintf "val_lowercase (%s)" (List.hd arg_strs)
  | Var "trim" -> Printf.sprintf "val_trim (%s)" (List.hd arg_strs)
  | Var "replace" ->
    Printf.sprintf "val_replace (%s) (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1) (List.nth arg_strs 2)
  | Var "unique" -> Printf.sprintf "val_unique (%s)" (List.hd arg_strs)
  | Var "sqrt" -> Printf.sprintf "val_sqrt (%s)" (List.hd arg_strs)
  | Var "sin" -> Printf.sprintf "val_sin (%s)" (List.hd arg_strs)
  | Var "cos" -> Printf.sprintf "val_cos (%s)" (List.hd arg_strs)
  | Var "abs" -> Printf.sprintf "val_abs (%s)" (List.hd arg_strs)
  | Var "log" -> Printf.sprintf "val_log (%s)" (List.hd arg_strs)
  | Var "floor" -> Printf.sprintf "val_floor (%s)" (List.hd arg_strs)
  | Var "ceil" -> Printf.sprintf "val_ceil (%s)" (List.hd arg_strs)
  | Var "random" -> begin
    match arg_strs with
    | [] -> "val_random VNil"
    | [e] -> Printf.sprintf "val_random (%s)" e
    | _ -> "val_random VNil"
    end
  | Var "async" -> begin
    match arg_strs with
    | [e] -> Printf.sprintf "val_async (fun () -> %s)" e
    | _ -> "VNil"
    end
  | Var "await" -> begin
    match arg_strs with
    | [e] -> Printf.sprintf "val_await (%s)" e
    | _ -> "VNil"
    end
  | Var "int_of" -> Printf.sprintf "val_int_of (%s)" (List.hd arg_strs)
  | Var "float_of" -> Printf.sprintf "val_float_of (%s)" (List.hd arg_strs)
  | Var "string_of" -> Printf.sprintf "val_string_of (%s)" (List.hd arg_strs)
  | Var "open" -> begin
    match arg_strs with
    | [e] -> Printf.sprintf "val_open (%s) (VString \"<\")" e
    | [e; mode] -> Printf.sprintf "val_open (%s) (%s)" e mode
    | _ -> "VNil"
    end
  | Var "close" -> Printf.sprintf "val_close (%s)" (List.hd arg_strs)
  | Var "readline" -> Printf.sprintf "val_readline (%s)" (List.hd arg_strs)
  | Var "read_file" -> Printf.sprintf "val_read_file (%s)" (List.hd arg_strs)
  | Var "writeln" ->
    Printf.sprintf "val_writeln (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1)
  | Var "regex_match" ->
    Printf.sprintf "val_regex_match_fn (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1)
  | Var "regex_replace" ->
    Printf.sprintf "val_regex_replace_fn (%s) (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1) (List.nth arg_strs 2)
  | Var "regex_find_all" ->
    Printf.sprintf "val_regex_find_all_fn (%s) (%s)" (List.nth arg_strs 0) (List.nth arg_strs 1)
  | Var name ->
    Printf.sprintf "val_call !%s [%s]" name (String.concat "; " arg_strs)
  | func ->
    Printf.sprintf "val_call (%s) [%s]" (gen_expr func) (String.concat "; " arg_strs)

and gen_lambda params body =
  let param_binds = List.mapi (fun i p ->
    Printf.sprintf "let %s = ref (List.nth _args %d) in" p i
  ) params in
  let body_str = gen_stmts_returning body in
  Printf.sprintf "VFunc (fun _args -> %s\n%s)" (String.concat " " param_binds) body_str

(* Generate a block of statements where the last ExprStmt returns its value *)
and gen_stmts_returning stmts =
  match List.rev stmts with
  | [] -> gen_stmts_to_string [ExprStmt Nil]
  | ExprStmt e :: rest ->
    let init = List.rev rest in
    let init_str = gen_stmts_to_string init in
    let saved = Buffer.contents buf in
    Buffer.clear buf;
    let saved_indent = !indent_level in
    indent_level := saved_indent + 1;
    emitln (Printf.sprintf "(%s)" (gen_expr e));
    let last_str = Buffer.contents buf in
    Buffer.clear buf;
    Buffer.add_string buf saved;
    indent_level := saved_indent;
    init_str ^ last_str
  | Return e :: rest ->
    let init = List.rev rest in
    let init_str = gen_stmts_to_string init in
    let saved = Buffer.contents buf in
    Buffer.clear buf;
    let saved_indent = !indent_level in
    indent_level := saved_indent + 1;
    emitln (Printf.sprintf "(%s)" (gen_expr e));
    let last_str = Buffer.contents buf in
    Buffer.clear buf;
    Buffer.add_string buf saved;
    indent_level := saved_indent;
    init_str ^ last_str
  | _ ->
    let s = gen_stmts_to_string stmts in
    s ^ "  VNil\n"

and gen_stmt = function
  | Let (name, e) ->
    emitln (Printf.sprintf "let %s = ref (%s) in" name (gen_expr e))
  | Assign (name, e) ->
    emitln (Printf.sprintf "%s := %s;" name (gen_expr e))
  | IndexAssign (e, idx, v) ->
    emitln (Printf.sprintf "val_index_assign (%s) (%s) (%s);" (gen_expr e) (gen_expr idx) (gen_expr v))
  | If (cond, then_body, []) ->
    emitln (Printf.sprintf "if val_truthy (%s) then begin" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt then_body);
    emitln "end;"
  | If (cond, then_body, else_body) ->
    emitln (Printf.sprintf "if val_truthy (%s) then begin" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt then_body);
    emitln "end else begin";
    indent (fun () -> List.iter gen_stmt else_body);
    emitln "end;"
  | While (cond, body) ->
    emitln (Printf.sprintf "while val_truthy (%s) do" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt body);
    emitln "done;"
  | For (name, e, body) ->
    emitln (Printf.sprintf "val_iter (%s) (fun _item ->" (gen_expr e));
    indent (fun () ->
      emitln (Printf.sprintf "let %s = ref _item in" name);
      List.iter gen_stmt body
    );
    emitln ");"
  | FnDef (name, params, body) ->
    let param_binds = List.mapi (fun i p ->
      Printf.sprintf "let %s = ref (List.nth _args %d) in" p i
    ) params in
    let body_str = gen_stmts_returning body in
    emitln (Printf.sprintf "let %s = ref (VFunc (fun _args -> %s" name (String.concat " " param_binds));
    emit body_str;
    emitln ")) in"
  | Return e ->
    emitln (Printf.sprintf "(%s)" (gen_expr e))
  | ExprStmt (RegexReplace (Var name, pat, repl, flags)) ->
    emitln (Printf.sprintf "val_regex_replace_inplace %s \"%s\" \"%s\" \"%s\";"
      name (ocaml_escape_string pat) (ocaml_escape_string repl) (ocaml_escape_string flags))
  | ExprStmt e ->
    emitln (Printf.sprintf "ignore (%s);" (gen_expr e))

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

let runtime = {|
(* Buoy runtime for OCaml target *)

type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VNil
  | VArray of value list ref
  | VHash of (value, value) Hashtbl.t ref
  | VFunc of (value list -> value)
  | VChannel of in_channel * out_channel option
  | VRegex of string * string
  | VFuture of (unit -> value) * value option ref

let val_truthy = function
  | VBool false | VNil | VInt 0 -> false
  | VString "" -> false
  | _ -> true

let to_string = function
  | VInt i -> string_of_int i
  | VFloat f -> Printf.sprintf "%g" f
  | VString s -> s
  | VBool true -> "1"
  | VBool false -> ""
  | VNil -> ""
  | VArray _ -> "(array)"
  | VHash _ -> "(hash)"
  | VFunc _ -> "(function)"
  | VChannel _ -> "(channel)"
  | VRegex (p, f) -> "/" ^ p ^ "/" ^ f
  | VFuture _ -> "(future)"

let to_int = function
  | VInt i -> i
  | VFloat f -> int_of_float f
  | VString s -> (try int_of_string (String.trim s) with _ -> 0)
  | VBool true -> 1
  | VBool false -> 0
  | _ -> 0

let to_float = function
  | VInt i -> float_of_int i
  | VFloat f -> f
  | VString s -> (try float_of_string (String.trim s) with _ -> 0.0)
  | VBool true -> 1.0
  | VBool false -> 0.0
  | _ -> 0.0

(* Arithmetic *)
let val_add a b = match a, b with
  | VInt x, VInt y -> VInt (x + y)
  | VFloat x, VFloat y -> VFloat (x +. y)
  | VInt x, VFloat y -> VFloat (float_of_int x +. y)
  | VFloat x, VInt y -> VFloat (x +. float_of_int y)
  | _ -> VFloat (to_float a +. to_float b)

let val_sub a b = match a, b with
  | VInt x, VInt y -> VInt (x - y)
  | _ -> VFloat (to_float a -. to_float b)

let val_mul a b = match a, b with
  | VInt x, VInt y -> VInt (x * y)
  | _ -> VFloat (to_float a *. to_float b)

let val_div a b = match a, b with
  | VInt x, VInt y when y <> 0 -> VInt (x / y)
  | _ -> VFloat (to_float a /. to_float b)

let val_mod a b = match a, b with
  | VInt x, VInt y when y <> 0 -> VInt (x mod y)
  | _ -> VInt (to_int a mod to_int b)

let val_neg = function
  | VInt i -> VInt (-i)
  | VFloat f -> VFloat (-.f)
  | v -> VInt (-(to_int v))

let val_not v = VBool (not (val_truthy v))

(* Comparison *)
let val_eq a b = match a, b with
  | VInt x, VInt y -> VBool (x = y)
  | VFloat x, VFloat y -> VBool (x = y)
  | VString x, VString y -> VBool (x = y)
  | VBool x, VBool y -> VBool (x = y)
  | VNil, VNil -> VBool true
  | _ -> VBool (to_string a = to_string b)

let val_neq a b = match val_eq a b with VBool b -> VBool (not b) | _ -> VBool true

let val_lt a b = match a, b with
  | VInt x, VInt y -> VBool (x < y)
  | _ -> VBool (to_float a < to_float b)

let val_gt a b = match a, b with
  | VInt x, VInt y -> VBool (x > y)
  | _ -> VBool (to_float a > to_float b)

let val_le a b = match a, b with
  | VInt x, VInt y -> VBool (x <= y)
  | _ -> VBool (to_float a <= to_float b)

let val_ge a b = match a, b with
  | VInt x, VInt y -> VBool (x >= y)
  | _ -> VBool (to_float a >= to_float b)

let val_and a b = if val_truthy (a ()) then b () else a ()
let val_or a b = let v = a () in if val_truthy v then v else b ()

(* String *)
let val_concat a b = VString (to_string a ^ to_string b)

(* I/O *)
let val_println args =
  List.iter (fun v -> print_string (to_string v)) args;
  print_newline ();
  VNil

let val_print args =
  List.iter (fun v -> print_string (to_string v)) args;
  VNil

(* Array/Hash indexing *)
let val_index arr idx = match arr, idx with
  | VArray r, VInt i ->
    let lst = !r in
    if i >= 0 && i < List.length lst then List.nth lst i else VNil
  | VHash r, key ->
    (match Hashtbl.find_opt !r key with Some v -> v | None -> VNil)
  | VString s, VInt i ->
    if i >= 0 && i < String.length s then VString (String.make 1 s.[i]) else VNil
  | _ -> VNil

let val_index_assign arr idx v = match arr with
  | VArray r ->
    let i = to_int idx in
    let lst = !r in
    let len = List.length lst in
    if i >= 0 && i < len then
      r := List.mapi (fun j x -> if j = i then v else x) lst
    else if i >= len then
      r := lst @ (List.init (i - len) (fun _ -> VNil)) @ [v]
  | VHash r -> Hashtbl.replace !r idx v
  | _ -> ()

(* Length *)
let val_length = function
  | VArray r -> VInt (List.length !r)
  | VHash r -> VInt (Hashtbl.length !r)
  | VString s -> VInt (String.length s)
  | _ -> VInt 0

(* Array operations *)
let val_push arr vals = match arr with
  | VArray r -> r := !r @ vals; VNil
  | _ -> VNil

let val_pop = function
  | VArray r ->
    let lst = !r in
    if lst = [] then VNil
    else begin
      let rev = List.rev lst in
      r := List.rev (List.tl rev);
      List.hd rev
    end
  | _ -> VNil

let val_shift = function
  | VArray r ->
    (match !r with
     | [] -> VNil
     | x :: rest -> r := rest; x)
  | _ -> VNil

let val_sort = function
  | VArray r ->
    let sorted = List.sort (fun a b ->
      match a, b with
      | VInt x, VInt y -> compare x y
      | VString x, VString y -> compare x y
      | _ -> compare (to_string a) (to_string b)
    ) !r in
    VArray (ref sorted)
  | VString s ->
    let chars = List.init (String.length s) (String.get s) in
    let sorted = List.sort compare chars in
    VString (String.init (List.length sorted) (List.nth sorted))
  | v -> v

let val_reverse = function
  | VArray r -> VArray (ref (List.rev !r))
  | VString s ->
    let len = String.length s in
    VString (String.init len (fun i -> s.[len - 1 - i]))
  | v -> v

let val_unique = function
  | VArray r ->
    let seen = Hashtbl.create 16 in
    let result = List.filter (fun v ->
      let k = to_string v in
      if Hashtbl.mem seen k then false
      else (Hashtbl.add seen k true; true)
    ) !r in
    VArray (ref result)
  | v -> v

(* Hash operations *)
let val_keys = function
  | VHash r ->
    VArray (ref (Hashtbl.fold (fun k _ acc -> k :: acc) !r []))
  | _ -> VArray (ref [])

let val_values = function
  | VHash r ->
    VArray (ref (Hashtbl.fold (fun _ v acc -> v :: acc) !r []))
  | _ -> VArray (ref [])

let val_exists hash key = match hash with
  | VHash r -> VBool (Hashtbl.mem !r key)
  | _ -> VBool false

let val_delete hash key = match hash with
  | VHash r ->
    let v = Hashtbl.find_opt !r key in
    Hashtbl.remove !r key;
    (match v with Some v -> v | None -> VNil)
  | _ -> VNil

(* Higher-order *)
let val_call f args = match f with
  | VFunc fn -> fn args
  | _ -> failwith "Cannot call non-function"

let val_async thunk =
  VFuture (thunk, ref None)

let val_await = function
  | VFuture (thunk, cache) ->
    (match !cache with
     | Some v -> v
     | None ->
       let v = thunk () in
       cache := Some v;
       v)
  | v -> v

let val_map arr f = match arr with
  | VArray r ->
    VArray (ref (List.map (fun v -> val_call f [v]) !r))
  | _ -> VNil

let val_filter arr f = match arr with
  | VArray r ->
    VArray (ref (List.filter (fun v -> val_truthy (val_call f [v])) !r))
  | _ -> VNil

let val_each arr f = match arr with
  | VArray r ->
    List.iter (fun v -> ignore (val_call f [v])) !r; VNil
  | _ -> VNil

let val_iter v f = match v with
  | VArray r -> List.iter f !r
  | _ -> ()

(* String ops *)
let val_join sep arr = match sep, arr with
  | VString s, VArray r ->
    VString (String.concat s (List.map to_string !r))
  | _ -> VNil

let val_split pat str =
  let p = to_string pat and s = to_string str in
  let re = Str.regexp p in
  VArray (ref (List.map (fun s -> VString s) (Str.split re s)))

let val_substr str start len =
  let s = to_string str and i = to_int start in
  let slen = String.length s in
  let i = if i < 0 then max 0 (slen + i) else i in
  if i >= slen then VString ""
  else match len with
    | VNil -> VString (String.sub s i (slen - i))
    | _ ->
      let l = min (to_int len) (slen - i) in
      VString (String.sub s i (max 0 l))

let val_uppercase v = VString (String.uppercase_ascii (to_string v))
let val_lowercase v = VString (String.lowercase_ascii (to_string v))

let val_trim v =
  VString (String.trim (to_string v))

let val_replace str pat repl =
  let s = to_string str and p = to_string pat and r = to_string repl in
  let re = Str.regexp_string p in
  VString (Str.global_replace re r s)

(* Math *)
let val_sqrt v = VFloat (sqrt (to_float v))
let val_sin v = VFloat (sin (to_float v))
let val_cos v = VFloat (cos (to_float v))
let val_abs = function
  | VInt i -> VInt (abs i)
  | v -> VFloat (abs_float (to_float v))
let val_log v = VFloat (log (to_float v))
let val_floor v = VInt (int_of_float (floor (to_float v)))
let val_ceil v = VInt (int_of_float (ceil (to_float v)))
let val_random = function
  | VNil -> VFloat (Random.float 1.0)
  | v -> VFloat (Random.float (to_float v))

let val_int_of v = VInt (to_int v)
let val_float_of v = VFloat (to_float v)
let val_string_of v = VString (to_string v)

(* I/O *)
let val_open filename mode =
  let f = to_string filename and m = to_string mode in
  if m = ">" || m = "w" then
    let oc = open_out f in
    VChannel (stdin, Some oc)
  else if m = ">>" || m = "a" then
    let oc = open_out_gen [Open_append; Open_creat; Open_wronly] 0o644 f in
    VChannel (stdin, Some oc)
  else
    let ic = open_in f in
    VChannel (ic, None)

let val_close = function
  | VChannel (ic, Some oc) ->
    (try close_in ic with _ -> ());
    (try close_out oc with _ -> ());
    VNil
  | VChannel (ic, None) ->
    (try close_in ic with _ -> ());
    VNil
  | _ -> VNil

let val_readline = function
  | VChannel (ic, _) ->
    (try VString (input_line ic) with End_of_file -> VNil)
  | _ -> VNil

let val_read_file filename =
  let f = to_string filename in
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  VString (Bytes.to_string s)

let val_writeln fh data = match fh with
  | VChannel (_, Some oc) ->
    output_string oc (to_string data);
    output_char oc '\n';
    VNil
  | _ -> VNil

(* Regex operations using Str *)
let make_regex pat flags =
  let _ = flags in (* Str doesn't support flags well, but we handle 'i' *)
  if String.contains flags 'i' then
    Str.regexp_case_fold pat
  else
    Str.regexp pat

let val_regex_match v pat flags =
  let s = to_string v in
  let re = make_regex pat flags in
  VBool (try ignore (Str.search_forward re s 0); true with Not_found -> false)

let val_regex_replace_inplace r pat repl flags =
  let s = to_string !r in
  let re = make_regex pat flags in
  let result = if String.contains flags 'g'
    then Str.global_replace re repl s
    else Str.replace_first re repl s in
  r := VString result

let val_regex_match_fn str pat =
  let s = to_string str in
  let p = to_string pat in
  let re = Str.regexp p in
  VBool (try ignore (Str.search_forward re s 0); true with Not_found -> false)

let val_regex_replace_fn str pat repl =
  let s = to_string str and p = to_string pat and r = to_string repl in
  let re = Str.regexp p in
  VString (Str.global_replace re r s)

let val_regex_find_all_fn str pat =
  let s = to_string str and p = to_string pat in
  let re = Str.regexp p in
  let results = ref [] in
  let pos = ref 0 in
  (try while true do
    let _ = Str.search_forward re s !pos in
    let m = Str.matched_string s in
    results := VString m :: !results;
    pos := Str.match_end ();
    if !pos >= String.length s then raise Not_found
  done with Not_found -> ());
  VArray (ref (List.rev !results))

let () = Random.self_init ()

(* Main program *)
let () =
|}

let generate (program : Ast.program) : string =
  Buffer.clear buf;
  indent_level := 0;
  emit runtime;
  List.iter gen_stmt program;
  emitln "()";
  Buffer.contents buf
