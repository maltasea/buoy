open Ast

type meta = {
  char_pos : int;
  line_nr : int;
  module_name : string;
}

type node = Node of string * meta * node list
type program = node

type decode_error = Errors.compile_error

let invalid where message = Error (Errors.Invalid_ast { where; message })
let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

let default_meta ?(module_name = "") () = {
  char_pos = 0;
  line_nr = 1;
  module_name;
}

let mk ?meta tag args =
  let m = match meta with
    | Some m -> m
    | None -> default_meta ()
  in
  Node (tag, m, args)

let tag_of (Node (tag, _, _)) = tag
let meta_of (Node (_, m, _)) = m
let args_of (Node (_, _, args)) = args

let payload ?meta tag value =
  let m = match meta with Some m -> m | None -> default_meta () in
  Node (tag, m, [Node (value, m, [])])

let sym ?meta name = payload ?meta "sym" name
let str ?meta s = payload ?meta "string" s
let int ?meta i = payload ?meta "int" (string_of_int i)
let float ?meta f = payload ?meta "float" (Printf.sprintf "%g" f)
let bool ?meta b = payload ?meta "bool" (if b then "true" else "false")
let nil ?meta () = mk ?meta "nil" []
let regex ?meta pat flags = mk ?meta "regex" [str ?meta pat; str ?meta flags]
let do_block ?meta stmts = mk ?meta "do" stmts

let binop_tag = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "and"
  | Or -> "or"
  | Concat -> "concat"

let unary_tag = function
  | Neg -> "neg"
  | Not -> "not"

let binop_of_tag = function
  | "+" -> Some Add
  | "-" -> Some Sub
  | "*" -> Some Mul
  | "/" -> Some Div
  | "%" -> Some Mod
  | "==" -> Some Eq
  | "!=" -> Some Neq
  | "<" -> Some Lt
  | ">" -> Some Gt
  | "<=" -> Some Le
  | ">=" -> Some Ge
  | "and" -> Some And
  | "or" -> Some Or
  | "concat" -> Some Concat
  | _ -> None

let unary_of_tag = function
  | "neg" -> Some Neg
  | "not" -> Some Not
  | _ -> None

let rec of_expr ?meta = function
  | IntLit i -> int ?meta i
  | FloatLit f -> float ?meta f
  | StringLit s -> str ?meta s
  | BoolLit b -> bool ?meta b
  | Nil -> nil ?meta ()
  | ArrayLit elems -> mk ?meta "array" (List.map (of_expr ?meta) elems)
  | HashLit pairs ->
    let pair_nodes = List.map (fun (k, v) -> mk ?meta "pair" [of_expr ?meta k; of_expr ?meta v]) pairs in
    mk ?meta "struct" pair_nodes
  | Var name -> sym ?meta name
  | BinOp (op, l, r) -> mk ?meta (binop_tag op) [of_expr ?meta l; of_expr ?meta r]
  | UnaryOp (op, e) -> mk ?meta (unary_tag op) [of_expr ?meta e]
  | Call (Var name, args) -> mk ?meta name (List.map (of_expr ?meta) args)
  | Call (f, args) -> mk ?meta "call" (of_expr ?meta f :: List.map (of_expr ?meta) args)
  | Index (e, idx) -> mk ?meta "index" [of_expr ?meta e; of_expr ?meta idx]
  | Lambda (params, body) ->
    mk ?meta "fn" [
      mk ?meta "params" (List.map (sym ?meta) params);
      do_block ?meta (List.map (of_stmt ?meta) body);
    ]
  | RegexLit (pat, flags) -> regex ?meta pat flags
  | RegexMatch (e, pat, flags) ->
    mk ?meta "regex_match" [of_expr ?meta e; str ?meta pat; str ?meta flags]
  | RegexReplace (e, pat, repl, flags) ->
    mk ?meta "regex_replace" [of_expr ?meta e; str ?meta pat; str ?meta repl; str ?meta flags]

and of_stmt ?meta = function
  | Let (name, e) -> mk ?meta "let" [sym ?meta name; of_expr ?meta e]
  | Assign (name, e) -> mk ?meta "set" [sym ?meta name; of_expr ?meta e]
  | IndexAssign (target, idx, value) ->
    mk ?meta "set-index" [of_expr ?meta target; of_expr ?meta idx; of_expr ?meta value]
  | If (cond, then_body, else_body) ->
    mk ?meta "if" [
      of_expr ?meta cond;
      do_block ?meta (List.map (of_stmt ?meta) then_body);
      do_block ?meta (List.map (of_stmt ?meta) else_body);
    ]
  | While (cond, body) ->
    mk ?meta "while" [of_expr ?meta cond; do_block ?meta (List.map (of_stmt ?meta) body)]
  | For (name, iter, body) ->
    mk ?meta "for" [sym ?meta name; of_expr ?meta iter; do_block ?meta (List.map (of_stmt ?meta) body)]
  | FnDef (name, params, body) ->
    mk ?meta "defn" [
      sym ?meta name;
      mk ?meta "params" (List.map (sym ?meta) params);
      do_block ?meta (List.map (of_stmt ?meta) body);
    ]
  | Return e -> mk ?meta "return" [of_expr ?meta e]
  | ExprStmt e -> of_expr ?meta e

let of_program ?(module_name = "") (p : Ast.program) : program =
  let meta = default_meta ~module_name () in
  mk ~meta "program" (List.map (of_stmt ~meta) p)

let payload_string where = function
  | Node (_, _, [Node (v, _, [])]) -> Ok v
  | _ -> invalid where "Expected payload node"

let expect_tag where expected = function
  | Node (tag, _, args) when tag = expected -> Ok args
  | Node (tag, _, _) -> invalid where (Printf.sprintf "Expected tag '%s' but got '%s'" expected tag)

let sym_name where n =
  match n with
  | Node ("sym", _, _) -> payload_string where n
  | Node (tag, _, []) -> Ok tag
  | Node (tag, _, _) -> invalid where (Printf.sprintf "Expected symbol but got '%s'" tag)

let as_int where n =
  match payload_string where n with
  | Ok v -> begin
      try Ok (int_of_string v)
      with Failure _ -> invalid where (Printf.sprintf "Invalid int literal '%s'" v)
    end
  | Error _ as e -> e

let as_float where n =
  match payload_string where n with
  | Ok v -> begin
      try Ok (float_of_string v)
      with Failure _ -> invalid where (Printf.sprintf "Invalid float literal '%s'" v)
    end
  | Error _ as e -> e

let as_bool where n =
  match payload_string where n with
  | Ok "true" -> Ok true
  | Ok "false" -> Ok false
  | Ok v -> invalid where (Printf.sprintf "Invalid bool literal '%s'" v)
  | Error _ as e -> e

let map_results f xs =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
      match f x with
      | Ok y -> aux (y :: acc) rest
      | Error _ as e -> e
  in
  aux [] xs

let rec expr_of_node (n : node) : (Ast.expr, decode_error) result =
  match n with
  | Node ("int", _, _) ->
    let* i = as_int "expr:int" n in
    Ok (IntLit i)
  | Node ("float", _, _) ->
    let* f = as_float "expr:float" n in
    Ok (FloatLit f)
  | Node ("string", _, _) ->
    let* s = payload_string "expr:string" n in
    Ok (StringLit s)
  | Node ("bool", _, _) ->
    let* b = as_bool "expr:bool" n in
    Ok (BoolLit b)
  | Node ("nil", _, _) -> Ok Nil
  | Node ("array", _, elems) ->
    let* parts = map_results expr_of_node elems in
    Ok (ArrayLit parts)
  | Node ("struct", _, pairs) ->
    let decode_pair = function
      | Node ("pair", _, [k; v]) ->
        let* k' = expr_of_node k in
        let* v' = expr_of_node v in
        Ok (k', v')
      | Node (tag, _, _) -> invalid "expr:struct" (Printf.sprintf "Expected pair, got '%s'" tag)
    in
    let* pair_values = map_results decode_pair pairs in
    Ok (HashLit pair_values)
  | Node ("sym", _, _) ->
    let* s = payload_string "expr:sym" n in
    Ok (Var s)
  | Node ("index", _, [e; idx]) ->
    let* e' = expr_of_node e in
    let* idx' = expr_of_node idx in
    Ok (Index (e', idx'))
  | Node ("fn", _, [Node ("params", _, params); Node ("do", _, body)]) ->
    let* names = map_results (sym_name "expr:fn-params") params in
    let* stmts = map_results stmt_of_node body in
    Ok (Lambda (names, stmts))
  | Node ("regex", _, [pat; flags]) ->
    let* p = payload_string "expr:regex-pattern" pat in
    let* f = payload_string "expr:regex-flags" flags in
    Ok (RegexLit (p, f))
  | Node ("regex_match", _, [e; pat; flags]) ->
    let* e' = expr_of_node e in
    let* p = payload_string "expr:regex-match-pattern" pat in
    let* f = payload_string "expr:regex-match-flags" flags in
    Ok (RegexMatch (e', p, f))
  | Node ("regex_replace", _, [e; pat; repl; flags]) ->
    let* e' = expr_of_node e in
    let* p = payload_string "expr:regex-replace-pattern" pat in
    let* r = payload_string "expr:regex-replace-repl" repl in
    let* f = payload_string "expr:regex-replace-flags" flags in
    Ok (RegexReplace (e', p, r, f))
  | Node ("call", _, func :: args) ->
    let* fn = expr_of_node func in
    let* argv = map_results expr_of_node args in
    Ok (Call (fn, argv))
  | Node (tag, _, [a; b]) -> begin
      match binop_of_tag tag with
      | Some op ->
        let* a' = expr_of_node a in
        let* b' = expr_of_node b in
        Ok (BinOp (op, a', b'))
      | None ->
        let* fn = expr_of_node (Node ("sym", default_meta (), [Node (tag, default_meta (), [])])) in
        let* argv = map_results expr_of_node [a; b] in
        Ok (Call (fn, argv))
    end
  | Node (tag, _, [e]) -> begin
      match unary_of_tag tag with
      | Some op ->
        let* e' = expr_of_node e in
        Ok (UnaryOp (op, e'))
      | None ->
        let* fn = expr_of_node (Node ("sym", default_meta (), [Node (tag, default_meta (), [])])) in
        let* argv = map_results expr_of_node [e] in
        Ok (Call (fn, argv))
    end
  | Node (tag, _, args) ->
    let fn = Var tag in
    let* argv = map_results expr_of_node args in
    Ok (Call (fn, argv))

and stmt_of_node (n : node) : (Ast.stmt, decode_error) result =
  match n with
  | Node ("let", _, [name; e]) ->
    let* v = sym_name "stmt:let-name" name in
    let* e' = expr_of_node e in
    Ok (Let (v, e'))
  | Node ("set", _, [name; e]) ->
    let* v = sym_name "stmt:set-name" name in
    let* e' = expr_of_node e in
    Ok (Assign (v, e'))
  | Node ("set-index", _, [target; idx; value]) ->
    let* t = expr_of_node target in
    let* i = expr_of_node idx in
    let* v = expr_of_node value in
    Ok (IndexAssign (t, i, v))
  | Node ("if", _, [cond; Node ("do", _, then_nodes); Node ("do", _, else_nodes)]) ->
    let* c = expr_of_node cond in
    let* t = map_results stmt_of_node then_nodes in
    let* e = map_results stmt_of_node else_nodes in
    Ok (If (c, t, e))
  | Node ("while", _, [cond; Node ("do", _, body_nodes)]) ->
    let* c = expr_of_node cond in
    let* b = map_results stmt_of_node body_nodes in
    Ok (While (c, b))
  | Node ("for", _, [name; iter; Node ("do", _, body_nodes)]) ->
    let* v = sym_name "stmt:for-name" name in
    let* i = expr_of_node iter in
    let* b = map_results stmt_of_node body_nodes in
    Ok (For (v, i, b))
  | Node ("defn", _, [name; Node ("params", _, params); Node ("do", _, body_nodes)]) ->
    let* n' = sym_name "stmt:defn-name" name in
    let* ps = map_results (sym_name "stmt:defn-param") params in
    let* b = map_results stmt_of_node body_nodes in
    Ok (FnDef (n', ps, b))
  | Node ("return", _, [e]) ->
    let* e' = expr_of_node e in
    Ok (Return e')
  | _ ->
    let* e = expr_of_node n in
    Ok (ExprStmt e)

let program_of_node (n : program) : (Ast.program, decode_error) result =
  match n with
  | Node ("program", _, stmts) -> map_results stmt_of_node stmts
  | Node (tag, _, _) -> invalid "program" (Printf.sprintf "Expected program node, got '%s'" tag)
