open Tuple_ast

type macro_fn = node -> node option

let macros : (string, macro_fn) Hashtbl.t = Hashtbl.create 32
let max_expansion_depth = 100
let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

let register_macro name fn =
  Hashtbl.replace macros name fn

let clear_macros () =
  Hashtbl.reset macros

let apply_macro_once n =
  let tag = tag_of n in
  match Hashtbl.find_opt macros tag with
  | None -> None
  | Some fn -> fn n

let rec expand_node ~depth n =
  if depth > max_expansion_depth then
    Error (Errors.Macro_error {
      where = "expand";
      message = "Maximum macro expansion depth exceeded";
    })
  else
    let rec rewrite_until_stable current depth_left =
      if depth_left > max_expansion_depth then
        Error (Errors.Macro_error {
          where = "expand";
          message = "Maximum macro rewrite depth exceeded";
        })
      else
        match apply_macro_once current with
        | None -> Ok current
        | Some next -> rewrite_until_stable next (depth_left + 1)
    in
    let* rewritten = rewrite_until_stable n depth in
    match rewritten with
    | Node (tag, meta, args) ->
      let* expanded_args = expand_list ~depth:(depth + 1) args in
      Ok (Node (tag, meta, expanded_args))

and expand_list ~depth = function
  | [] -> Ok []
  | x :: xs ->
    let* x' = expand_node ~depth x in
    let* xs' = expand_list ~depth xs in
    Ok (x' :: xs')

let expand_program (program : node) = expand_node ~depth:0 program
