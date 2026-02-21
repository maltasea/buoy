open Tuple_ast

let rec normalize_node (Node (tag, meta, args)) =
  let normalized_args = List.map normalize_node args in
  match tag, normalized_args with
  | "if", [cond; Node ("do", m1, then_body); Node ("do", m2, else_body)] ->
    Node ("if", meta, [cond; Node ("do", m1, then_body); Node ("do", m2, else_body)])
  | "while", [cond; Node ("do", m, body)] ->
    Node ("while", meta, [cond; Node ("do", m, body)])
  | _ ->
    Node (tag, meta, normalized_args)

let normalize_program = normalize_node
