type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  | Concat

type unaryop =
  | Neg | Not

type expr =
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | Nil
  | ArrayLit of expr list
  | HashLit of (expr * expr) list
  | Var of string
  | BinOp of binop * expr * expr
  | UnaryOp of unaryop * expr
  | Call of expr * expr list
  | Index of expr * expr
  | Lambda of string list * stmt list
  | RegexLit of string * string  (* pattern, flags *)
  | RegexMatch of expr * string * string  (* expr =~ /pattern/flags *)
  | RegexReplace of expr * string * string * string  (* expr =~ s/pat/repl/flags *)

and stmt =
  | Let of string * expr
  | Assign of string * expr
  | IndexAssign of expr * expr * expr  (* target, index, value *)
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | FnDef of string * string list * stmt list
  | Return of expr
  | ExprStmt of expr

type program = stmt list
