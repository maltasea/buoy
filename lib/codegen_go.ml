open Ast

let buf = Buffer.create 4096
let indent_level = ref 0
let temp_counter = ref 0
let function_depth = ref 0

let emit s = Buffer.add_string buf s
let emitln s =
  Buffer.add_string buf (String.make (!indent_level * 2) ' ');
  Buffer.add_string buf s;
  Buffer.add_char buf '\n'

let indent f =
  incr indent_level;
  f ();
  decr indent_level

let fresh prefix =
  incr temp_counter;
  Printf.sprintf "__%s_%d" prefix !temp_counter

let go_escape_string s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string b "\\\\"
    | '"' -> Buffer.add_string b "\\\""
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | '\r' -> Buffer.add_string b "\\r"
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let go_quote s = "\"" ^ go_escape_string s ^ "\""

let rec gen_expr = function
  | IntLit i -> string_of_int i
  | FloatLit f -> Printf.sprintf "%g" f
  | StringLit s -> go_quote s
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | Nil -> "nil"
  | ArrayLit elems ->
    let parts = List.map gen_expr elems in
    Printf.sprintf "newArray([]Value{%s})" (String.concat ", " parts)
  | HashLit pairs ->
    let parts = List.map (fun (k, v) ->
      Printf.sprintf "[2]Value{%s, %s}" (gen_expr k) (gen_expr v)
    ) pairs in
    Printf.sprintf "valHash([][2]Value{%s})" (String.concat ", " parts)
  | Var name -> name
  | BinOp (op, l, r) -> gen_binop op l r
  | UnaryOp (Neg, e) -> Printf.sprintf "valNeg(%s)" (gen_expr e)
  | UnaryOp (Not, e) -> Printf.sprintf "valNot(%s)" (gen_expr e)
  | Call (func, args) -> gen_call func args
  | Index (e, idx) -> Printf.sprintf "valIndex(%s, %s)" (gen_expr e) (gen_expr idx)
  | Lambda (params, body) -> gen_lambda params body
  | RegexLit (pat, flags) ->
    Printf.sprintf "RegexValue{Pattern: %s, Flags: %s}" (go_quote pat) (go_quote flags)
  | RegexMatch (e, pat, flags) ->
    Printf.sprintf "valRegexMatch(%s, %s, %s)" (gen_expr e) (go_quote pat) (go_quote flags)
  | RegexReplace (e, pat, repl, flags) ->
    Printf.sprintf "valRegexReplaceExpr(%s, %s, %s, %s)"
      (gen_expr e) (go_quote pat) (go_quote repl) (go_quote flags)

and gen_binop op l r =
  let ls = gen_expr l and rs = gen_expr r in
  match op with
  | Add -> Printf.sprintf "valAdd(%s, %s)" ls rs
  | Sub -> Printf.sprintf "valSub(%s, %s)" ls rs
  | Mul -> Printf.sprintf "valMul(%s, %s)" ls rs
  | Div -> Printf.sprintf "valDiv(%s, %s)" ls rs
  | Mod -> Printf.sprintf "valMod(%s, %s)" ls rs
  | Eq -> Printf.sprintf "valEq(%s, %s)" ls rs
  | Neq -> Printf.sprintf "valNeq(%s, %s)" ls rs
  | Lt -> Printf.sprintf "valLt(%s, %s)" ls rs
  | Gt -> Printf.sprintf "valGt(%s, %s)" ls rs
  | Le -> Printf.sprintf "valLe(%s, %s)" ls rs
  | Ge -> Printf.sprintf "valGe(%s, %s)" ls rs
  | And ->
    Printf.sprintf "valAnd(func() Value { return %s }, func() Value { return %s })" ls rs
  | Or ->
    Printf.sprintf "valOr(func() Value { return %s }, func() Value { return %s })" ls rs
  | Concat -> Printf.sprintf "valConcat(%s, %s)" ls rs

and gen_call func args =
  let gen_arg_array args =
    Printf.sprintf "[]Value{%s}" (String.concat ", " (List.map gen_expr args))
  in
  match func with
  | Var "println" -> Printf.sprintf "valPrintln(%s)" (gen_arg_array args)
  | Var "print" -> Printf.sprintf "valPrint(%s)" (gen_arg_array args)
  | Var "length" -> begin
    match args with
    | [e] -> Printf.sprintf "valLength(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "push" -> begin
    match args with
    | arr :: rest -> Printf.sprintf "valPush(%s, %s)" (gen_expr arr) (gen_arg_array rest)
    | _ -> "nil"
    end
  | Var "pop" -> begin
    match args with
    | [e] -> Printf.sprintf "valPop(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "shift" -> begin
    match args with
    | [e] -> Printf.sprintf "valShift(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "sort" -> begin
    match args with
    | [e] -> Printf.sprintf "valSort(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "reverse" -> begin
    match args with
    | [e] -> Printf.sprintf "valReverse(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "keys" -> begin
    match args with
    | [e] -> Printf.sprintf "valKeys(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "values" -> begin
    match args with
    | [e] -> Printf.sprintf "valValues(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "exists" -> begin
    match args with
    | [Index (e, key)] -> Printf.sprintf "valExists(%s, %s)" (gen_expr e) (gen_expr key)
    | _ -> "false"
    end
  | Var "delete" -> begin
    match args with
    | [Index (e, key)] -> Printf.sprintf "valDelete(%s, %s)" (gen_expr e) (gen_expr key)
    | _ -> "nil"
    end
  | Var "map" -> begin
    match args with
    | [arr; f] -> Printf.sprintf "valMap(%s, %s)" (gen_expr arr) (gen_expr f)
    | _ -> "nil"
    end
  | Var "filter" -> begin
    match args with
    | [arr; f] -> Printf.sprintf "valFilter(%s, %s)" (gen_expr arr) (gen_expr f)
    | _ -> "nil"
    end
  | Var "each" -> begin
    match args with
    | [arr; f] -> Printf.sprintf "valEach(%s, %s)" (gen_expr arr) (gen_expr f)
    | _ -> "nil"
    end
  | Var "join" -> begin
    match args with
    | [sep; arr] -> Printf.sprintf "valJoin(%s, %s)" (gen_expr sep) (gen_expr arr)
    | _ -> "nil"
    end
  | Var "split" -> begin
    match args with
    | [pat; str] -> Printf.sprintf "valSplit(%s, %s)" (gen_expr pat) (gen_expr str)
    | _ -> "nil"
    end
  | Var "substr" -> begin
    match args with
    | [str; start] -> Printf.sprintf "valSubstr(%s, %s, nil)" (gen_expr str) (gen_expr start)
    | [str; start; len] ->
      Printf.sprintf "valSubstr(%s, %s, %s)" (gen_expr str) (gen_expr start) (gen_expr len)
    | _ -> "nil"
    end
  | Var "uppercase" -> begin
    match args with
    | [e] -> Printf.sprintf "valUppercase(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "lowercase" -> begin
    match args with
    | [e] -> Printf.sprintf "valLowercase(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "trim" -> begin
    match args with
    | [e] -> Printf.sprintf "valTrim(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "replace" -> begin
    match args with
    | [str; pat; repl] ->
      Printf.sprintf "valReplace(%s, %s, %s)" (gen_expr str) (gen_expr pat) (gen_expr repl)
    | _ -> "nil"
    end
  | Var "unique" -> begin
    match args with
    | [e] -> Printf.sprintf "valUnique(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "sqrt" -> begin
    match args with
    | [e] -> Printf.sprintf "valSqrt(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "sin" -> begin
    match args with
    | [e] -> Printf.sprintf "valSin(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "cos" -> begin
    match args with
    | [e] -> Printf.sprintf "valCos(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "abs" -> begin
    match args with
    | [e] -> Printf.sprintf "valAbs(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "log" -> begin
    match args with
    | [e] -> Printf.sprintf "valLog(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "floor" -> begin
    match args with
    | [e] -> Printf.sprintf "valFloor(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "ceil" -> begin
    match args with
    | [e] -> Printf.sprintf "valCeil(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "random" -> begin
    match args with
    | [] -> "valRandom(nil)"
    | [e] -> Printf.sprintf "valRandom(%s)" (gen_expr e)
    | _ -> "valRandom(nil)"
    end
  | Var "async" -> begin
    match args with
    | [e] -> Printf.sprintf "valAsync(func() Value { return %s })" (gen_expr e)
    | _ -> "nil"
    end
  | Var "await" -> begin
    match args with
    | [e] -> Printf.sprintf "valAwait(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "int_of" -> begin
    match args with
    | [e] -> Printf.sprintf "valIntOf(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "float_of" -> begin
    match args with
    | [e] -> Printf.sprintf "valFloatOf(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "string_of" -> begin
    match args with
    | [e] -> Printf.sprintf "valStringOf(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "open" -> begin
    match args with
    | [e] -> Printf.sprintf "valOpen(%s, %s)" (gen_expr e) (go_quote "<")
    | [e; mode] -> Printf.sprintf "valOpen(%s, %s)" (gen_expr e) (gen_expr mode)
    | _ -> "nil"
    end
  | Var "close" -> begin
    match args with
    | [e] -> Printf.sprintf "valClose(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "readline" -> begin
    match args with
    | [e] -> Printf.sprintf "valReadline(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "read_file" -> begin
    match args with
    | [e] -> Printf.sprintf "valReadFile(%s)" (gen_expr e)
    | _ -> "nil"
    end
  | Var "writeln" -> begin
    match args with
    | [fh; data] -> Printf.sprintf "valWriteln(%s, %s)" (gen_expr fh) (gen_expr data)
    | _ -> "nil"
    end
  | Var "regex_match" -> begin
    match args with
    | [str; pat] -> Printf.sprintf "valRegexMatchFn(%s, %s)" (gen_expr str) (gen_expr pat)
    | _ -> "false"
    end
  | Var "regex_replace" -> begin
    match args with
    | [str; pat; repl] ->
      Printf.sprintf "valRegexReplaceFn(%s, %s, %s)" (gen_expr str) (gen_expr pat) (gen_expr repl)
    | _ -> "nil"
    end
  | Var "regex_find_all" -> begin
    match args with
    | [str; pat] -> Printf.sprintf "valRegexFindAllFn(%s, %s)" (gen_expr str) (gen_expr pat)
    | _ -> "newArray([]Value{})"
    end
  | Var name -> Printf.sprintf "valCall(%s, %s)" name (gen_arg_array args)
  | f -> Printf.sprintf "valCall(%s, %s)" (gen_expr f) (gen_arg_array args)

and gen_lambda params body =
  let body_str = gen_function_body_to_string params body in
  Printf.sprintf "BuoyFunc(func(_args []Value) Value {\n%s})" body_str

and gen_function_body_to_string params body =
  let saved = Buffer.contents buf in
  Buffer.clear buf;
  let saved_indent = !indent_level in
  let saved_function_depth = !function_depth in
  indent_level := 1;
  function_depth := saved_function_depth + 1;
  List.iteri (fun i p ->
    emitln (Printf.sprintf "var %s Value = argAt(_args, %d)" p i)
  ) params;
  gen_stmts_returning body;
  let result = Buffer.contents buf in
  Buffer.clear buf;
  Buffer.add_string buf saved;
  indent_level := saved_indent;
  function_depth := saved_function_depth;
  result

and gen_stmts_returning stmts =
  match List.rev stmts with
  | [] -> emitln "return nil"
  | last :: rest ->
    List.iter gen_stmt (List.rev rest);
    begin match last with
    | ExprStmt e -> emitln (Printf.sprintf "return %s" (gen_expr e))
    | Return e -> emitln (Printf.sprintf "return %s" (gen_expr e))
    | _ ->
      gen_stmt last;
      emitln "return nil"
    end

and gen_stmt = function
  | Let (name, e) ->
    emitln (Printf.sprintf "var %s Value = %s" name (gen_expr e))
  | Assign (name, e) ->
    emitln (Printf.sprintf "%s = %s" name (gen_expr e))
  | IndexAssign (e, idx, v) ->
    emitln (Printf.sprintf "valIndexAssign(%s, %s, %s)" (gen_expr e) (gen_expr idx) (gen_expr v))
  | If (cond, then_body, []) ->
    emitln (Printf.sprintf "if valTruthy(%s) {" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt then_body);
    emitln "}"
  | If (cond, then_body, else_body) ->
    emitln (Printf.sprintf "if valTruthy(%s) {" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt then_body);
    emitln "} else {";
    indent (fun () -> List.iter gen_stmt else_body);
    emitln "}"
  | While (cond, body) ->
    emitln (Printf.sprintf "for valTruthy(%s) {" (gen_expr cond));
    indent (fun () -> List.iter gen_stmt body);
    emitln "}"
  | For (name, e, body) ->
    let iter_name = fresh "iter" in
    emitln (Printf.sprintf "%s := %s" iter_name (gen_expr e));
    emitln (Printf.sprintf "valIter(%s, func(_item Value) {" iter_name);
    indent (fun () ->
      emitln (Printf.sprintf "var %s Value = _item" name);
      List.iter gen_stmt body
    );
    emitln "})"
  | FnDef (name, params, body) ->
    emitln (Printf.sprintf "var %s Value" name);
    emitln (Printf.sprintf "%s = BuoyFunc(func(_args []Value) Value {" name);
    indent (fun () ->
      incr function_depth;
      List.iteri (fun i p ->
        emitln (Printf.sprintf "var %s Value = argAt(_args, %d)" p i)
      ) params;
      gen_stmts_returning body;
      decr function_depth
    );
    emitln "})"
  | Return e ->
    if !function_depth > 0 then
      emitln (Printf.sprintf "return %s" (gen_expr e))
    else begin
      emitln (Printf.sprintf "_ = %s" (gen_expr e));
      emitln "return"
    end
  | ExprStmt (RegexReplace (Var name, pat, repl, flags)) ->
    emitln (Printf.sprintf "valRegexReplaceInplace(&%s, %s, %s, %s)"
      name (go_quote pat) (go_quote repl) (go_quote flags))
  | ExprStmt e ->
    emitln (Printf.sprintf "_ = %s" (gen_expr e))

let runtime = {|
package main

import (
  "bufio"
  "fmt"
  "math"
  "math/rand"
  "os"
  "regexp"
  "sort"
  "strconv"
  "strings"
  "sync"
  "time"
)

type Value interface{}
type Array []Value
type Hash map[string]Value
type BuoyFunc func([]Value) Value

type Channel struct {
  file   *os.File
  reader *bufio.Reader
}

type Future struct {
  once  sync.Once
  thunk func() Value
  value Value
}

type RegexValue struct {
  Pattern string
  Flags   string
}

func init() {
  rand.Seed(time.Now().UnixNano())
}

func argAt(args []Value, i int) Value {
  if i < 0 || i >= len(args) {
    return nil
  }
  return args[i]
}

func newArray(items []Value) Value {
  out := make(Array, len(items))
  copy(out, items)
  return &out
}

func valHash(pairs [][2]Value) Value {
  h := Hash{}
  for _, p := range pairs {
    h[toKey(p[0])] = p[1]
  }
  return h
}

func valTruthy(v Value) bool {
  switch x := v.(type) {
  case nil:
    return false
  case bool:
    return x
  case int:
    return x != 0
  case float64:
    return x != 0
  case string:
    return x != ""
  default:
    return true
  }
}

func toString(v Value) string {
  switch x := v.(type) {
  case nil:
    return ""
  case string:
    return x
  case bool:
    if x {
      return "1"
    }
    return ""
  case int:
    return strconv.Itoa(x)
  case float64:
    return strconv.FormatFloat(x, 'g', -1, 64)
  case *Array:
    return "(array)"
  case Hash:
    return "(hash)"
  case BuoyFunc:
    return "(function)"
  case *Channel:
    return "(channel)"
  case RegexValue:
    return "/" + x.Pattern + "/" + x.Flags
  default:
    return fmt.Sprint(x)
  }
}

func toInt(v Value) int {
  switch x := v.(type) {
  case nil:
    return 0
  case int:
    return x
  case float64:
    return int(x)
  case bool:
    if x {
      return 1
    }
    return 0
  case string:
    s := strings.TrimSpace(x)
    if i, err := strconv.Atoi(s); err == nil {
      return i
    }
    if f, err := strconv.ParseFloat(s, 64); err == nil {
      return int(f)
    }
    return 0
  default:
    return 0
  }
}

func toFloat(v Value) float64 {
  switch x := v.(type) {
  case nil:
    return 0
  case int:
    return float64(x)
  case float64:
    return x
  case bool:
    if x {
      return 1
    }
    return 0
  case string:
    s := strings.TrimSpace(x)
    if f, err := strconv.ParseFloat(s, 64); err == nil {
      return f
    }
    return 0
  default:
    return 0
  }
}

func toKey(v Value) string {
  return toString(v)
}

func isNumber(v Value) bool {
  switch v.(type) {
  case int, float64:
    return true
  default:
    return false
  }
}

func compareValues(a, b Value) int {
  if isNumber(a) && isNumber(b) {
    af := toFloat(a)
    bf := toFloat(b)
    if af < bf {
      return -1
    }
    if af > bf {
      return 1
    }
    return 0
  }
  as := toString(a)
  bs := toString(b)
  if as < bs {
    return -1
  }
  if as > bs {
    return 1
  }
  return 0
}

func valAdd(a, b Value) Value {
  if ai, ok := a.(int); ok {
    if bi, ok2 := b.(int); ok2 {
      return ai + bi
    }
  }
  return toFloat(a) + toFloat(b)
}

func valSub(a, b Value) Value {
  if ai, ok := a.(int); ok {
    if bi, ok2 := b.(int); ok2 {
      return ai - bi
    }
  }
  return toFloat(a) - toFloat(b)
}

func valMul(a, b Value) Value {
  if ai, ok := a.(int); ok {
    if bi, ok2 := b.(int); ok2 {
      return ai * bi
    }
  }
  return toFloat(a) * toFloat(b)
}

func valDiv(a, b Value) Value {
  if ai, ok := a.(int); ok {
    if bi, ok2 := b.(int); ok2 && bi != 0 {
      return ai / bi
    }
  }
  return toFloat(a) / toFloat(b)
}

func valMod(a, b Value) Value {
  bi := toInt(b)
  if bi == 0 {
    return 0
  }
  return toInt(a) % bi
}

func valNeg(v Value) Value {
  if i, ok := v.(int); ok {
    return -i
  }
  return -toFloat(v)
}

func valNot(v Value) Value {
  return !valTruthy(v)
}

func valEq(a, b Value) Value {
  switch x := a.(type) {
  case nil:
    return b == nil
  case int:
    if y, ok := b.(int); ok {
      return x == y
    }
    if isNumber(b) {
      return float64(x) == toFloat(b)
    }
  case float64:
    if isNumber(b) {
      return x == toFloat(b)
    }
  case string:
    if y, ok := b.(string); ok {
      return x == y
    }
  case bool:
    if y, ok := b.(bool); ok {
      return x == y
    }
  }
  return toString(a) == toString(b)
}

func valNeq(a, b Value) Value {
  return !valTruthy(valEq(a, b))
}

func valLt(a, b Value) Value {
  return compareValues(a, b) < 0
}

func valGt(a, b Value) Value {
  return compareValues(a, b) > 0
}

func valLe(a, b Value) Value {
  return compareValues(a, b) <= 0
}

func valGe(a, b Value) Value {
  return compareValues(a, b) >= 0
}

func valAnd(a, b func() Value) Value {
  av := a()
  if valTruthy(av) {
    return b()
  }
  return av
}

func valOr(a, b func() Value) Value {
  av := a()
  if valTruthy(av) {
    return av
  }
  return b()
}

func valConcat(a, b Value) Value {
  return toString(a) + toString(b)
}

func valCall(f Value, args []Value) Value {
  switch fn := f.(type) {
  case BuoyFunc:
    return fn(args)
  case func([]Value) Value:
    return fn(args)
  default:
    return nil
  }
}

func valPrintln(args []Value) Value {
  for _, v := range args {
    fmt.Print(toString(v))
  }
  fmt.Println()
  return nil
}

func valPrint(args []Value) Value {
  for _, v := range args {
    fmt.Print(toString(v))
  }
  return nil
}

func valIndex(target, idx Value) Value {
  switch t := target.(type) {
  case *Array:
    i := toInt(idx)
    if i >= 0 && i < len(*t) {
      return (*t)[i]
    }
    return nil
  case Hash:
    return t[toKey(idx)]
  case string:
    r := []rune(t)
    i := toInt(idx)
    if i >= 0 && i < len(r) {
      return string(r[i])
    }
    return nil
  default:
    return nil
  }
}

func valIndexAssign(target, idx, v Value) {
  switch t := target.(type) {
  case *Array:
    i := toInt(idx)
    if i < 0 {
      return
    }
    if i < len(*t) {
      (*t)[i] = v
      return
    }
    for len(*t) < i {
      *t = append(*t, nil)
    }
    *t = append(*t, v)
  case Hash:
    t[toKey(idx)] = v
  }
}

func valLength(v Value) Value {
  switch x := v.(type) {
  case *Array:
    return len(*x)
  case Hash:
    return len(x)
  case string:
    return len([]rune(x))
  default:
    return 0
  }
}

func valPush(arr Value, vals []Value) Value {
  if a, ok := arr.(*Array); ok {
    *a = append(*a, vals...)
  }
  return nil
}

func valPop(arr Value) Value {
  if a, ok := arr.(*Array); ok {
    n := len(*a)
    if n == 0 {
      return nil
    }
    out := (*a)[n-1]
    *a = (*a)[:n-1]
    return out
  }
  return nil
}

func valShift(arr Value) Value {
  if a, ok := arr.(*Array); ok {
    if len(*a) == 0 {
      return nil
    }
    out := (*a)[0]
    *a = (*a)[1:]
    return out
  }
  return nil
}

func valSort(v Value) Value {
  switch x := v.(type) {
  case *Array:
    out := make(Array, len(*x))
    copy(out, *x)
    sort.Slice(out, func(i, j int) bool {
      return compareValues(out[i], out[j]) < 0
    })
    return &out
  case string:
    r := []rune(x)
    sort.Slice(r, func(i, j int) bool { return r[i] < r[j] })
    return string(r)
  default:
    return v
  }
}

func valReverse(v Value) Value {
  switch x := v.(type) {
  case *Array:
    n := len(*x)
    out := make(Array, n)
    for i := 0; i < n; i++ {
      out[i] = (*x)[n-1-i]
    }
    return &out
  case string:
    r := []rune(x)
    for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {
      r[i], r[j] = r[j], r[i]
    }
    return string(r)
  default:
    return v
  }
}

func valUnique(v Value) Value {
  if x, ok := v.(*Array); ok {
    seen := map[string]bool{}
    out := make(Array, 0, len(*x))
    for _, item := range *x {
      k := toString(item)
      if seen[k] {
        continue
      }
      seen[k] = true
      out = append(out, item)
    }
    return &out
  }
  return v
}

func valKeys(v Value) Value {
  if h, ok := v.(Hash); ok {
    out := make(Array, 0, len(h))
    for k := range h {
      out = append(out, k)
    }
    return &out
  }
  return newArray([]Value{})
}

func valValues(v Value) Value {
  if h, ok := v.(Hash); ok {
    out := make(Array, 0, len(h))
    for _, val := range h {
      out = append(out, val)
    }
    return &out
  }
  return newArray([]Value{})
}

func valExists(v, key Value) Value {
  if h, ok := v.(Hash); ok {
    _, found := h[toKey(key)]
    return found
  }
  return false
}

func valDelete(v, key Value) Value {
  if h, ok := v.(Hash); ok {
    k := toKey(key)
    out := h[k]
    delete(h, k)
    return out
  }
  return nil
}

func valMap(arr, f Value) Value {
  if a, ok := arr.(*Array); ok {
    out := make(Array, 0, len(*a))
    for _, item := range *a {
      out = append(out, valCall(f, []Value{item}))
    }
    return &out
  }
  return nil
}

func valFilter(arr, f Value) Value {
  if a, ok := arr.(*Array); ok {
    out := make(Array, 0, len(*a))
    for _, item := range *a {
      if valTruthy(valCall(f, []Value{item})) {
        out = append(out, item)
      }
    }
    return &out
  }
  return nil
}

func valEach(arr, f Value) Value {
  if a, ok := arr.(*Array); ok {
    for _, item := range *a {
      _ = valCall(f, []Value{item})
    }
  }
  return nil
}

func valIter(v Value, f func(Value)) {
  switch x := v.(type) {
  case *Array:
    for _, item := range *x {
      f(item)
    }
  case Hash:
    for _, item := range x {
      f(item)
    }
  case string:
    for _, r := range []rune(x) {
      f(string(r))
    }
  }
}

func valJoin(sep, arr Value) Value {
  if a, ok := arr.(*Array); ok {
    parts := make([]string, 0, len(*a))
    for _, item := range *a {
      parts = append(parts, toString(item))
    }
    return strings.Join(parts, toString(sep))
  }
  return nil
}

func valSplit(pat, str Value) Value {
  pattern := toString(pat)
  s := toString(str)
  re, err := regexp.Compile(pattern)
  parts := []string{}
  if err == nil {
    parts = re.Split(s, -1)
  } else {
    parts = strings.Split(s, pattern)
  }
  out := make(Array, 0, len(parts))
  for _, p := range parts {
    out = append(out, p)
  }
  return &out
}

func valSubstr(str, start, length Value) Value {
  s := []rune(toString(str))
  i := toInt(start)
  if i < 0 {
    i = len(s) + i
  }
  if i < 0 {
    i = 0
  }
  if i >= len(s) {
    return ""
  }
  if length == nil {
    return string(s[i:])
  }
  l := toInt(length)
  if l < 0 {
    return ""
  }
  end := i + l
  if end > len(s) {
    end = len(s)
  }
  return string(s[i:end])
}

func valUppercase(v Value) Value {
  return strings.ToUpper(toString(v))
}

func valLowercase(v Value) Value {
  return strings.ToLower(toString(v))
}

func valTrim(v Value) Value {
  return strings.TrimSpace(toString(v))
}

func valReplace(str, pat, repl Value) Value {
  return strings.ReplaceAll(toString(str), toString(pat), toString(repl))
}

func valSqrt(v Value) Value {
  return math.Sqrt(toFloat(v))
}

func valSin(v Value) Value {
  return math.Sin(toFloat(v))
}

func valCos(v Value) Value {
  return math.Cos(toFloat(v))
}

func valAbs(v Value) Value {
  if i, ok := v.(int); ok {
    if i < 0 {
      return -i
    }
    return i
  }
  return math.Abs(toFloat(v))
}

func valLog(v Value) Value {
  return math.Log(toFloat(v))
}

func valFloor(v Value) Value {
  return int(math.Floor(toFloat(v)))
}

func valCeil(v Value) Value {
  return int(math.Ceil(toFloat(v)))
}

func valRandom(v Value) Value {
  if v == nil {
    return rand.Float64()
  }
  return rand.Float64() * toFloat(v)
}

func valAsync(thunk func() Value) Value {
  if thunk == nil {
    return nil
  }
  return &Future{thunk: thunk}
}

func valAwait(v Value) Value {
  future, ok := v.(*Future)
  if !ok || future == nil {
    return v
  }
  future.once.Do(func() {
    if future.thunk != nil {
      future.value = future.thunk()
      future.thunk = nil
    }
  })
  return future.value
}

func valIntOf(v Value) Value {
  return toInt(v)
}

func valFloatOf(v Value) Value {
  return toFloat(v)
}

func valStringOf(v Value) Value {
  return toString(v)
}

func valOpen(filename, mode Value) Value {
  f := toString(filename)
  m := toString(mode)
  if m == "" {
    m = "<"
  }
  switch m {
  case ">", "w":
    file, err := os.Create(f)
    if err != nil {
      return nil
    }
    return &Channel{file: file}
  case ">>", "a":
    file, err := os.OpenFile(f, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
    if err != nil {
      return nil
    }
    return &Channel{file: file}
  default:
    file, err := os.Open(f)
    if err != nil {
      return nil
    }
    return &Channel{file: file, reader: bufio.NewReader(file)}
  }
}

func valClose(v Value) Value {
  if ch, ok := v.(*Channel); ok && ch != nil && ch.file != nil {
    _ = ch.file.Close()
  }
  return nil
}

func valReadline(v Value) Value {
  ch, ok := v.(*Channel)
  if !ok || ch == nil || ch.file == nil {
    return nil
  }
  if ch.reader == nil {
    ch.reader = bufio.NewReader(ch.file)
  }
  line, err := ch.reader.ReadString('\n')
  if err != nil && line == "" {
    return nil
  }
  line = strings.TrimSuffix(line, "\n")
  line = strings.TrimSuffix(line, "\r")
  return line
}

func valReadFile(filename Value) Value {
  data, err := os.ReadFile(toString(filename))
  if err != nil {
    return nil
  }
  return string(data)
}

func valWriteln(fh, data Value) Value {
  ch, ok := fh.(*Channel)
  if !ok || ch == nil || ch.file == nil {
    return nil
  }
  _, _ = ch.file.WriteString(toString(data) + "\n")
  return nil
}

func compileRegex(pattern, flags string) (*regexp.Regexp, error) {
  if strings.Contains(flags, "i") {
    pattern = "(?i)" + pattern
  }
  return regexp.Compile(pattern)
}

func regexFromValue(v Value) (*regexp.Regexp, error) {
  switch p := v.(type) {
  case RegexValue:
    return compileRegex(p.Pattern, p.Flags)
  case string:
    return regexp.Compile(p)
  default:
    return regexp.Compile(toString(v))
  }
}

func regexReplaceText(text string, re *regexp.Regexp, repl string, global bool) string {
  if global {
    return re.ReplaceAllString(text, repl)
  }
  match := re.FindStringSubmatchIndex(text)
  if match == nil {
    return text
  }
  replaced := re.ExpandString([]byte{}, repl, text, match)
  return text[:match[0]] + string(replaced) + text[match[1]:]
}

func valRegexMatch(v Value, pattern, flags string) Value {
  re, err := compileRegex(pattern, flags)
  if err != nil {
    return false
  }
  return re.FindStringIndex(toString(v)) != nil
}

func valRegexReplaceExpr(v Value, pattern, repl, flags string) Value {
  re, err := compileRegex(pattern, flags)
  if err != nil {
    return toString(v)
  }
  return regexReplaceText(toString(v), re, repl, strings.Contains(flags, "g"))
}

func valRegexReplaceInplace(target *Value, pattern, repl, flags string) {
  if target == nil {
    return
  }
  *target = valRegexReplaceExpr(*target, pattern, repl, flags)
}

func valRegexMatchFn(str, pat Value) Value {
  re, err := regexFromValue(pat)
  if err != nil {
    return false
  }
  return re.FindStringIndex(toString(str)) != nil
}

func valRegexReplaceFn(str, pat, repl Value) Value {
  re, err := regexFromValue(pat)
  if err != nil {
    return toString(str)
  }
  return regexReplaceText(toString(str), re, toString(repl), true)
}

func valRegexFindAllFn(str, pat Value) Value {
  re, err := regexFromValue(pat)
  if err != nil {
    return newArray([]Value{})
  }
  matches := re.FindAllString(toString(str), -1)
  out := make(Array, 0, len(matches))
  for _, m := range matches {
    out = append(out, m)
  }
  return &out
}

func main() {
|}

let generate (program : Ast.program) : string =
  Buffer.clear buf;
  indent_level := 0;
  temp_counter := 0;
  function_depth := 0;
  emit runtime;
  List.iter gen_stmt program;
  emitln "}";
  Buffer.contents buf
