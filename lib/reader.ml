let is_space = function
  | ' ' | '\t' -> true
  | _ -> false

let trim = String.trim

let is_symbol_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_symbol_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '?' -> true
  | _ -> false

let is_operator_prefix = function
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '!' -> true
  | _ -> false

let is_boundary s i =
  i < 0 || i >= String.length s || not (is_symbol_char s.[i])

let ends_with_keyword s kw =
  let t = trim s in
  let n = String.length t and k = String.length kw in
  n >= k
  && String.sub t (n - k) k = kw
  && is_boundary t (n - k - 1)

let strip_trailing_keyword s kw =
  if ends_with_keyword s kw then
    let t = trim s in
    let n = String.length t and k = String.length kw in
    Some (trim (String.sub t 0 (n - k)))
  else
    None

let starts_with_keyword s kw =
  let n = String.length kw in
  String.length s >= n
  && String.sub s 0 n = kw
  && is_boundary s n

let find_top_level s ch =
  let len = String.length s in
  let rec loop i parens braces brackets in_string escaped =
    if i >= len then None
    else
      let c = s.[i] in
      if in_string then
        if escaped then loop (i + 1) parens braces brackets true false
        else if c = '\\' then loop (i + 1) parens braces brackets true true
        else if c = '"' then loop (i + 1) parens braces brackets false false
        else loop (i + 1) parens braces brackets true false
      else
        match c with
        | '"' -> loop (i + 1) parens braces brackets true false
        | '(' -> loop (i + 1) (parens + 1) braces brackets false false
        | ')' -> loop (i + 1) (max 0 (parens - 1)) braces brackets false false
        | '{' -> loop (i + 1) parens (braces + 1) brackets false false
        | '}' -> loop (i + 1) parens (max 0 (braces - 1)) brackets false false
        | '[' -> loop (i + 1) parens braces (brackets + 1) false false
        | ']' -> loop (i + 1) parens braces (max 0 (brackets - 1)) false false
        | _ when c = ch && parens = 0 && braces = 0 && brackets = 0 -> Some i
        | _ -> loop (i + 1) parens braces brackets false false
  in
  loop 0 0 0 0 false false

let split_top_level_commas s =
  let rec split acc rest =
    match find_top_level rest ',' with
    | None ->
      let part = trim rest in
      List.rev (if part = "" then acc else part :: acc)
    | Some i ->
      let left = trim (String.sub rest 0 i) in
      let right = String.sub rest (i + 1) (String.length rest - i - 1) in
      split (if left = "" then acc else left :: acc) right
  in
  split [] s

let strip_outer_parens s =
  let s = trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
    match find_top_level (String.sub s 1 (len - 2)) ')' with
    | None -> Some (String.sub s 1 (len - 2))
    | Some _ -> None
  else
    None

let normalize_regex_literals s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if i + 1 < len && s.[i] = '#' && s.[i + 1] = '"' then begin
      Buffer.add_char b '/';
      let rec str j escaped =
        if j >= len then j
        else
          let c = s.[j] in
          if escaped then begin
            if c = '/' then Buffer.add_string b "\\/"
            else Buffer.add_char b c;
            str (j + 1) false
          end else if c = '\\' then begin
            Buffer.add_char b c;
            str (j + 1) true
          end else if c = '"' then begin
            Buffer.add_char b '/';
            j + 1
          end else begin
            if c = '/' then Buffer.add_string b "\\/"
            else Buffer.add_char b c;
            str (j + 1) false
          end
      in
      let next = str (i + 2) false in
      loop next
    end else begin
      Buffer.add_char b s.[i];
      loop (i + 1)
    end
  in
  loop 0;
  Buffer.contents b

let rec normalize_expr s =
  let s = normalize_regex_literals (trim s) in
  if s = "" then s
  else
    match strip_outer_parens s with
    | Some inner -> "(" ^ normalize_expr inner ^ ")"
    | None -> begin
        match normalize_call_form s with
        | Some x -> x
        | None -> s
      end

and normalize_call_form s =
  let s = trim s in
  let len = String.length s in
  if len = 0 || not (is_symbol_start s.[0]) then None
  else
    let rec read_sym i =
      if i < len && is_symbol_char s.[i] then read_sym (i + 1) else i
    in
    let sym_end = read_sym 1 in
    let head = String.sub s 0 sym_end in
    if head = "if" || head = "else" || head = "while" || head = "for" || head = "let"
       || head = "fn" || head = "return" then None
    else
      let rest = trim (String.sub s sym_end (len - sym_end)) in
      if rest = "" then None
      else if is_operator_prefix rest.[0] then None
      else
        let args =
          if rest.[0] = '(' then
            match strip_outer_parens rest with
            | Some inner -> split_top_level_commas inner
            | None -> split_top_level_commas rest
          else
            split_top_level_commas rest
        in
        let normalized_args = List.map normalize_expr args in
        Some (Printf.sprintf "%s(%s)" head (String.concat ", " normalized_args))

let find_assignment_eq s =
  match find_top_level s '=' with
  | None -> None
  | Some i ->
    let prev = if i > 0 then s.[i - 1] else '\x00' in
    let next = if i + 1 < String.length s then s.[i + 1] else '\x00' in
    if prev = '=' || prev = '!' || prev = '<' || prev = '>' || next = '=' || next = '~'
    then None
    else Some i

let normalize_if_like prefix line =
  let p = String.length prefix in
  if String.length line >= p + 1 && String.sub line 0 p = prefix then
    let rest = trim (String.sub line p (String.length line - p)) in
    if rest <> "" && rest.[String.length rest - 1] = '{' then
      let cond = trim (String.sub rest 0 (String.length rest - 1)) in
      Some (prefix ^ normalize_expr cond ^ " {")
    else begin
      match strip_trailing_keyword rest "do" with
      | Some cond -> Some (prefix ^ normalize_expr cond ^ " {")
      | None -> None
    end
  else None

let normalize_for line =
  if not (starts_with_keyword line "for") then None
  else
    let rest = trim (String.sub line 3 (String.length line - 3)) in
    let inner_opt =
      if rest <> "" && rest.[String.length rest - 1] = '{' then
        Some (trim (String.sub rest 0 (String.length rest - 1)))
      else
        strip_trailing_keyword rest "do"
    in
    match inner_opt with
    | None -> None
    | Some inner ->
      match find_top_level inner ' ' with
      | None -> None
      | Some first_space ->
        let name = trim (String.sub inner 0 first_space) in
        let tail = trim (String.sub inner (first_space + 1) (String.length inner - first_space - 1)) in
        if not (starts_with_keyword tail "in") then None
        else
          let iter_expr = trim (String.sub tail 2 (String.length tail - 2)) in
          Some (Printf.sprintf "for %s in %s {" name (normalize_expr iter_expr))

let normalize_fn_header line =
  if not (starts_with_keyword line "fn") then None
  else
    let t = trim line in
    if t <> "" && t.[String.length t - 1] = '{' then Some t
    else
      match strip_trailing_keyword t "do" with
      | Some body_head -> Some (body_head ^ " {")
      | None -> None

let normalize_else_line line =
  let t = trim line in
  if t = "else {" || t = "else do" then Some "} else {"
  else if starts_with_keyword t "else if" then
    let rest = trim (String.sub t 4 (String.length t - 4)) in
    let cond_tail =
      if starts_with_keyword rest "if" then
        trim (String.sub rest 2 (String.length rest - 2))
      else
        rest
    in
    if cond_tail <> "" && cond_tail.[String.length cond_tail - 1] = '{' then
      let cond = trim (String.sub cond_tail 0 (String.length cond_tail - 1)) in
      Some (Printf.sprintf "} else if %s {" (normalize_expr cond))
    else
      match strip_trailing_keyword cond_tail "do" with
      | Some cond -> Some (Printf.sprintf "} else if %s {" (normalize_expr cond))
      | None -> None
  else None

let normalize_line line =
  let t = trim line in
  if t = "" then line
  else if t.[0] = '#' then line
  else if t = "end" then "}"
  else
    match normalize_else_line t with
    | Some x -> x
    | None ->
      begin
        match normalize_fn_header t with
        | Some x -> x
        | None ->
          if ends_with_keyword t "do" then
            normalize_expr (match strip_trailing_keyword t "do" with Some h -> h | None -> t) ^ " {"
          else
            begin
              match normalize_if_like "if " t with
              | Some x -> x
              | None -> begin
                  match normalize_if_like "while " t with
                  | Some x -> x
                  | None -> begin
                      match normalize_for t with
                      | Some x -> x
                      | None ->
                        if starts_with_keyword t "return" then
                          let expr = trim (String.sub t 6 (String.length t - 6)) in
                          if expr = "" then t
                          else "return " ^ normalize_expr expr
                        else if starts_with_keyword t "let" then begin
                          match find_assignment_eq t with
                          | Some i ->
                            let left = trim (String.sub t 0 i) in
                            let right = trim (String.sub t (i + 1) (String.length t - i - 1)) in
                            left ^ " = " ^ normalize_expr right
                          | None -> t
                        end else begin
                          match find_assignment_eq t with
                          | Some i ->
                            let left = trim (String.sub t 0 i) in
                            let right = trim (String.sub t (i + 1) (String.length t - i - 1)) in
                            left ^ " = " ^ normalize_expr right
                          | None ->
                            normalize_expr t
                        end
                    end
                end
            end
      end

let rewrite_source (source : string) : string =
  let lines = String.split_on_char '\n' source in
  String.concat "\n" (List.map normalize_line lines)
