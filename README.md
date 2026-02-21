 # Buoy Multi-Backend Spec (Session Handoff)

  ## Summary

  Build Buoy as a single-host compiler with gradually added backends.
  First production backend after current work: Python, targeting semantic parity for core language + collections + regex (not full async/I/O initially).

  This spec is decision-complete for next implementation prompt.

  ## Scope and Goals

  ### In scope

  1. Stabilize compiler pipeline around canonical tuple AST stages:

  - Reader -> Parser -> Tuple AST -> Normalize -> Macro Expand -> Backend Codegen

  2. Keep current backends (perl, ocaml, go) compiling through unified pipeline.
  3. Implement Python backend v1 with parity for:

  - expressions, assignment, control flow, functions/lambdas
  - core containers currently modeled in AST ([], {}-mapped forms already representable)
  - string/array/hash builtins used by examples
  - regex literals and regex builtins

  4. Add backend conformance tests (shared source fixtures, expected outputs).

  ### Out of scope (this phase)

  - Full cross-platform compiler host support.
  - Full async/runtime parity in Python v1.
  - Full I/O builtin parity in Python v1.
  - Major language grammar rewrite beyond current reader+parser compatibility.

  ## Architectural Contract

  ### Compiler portability

  - Compiler host may remain single-host for now.

  ### Backend strategy

  - Add backends one-by-one; Python first.
  - Every backend uses normalized tuple AST, not direct surface syntax parsing.

  ### Canonical internal model

  - Tuple AST node shape remains canonical internally:
  - [tag, [char_pos, line_nr, module_name], args[]]
  - Existing Tuple_ast.node wrapper is authoritative.
  - Backend codegen receives expanded tuple AST (or validated adapter output).

  ## Public Interfaces / APIs

  ### lib/buoy.ml

  Maintain and use these APIs as primary:

  1. target_of_string : string -> (target, compile_error) result
  2. read_ast : ?module_name:string -> string -> (Tuple_ast.program, compile_error) result
  3. compile_ast : target -> Tuple_ast.program -> (string, compile_error) result
  4. compile_source_target : ?module_name:string -> string -> string -> (string, compile_error) result
  5. compile_file_target : string -> string -> (string, compile_error) result

  ### CLI

  - bin/buoy.ml remains thin wrapper over compile_file_target.
  - Target list will expand to include python when backend lands.

  ## Reader / Syntax Rules (current expected behavior)

  1. Line-based expressions normalize to call forms where applicable.
  2. Comma-separated call args supported:

  - printf "%s", concat ("ass", "hole")

  3. do/end block syntax is supported via reader rewrite:

  - if cond do ... end
  - else do
  - else if cond do

  4. Symbols allow - and ?.
  5. Regex shorthand #"...“ is intended language syntax (must not conflict with comment handling).

  ## Critical Fixes Required Before/With Python Backend

  1. Resolve # ambiguity:

  - #"...“, #[], #{} must not be consumed as comments.
  - Keep comment syntax without breaking new literals.

  2. Remove fragile generic ... do -> { rewrite:

  - only rewrite do in valid statement contexts (if/while/for/fn/else).

  3. Preserve/restore accurate source positions:

  - tuple AST metadata should use real parse positions.
  - error coordinates should map to user source (not rewritten-only offsets).

  ## Python Backend v1 Spec

  ### Output format

  - Default assumption: emit a single self-contained .py file (runtime helpers + program body).

  ### Runtime model

  Implement Python runtime helpers mirroring core semantic ops:

  - truthiness/coercion helpers
  - arithmetic/comparison ops with Buoy semantics
  - concat/index/index-assign
  - collection helpers (length/push/pop/shift/sort/reverse/keys/values/exists/delete/map/filter/each/join/split/substr/etc. as in v1 scope)
  - regex helpers

  ### Codegen pattern

  - Follow existing backend style: emit helper runtime first, then translated program.
  - Builtins dispatched explicitly in generator (like current Perl/OCaml/Go generators).
  - Unknown function calls compile to runtime call helper.

  ## Testing and Acceptance

  ### Conformance test suite

  Create shared tests run per backend (perl, ocaml, go, python once added):

  1. Arithmetic + precedence
  2. Conditionals (if/else if/else)
  3. Loops (while, for in)
  4. Functions/lambdas + returns
  5. Collections (array/hash operations currently supported)
  6. String utilities
  7. Regex match/replace/find-all
  8. Reader syntax cases:

  - comma arg calls
  - paren grouping
  - do/end blocks
  - dashed/question symbols
  - #"...“ literals

  ### Acceptance criteria

  - All existing examples compile via unified pipeline.
  - Python backend v1 passes conformance subset defined above.
  - No silent expression dropping due to comment/literal ambiguity.
  - Error messages remain structured and include source location metadata.

  ## Assumptions and Defaults

  1. Compiler host portability is deferred.
  2. Backend rollout is incremental; Python first.
  3. Python v1 includes core+collections+regex, excluding full async/I/O parity.
  4. Single-file Python output is the default packaging mode.
  5. Tuple AST remains canonical internal representation.
