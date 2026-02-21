let () =
  let usage =
    "buoy - a proglang for lanzy people\nUsage: buoy <file.slb> --target perl|ocaml|go"
  in
  let target = ref "" in
  let filename = ref "" in
  let speclist = [
    ("--target", Arg.Set_string target, "Target language: perl, ocaml, or go");
  ] in
  Arg.parse speclist (fun f -> filename := f) usage;
  if !filename = "" then (
    Printf.eprintf "%s\n" usage;
    exit 1
  );
  if !target = "" then (
    Printf.eprintf "Error: --target is required (perl, ocaml, or go)\n";
    exit 1
  );
  match Buoy_lib.Buoy.compile_file_target !target !filename with
  | Ok code -> print_string code
  | Error err ->
    Printf.eprintf "%s\n" (Buoy_lib.Errors.format_error err);
    exit 1
