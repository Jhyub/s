open Smm_

let parse path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      Parser.program Lexer.start (Lexing.from_channel ic)
      |> Smm.Smm_pre.from_pre2
      |> Smm.Smm.from_pre)

let () =
  let path =
    match Array.to_list Sys.argv |> List.tl with
    | [ path ] -> path
    | _ ->
      prerr_endline "Usage: smm <file>";
      exit 1
  in
  let pgm =
    try parse path with
    | Sys_error msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1
    | Lexer.LexicalError msg ->
      Printf.eprintf "Lexical error: %s\n%!" msg;
      exit 1
    | Parser.Error ->
      Printf.eprintf "Syntax error\n%!";
      exit 1
  in
  match Smm.Smm.eval pgm with
  | v, _, _, _ -> print_endline (Pp.string_of_value v)
  | exception Smm.Smm.Error msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1
  | exception Smm.Smm_pre.Error msg ->
    Printf.eprintf "Error: %s\n%!" msg;
    exit 1
  | exception Smm.Env.Not_bound ->
    Printf.eprintf "Error: unbound identifier\n%!";
    exit 1
  | exception Division_by_zero ->
    Printf.eprintf "Error: division by zero\n%!";
    exit 1
  | exception Smm.Mem.Not_initialized ->
    Printf.eprintf "Error: memory not initialized\n%!";
    exit 1
  | exception Smm.Mem.Not_allocated ->
    Printf.eprintf "Error: memory not allocated\n%!";
    exit 1
