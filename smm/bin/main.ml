open Smm_

let parse path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parser.program Lexer.start (Lexing.from_channel ic) |> Smm.Smm.from_pre)

type previous_run =
  { values : Smm.Smm.value list
  ; result : Smm.Smm.value
  }

let print_value ~silent result =
  let result = Pp.string_of_value result in
  if silent then Printf.printf "%s\n%!" result
  else Printf.printf "Result: %s\n%!" result

let eval_once pgm bindings =
  try
    Ok (Smm.Smm.run_with_values pgm bindings)
  with
  | Smm.Smm.Error msg -> Error ("Error: " ^ msg)
  | Smm.Env.Not_bound -> Error "Error: unbound identifier"
  | Division_by_zero -> Error "Error: division by zero"
  | Smm.Mem.Not_initialized -> Error "Error: memory not initialized"
  | Smm.Mem.Not_allocated -> Error "Error: memory not allocated"

let run_once ~silent pgm values bindings =
  match eval_once pgm bindings with
  | Ok result ->
    print_value ~silent result;
    Some { values; result }
  | Error msg ->
    Printf.eprintf "%s\n%!" msg;
    None

let bindings_of_values ids values = List.combine ids values

let maybe_reuse change_fn ids previous values =
  match previous with
  | None -> None
  | Some previous ->
    (match Smm.Smm.final_change change_fn ids previous.values values with
     | Smm.Smm.Same -> Some previous.result
     | Smm.Smm.Diff | Smm.Smm.Unknown -> None
     | exception Smm.Smm.Error _ -> None
     | exception Smm.Env.Not_bound -> None)

let run_or_reuse ~silent pgm change_fn ids previous values =
  let bindings = bindings_of_values ids values in
  match maybe_reuse change_fn ids previous values with
  | Some result ->
    Printf.eprintf "Optimization hit: reusing previous result\n%!";
    print_value ~silent result;
    Some { values; result }
  | None -> run_once ~silent pgm values bindings

let rec silent_loop pgm change_fn ids previous =
  match Scan.scan_args (List.length ids) with
  | exception Scan.Invalid_input s ->
    Printf.eprintf "Error: invalid input: %s\n%!" s;
    silent_loop pgm change_fn ids previous
  | None -> ()
  | Some values ->
    let previous' = run_or_reuse ~silent:true pgm change_fn ids previous values in
    silent_loop pgm change_fn ids previous'

let rec prompt_value id =
  Printf.printf "Value for %s: %!" id;
  match Scan.scan_value () with
  | exception Scan.Invalid_input s ->
    Printf.eprintf "Error: invalid input: %s\n%!" s;
    prompt_value id
  | v -> v

let rec interactive_loop pgm change_fn ids previous i =
  Printf.printf "Run #%d\n%!" i;
  let rec prompt_values ids acc =
    match ids with
    | [] -> Some (List.rev acc)
    | id :: rest ->
      (match prompt_value id with
       | None -> None
       | Some value -> prompt_values rest (value :: acc))
  in
  match prompt_values ids [] with
  | None -> print_newline ()
  | Some values ->
    let previous' = run_or_reuse ~silent:false pgm change_fn ids previous values in
    interactive_loop pgm change_fn ids previous' (i + 1)

let () =
  let silent_flags, files =
    Array.to_list Sys.argv |> List.tl |> List.partition (( = ) "--silent")
  in
  let silent = silent_flags <> [] in
  let path =
    match files with
    | [ path ] -> path
    | _ ->
      prerr_endline "Usage: smm [--silent] <file>";
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
  let ids = Smm.Smm.free_variable_list pgm in
  let change_fn = Smm.Smm.eval_change pgm in
  if not silent then
    Printf.printf "Free variables: (%s)\n%!" (String.concat ", " ids);
  match ids with
  | [] ->
    if not silent then print_endline "Run #1";
    ignore (run_once ~silent pgm [] [])
  | _ ->
    if silent then silent_loop pgm change_fn ids None
    else interactive_loop pgm change_fn ids None 1
