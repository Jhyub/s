open Smm_

let parse path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> Parser.program Lexer.start (Lexing.from_channel ic))

let run_once ~silent ctx args =
  let fmt : _ format = if silent then "%s\n" else "Result: %s\n" in
  try Printf.printf fmt (Pp.string_of_value (fst (Smm.Smm.run (ctx, args)))) with
  | Smm.Smm.Error msg -> Printf.eprintf "Error: %s\n%!" msg
  | Division_by_zero -> Printf.eprintf "Error: division by zero\n%!"
  | Smm.Mem.Not_initialized -> Printf.eprintf "Error: memory not initialized\n%!"
  | Smm.Mem.Not_allocated -> Printf.eprintf "Error: memory not allocated\n%!"

let equality_of_eq_val eq_val =
  match eq_val with
  | Smm.Smm.Plain eq -> eq
  | Smm.Smm.Closure (self, _) -> self

let inner_equality pgm =
  try
    match Smm.Smm.eval_eq_val Smm.Smm.emptyEqEnv pgm with
    | Smm.Smm.Closure (_, (_, inner)) -> equality_of_eq_val inner
    | Smm.Smm.Plain eq -> eq
  with
  | Smm.Env.Not_bound -> Smm.Smm.Unknown

let print_inner_equality pgm =
  Printf.printf "Inner equality: %s\n" (Pp.string_of_equality (inner_equality pgm))

let rec silent_loop ctx arity =
  match Scan.scan_args arity with
  | exception Scan.Invalid_input s ->
    Printf.eprintf "Error: invalid input: %s\n%!" s;
    silent_loop ctx arity
  | None -> ()
  | Some args ->
    run_once ~silent:true ctx args;
    silent_loop ctx arity

(* Prompts until a valid value is read; None on EOF *)
let rec prompt_value id =
  Printf.printf "Value for %s: %!" id;
  match Scan.scan_value () with
  | exception Scan.Invalid_input s ->
    Printf.eprintf "Error: invalid input: %s\n%!" s;
    prompt_value id
  | v -> v

let rec interactive_loop ctx ids i =
  Printf.printf "Run #%d\n" i;
  let rec prompt_args ids acc =
    match ids with
    | [] -> Some (List.rev acc)
    | id :: rest ->
      (match prompt_value id with
       | None -> None
       | Some v -> prompt_args rest (v :: acc))
  in
  match prompt_args ids [] with
  | None -> print_newline ()
  | Some args ->
    run_once ~silent:false ctx args;
    interactive_loop ctx ids (i + 1)

let () =
  let silent, files =
    Array.to_list Sys.argv |> List.tl |> List.partition (( = ) "--silent")
  in
  let silent = silent <> [] in
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
  let ctx =
    try Smm.Smm.build_run_ctx (Smm.Smm.emptyMemory, Smm.Smm.emptyEnv, pgm) with
    | Smm.Smm.Error msg ->
      Printf.eprintf "Error: %s\n%!" msg;
      exit 1
    | Smm.Env.Not_bound ->
      Printf.eprintf "Error: unbound identifier\n%!";
      exit 1
  in
  let (_, _, f, _) = ctx in
  let (_, ids, _, _) = Smm.Smm.value_function f in
  if not silent then begin
    Printf.printf "Evaluated function with (%s)\n" (String.concat ", " ids);
    print_inner_equality pgm
  end;
  match ids with
  | [] ->
    if not silent then print_endline "Run #1";
    run_once ~silent ctx []
  | _ -> if silent then silent_loop ctx (List.length ids) else interactive_loop ctx ids 1
