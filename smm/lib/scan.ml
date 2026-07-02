exception Invalid_input of string

let value_of_string s =
  match s with
  | "true" -> Smm.Smm.Bool true
  | "false" -> Smm.Smm.Bool false
  | _ ->
    (match int_of_string_opt s with
     | Some n -> Smm.Smm.Num n
     | None -> raise (Invalid_input s))

(* Reads the next whitespace-delimited token; %s yields "" at EOF *)
let scan_value () =
  let s = Scanf.scanf " %s" (fun s -> s) in
  if s = "" then None else Some (value_of_string s)

let scan_args arity =
  let rec aux n acc =
    if n = 0 then Some (List.rev acc)
    else
      match scan_value () with
      | None -> None
      | Some v -> aux (n - 1) (v :: acc)
  in
  aux arity []
