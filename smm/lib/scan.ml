exception Invalid_input of string

let value_of_string s =
  match s with
  | "true" -> Smm.Smm.Bool true
  | "false" -> Smm.Smm.Bool false
  | _ ->
    (match int_of_string_opt s with
     | Some n -> Smm.Smm.Num n
     | None -> raise (Invalid_input s))

let scan_token () =
  try Some (Scanf.scanf " %s" (fun s -> s)) with
  | End_of_file -> None
  | Scanf.Scan_failure _ -> None

let scan_value () =
  match scan_token () with
  | None | Some "" -> None
  | Some s -> Some (value_of_string s)

let scan_args arity =
  let rec aux n acc =
    if n = 0 then Some (List.rev acc)
    else
      match scan_value () with
      | None -> None
      | Some v -> aux (n - 1) (v :: acc)
  in
  aux arity []
