let rec string_of_equality eq =
  match eq with
  | Smm.Smm.Equal -> "Equal"
  | Smm.Smm.Unknown -> "Unknown"
  | Smm.Smm.IfVar id -> Printf.sprintf "IfVar(%s)" id
  | Smm.Smm.IfBoth (eq1, eq2) ->
    Printf.sprintf
      "IfBoth(%s, %s)"
      (string_of_equality eq1)
      (string_of_equality eq2)
  | Smm.Smm.IfEither (eq1, eq2) ->
    Printf.sprintf
      "IfEither(%s, %s)"
      (string_of_equality eq1)
      (string_of_equality eq2)

let rec string_of_eq_val eq_val =
  match eq_val with
  | Smm.Smm.Plain eq -> Printf.sprintf "Plain(%s)" (string_of_equality eq)
  | Smm.Smm.Closure (self, (ids, inner)) ->
    Printf.sprintf
      "Closure(self=%s, params=[%s], inner=%s)"
      (string_of_equality self)
      (String.concat ", " ids)
      (string_of_eq_val inner)

let string_of_value v =
  match v with
  | Smm.Smm.Num n -> string_of_int n
  | Smm.Smm.Bool b -> string_of_bool b
  | Smm.Smm.Function (_, ids, _, _) ->
    Printf.sprintf "<fun (%s)>" (String.concat ", " ids)
