let string_of_value v =
  match v with
  | Smm.Smm.Num n -> string_of_int n
  | Smm.Smm.Bool b -> string_of_bool b
