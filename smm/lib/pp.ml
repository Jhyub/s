let string_of_value v =
  match v with
  | Smm.Smm_pre.Num n -> string_of_int n
  | Smm.Smm_pre.Bool b -> string_of_bool b
