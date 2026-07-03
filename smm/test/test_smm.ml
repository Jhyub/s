open Smm_

let assert_string expected actual =
  if not (String.equal expected actual) then
    failwith (Printf.sprintf "expected %S, got %S" expected actual)

let () =
  assert_string "Equal" (Pp.string_of_equality Smm.Smm.Equal);
  assert_string "Unknown" (Pp.string_of_equality Smm.Smm.Unknown);
  assert_string "IfVar(x)" (Pp.string_of_equality (Smm.Smm.IfVar "x"));
  assert_string
    "IfBoth(IfVar(x), Equal)"
    (Pp.string_of_equality (Smm.Smm.IfBoth (Smm.Smm.IfVar "x", Smm.Smm.Equal)));
  assert_string
    "IfEither(IfVar(x), Unknown)"
    (Pp.string_of_equality
       (Smm.Smm.IfEither (Smm.Smm.IfVar "x", Smm.Smm.Unknown)));
  assert_string
    "Plain(IfBoth(IfVar(x), Equal))"
    (Pp.string_of_eq_val
       (Smm.Smm.Plain (Smm.Smm.IfBoth (Smm.Smm.IfVar "x", Smm.Smm.Equal))));
  assert_string
    "Closure(self=IfVar(outer), params=[f], inner=Closure(self=IfVar(inner), params=[x], inner=Plain(Equal)))"
    (Pp.string_of_eq_val
       (Smm.Smm.Closure
          ( Smm.Smm.IfVar "outer",
            ( [ "f" ],
              Smm.Smm.Closure
                (Smm.Smm.IfVar "inner", ([ "x" ], Smm.Smm.Plain Smm.Smm.Equal))
            ) )))
