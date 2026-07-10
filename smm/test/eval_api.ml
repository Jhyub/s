open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

let function_argument_error = "TypeError: function arguments are not supported"

let lower n =
  Pre2.NUM n
  |> Pre.from_pre2
  |> Optimized.from_pre

let expect_num expected exp =
  match Optimized.eval exp with
  | Pre.Num actual, _, _, _ when actual = expected -> ()
  | Pre.Num actual, _, _, _ ->
    failwith
      ("expected " ^ string_of_int expected ^ ", got " ^ string_of_int actual)
  | Pre.Bool _, _, _, _ ->
    failwith "expected a number, got a boolean"

let expect_num_value name evaluator expected = function
  | Pre.Num actual when actual = expected -> ()
  | Pre.Num actual ->
    failwith
      (Printf.sprintf
         "%s: %s expected %d, got %d"
         name evaluator expected actual)
  | Pre.Bool actual ->
    failwith
      (Printf.sprintf
         "%s: %s expected a number, got boolean %b"
         name evaluator actual)

let expect_both_num name expected program =
  let annotated = Pre.from_pre2 program in
  Pre.run annotated |> expect_num_value name "Smm_pre.run" expected;
  let actual, _, _, _ = Optimized.eval (Optimized.from_pre annotated) in
  expect_num_value name "Smm.eval" expected actual

let expect_pre_function_argument_error name annotated =
  match Pre.run annotated with
  | _ ->
    failwith
      (Printf.sprintf
         "%s: Smm_pre.run accepted a function argument"
         name)
  | exception Pre.Error actual when actual = function_argument_error -> ()
  | exception Pre.Error actual ->
    failwith
      (Printf.sprintf
         "%s: Smm_pre.run raised %S instead of %S"
         name actual function_argument_error)
  | exception exn ->
    failwith
      (Printf.sprintf
         "%s: Smm_pre.run raised %s instead of Smm_pre.Error"
         name (Printexc.to_string exn))

let expect_optimized_function_argument_error name annotated =
  match Optimized.eval (Optimized.from_pre annotated) with
  | _ ->
    failwith
      (Printf.sprintf
         "%s: Smm.eval accepted a function argument"
         name)
  | exception Optimized.Error actual when actual = function_argument_error -> ()
  | exception Optimized.Error actual ->
    failwith
      (Printf.sprintf
         "%s: Smm.eval raised %S instead of %S"
         name actual function_argument_error)
  | exception exn ->
    failwith
      (Printf.sprintf
         "%s: Smm.eval raised %s instead of Smm.Error"
         name (Printexc.to_string exn))

let expect_function_argument_error name program =
  let annotated = Pre.from_pre2 program in
  expect_pre_function_argument_error name annotated;
  expect_optimized_function_argument_error name annotated

let with_inc body =
  Pre2.LET
    ( "one",
      Pre2.NUM 1,
      Pre2.LET
        ( "five",
          Pre2.NUM 5,
          Pre2.LETFN
            ( "inc",
              [ "x" ],
              Pre2.ADD (Pre2.VAR "x", Pre2.VAR "one"),
              body ) ) )

let with_apply body =
  with_inc
    (Pre2.LETFN
       ( "apply",
         [ "f"; "x" ],
         Pre2.CALL ("f", [ "x" ]),
         body ))

let primitive_call =
  with_inc (Pre2.CALL ("inc", [ "five" ]))

let function_argument_call =
  with_apply (Pre2.CALL ("apply", [ "inc"; "five" ]))

let untaken_function_argument_call =
  with_apply
    (Pre2.IF
       ( Pre2.TRUE,
         Pre2.VAR "five",
         Pre2.CALL ("apply", [ "inc"; "five" ]) ))

let unused_function_argument_call =
  with_inc
    (Pre2.LETFN
       ( "ignore_function",
         [ "f"; "x" ],
         Pre2.VAR "x",
         Pre2.CALL ("ignore_function", [ "inc"; "five" ]) ))

let shadowed_duplicate_formal_call =
  with_inc
    (Pre2.LETFN
       ( "last",
         [ "x"; "x" ],
         Pre2.VAR "x",
         Pre2.CALL ("last", [ "inc"; "five" ]) ))

let () =
  (* Each lowering starts at Eid 0; separate eval calls must not share traces. *)
  expect_num 1 (lower 1);
  expect_num 2 (lower 2);
  expect_both_num "primitive argument call" 6 primitive_call;
  expect_function_argument_error
    "correctly arity-matched function argument"
    function_argument_call;
  expect_both_num
    "function argument in an untaken branch"
    5 untaken_function_argument_call;
  expect_function_argument_error
    "unused function argument"
    unused_function_argument_call;
  expect_function_argument_error
    "function argument shadowed by a duplicate formal"
    shadowed_duplicate_formal_call
