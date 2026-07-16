open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm
module Cache = Smm.Cache
module Env = Smm.Env

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

let duplicate_numeric_formal_call =
  Pre2.LET
    ( "one",
      Pre2.NUM 1,
      Pre2.LET
        ( "two",
          Pre2.NUM 2,
          Pre2.LETFN
            ( "last",
              [ "x"; "x" ],
              Pre2.VAR "x",
              Pre2.CALL ("last", [ "one"; "two" ]) ) ) )

let rec annotated_ids ((eid, body) : Pre.exp) =
  eid
  :: (match body with
      | Pre.NUM _ | Pre.TRUE | Pre.FALSE | Pre.VAR _ | Pre.CALL _ -> []
      | Pre.ADD (left, right)
      | Pre.SUB (left, right)
      | Pre.MUL (left, right)
      | Pre.DIV (left, right)
      | Pre.MOD (left, right)
      | Pre.EQUAL (left, right)
      | Pre.LESS (left, right) ->
        annotated_ids left @ annotated_ids right
      | Pre.NOT body -> annotated_ids body
      | Pre.IF (condition, if_true, if_false) ->
        annotated_ids condition
        @ annotated_ids if_true
        @ annotated_ids if_false
      | Pre.LET (_, value, body) ->
        annotated_ids value @ annotated_ids body
      | Pre.LETFN (_, _, params, function_body, body) ->
        List.map fst params
        @ annotated_ids function_body
        @ annotated_ids body)

let expect_argument_trace name expected trace eid =
  match Cache.lookup trace eid with
  | Some (Pre.Num actual) when actual = expected -> ()
  | Some (Pre.Num actual) ->
    failwith
      (Printf.sprintf "%s: expected %d, got %d" name expected actual)
  | Some (Pre.Bool actual) ->
    failwith
      (Printf.sprintf "%s: expected a number, got boolean %b" name actual)
  | None ->
    failwith (Printf.sprintf "%s: eid %d is absent" name eid)

let test_parameter_eids_and_argument_trace () =
  let annotated = Pre.from_pre2 duplicate_numeric_formal_call in
  let ids = annotated_ids annotated |> List.sort Int.compare in
  let expected_ids = List.init (List.length ids) Fun.id in
  if ids <> expected_ids then
    failwith "expression and parameter eids are not unique and dense";
  let letfn_eid, fid, params, function_body_eid =
    match annotated with
    | _, Pre.LET
        (_, _,
          (_, Pre.LET
            (_, _,
              (letfn_eid,
                Pre.LETFN
                  (fid, _, params, (function_body_eid, _), _))))) ->
      (letfn_eid, fid, params, function_body_eid)
    | _ -> failwith "unexpected annotated duplicate-formal program shape"
  in
  let first_eid, second_eid =
    match params with
    | [ (first_eid, "x"); (second_eid, "x") ]
      when first_eid <> second_eid ->
      (first_eid, second_eid)
    | _ -> failwith "duplicate formal occurrences do not have distinct eids"
  in
  if first_eid <> letfn_eid + 1
     || second_eid <> first_eid + 1
     || function_body_eid <> second_eid + 1
  then failwith "function parameter and body eids are not preorder-local";
  Pre.run annotated
  |> expect_num_value "duplicate numeric formals" "Smm_pre.run" 2;
  let state : Optimized.eval_state =
    {
      function_traces = Hashtbl.create 1;
      active_trace = None;
      debug = false;
    }
  in
  let value, _, _, _ =
    Optimized.eval_with_state
      state
      Pre.emptyMemory
      Env.empty
      Pre.emptyTrace
      (Cache.create ())
      Pre.emptyChangeEnv
      (Cache.create ())
      (Optimized.from_pre annotated)
  in
  expect_num_value "duplicate numeric formals" "Smm.eval" 2 value;
  let trace = Hashtbl.find state.function_traces fid in
  expect_argument_trace "first formal trace" 1 trace first_eid;
  expect_argument_trace "second formal trace" 2 trace second_eid

let () =
  (* Each lowering starts at eid 0; separate eval calls must not share traces. *)
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
    shadowed_duplicate_formal_call;
  test_parameter_eids_and_argument_trace ()
