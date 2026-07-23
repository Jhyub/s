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

let rec annotated_functions ((_, body) : Pre.exp) =
  match body with
  | Pre.NUM _ | Pre.TRUE | Pre.FALSE | Pre.VAR _ | Pre.CALL _ -> []
  | Pre.ADD (left, right)
  | Pre.SUB (left, right)
  | Pre.MUL (left, right)
  | Pre.DIV (left, right)
  | Pre.MOD (left, right)
  | Pre.EQUAL (left, right)
  | Pre.LESS (left, right) ->
    annotated_functions left @ annotated_functions right
  | Pre.NOT body -> annotated_functions body
  | Pre.IF (condition, if_true, if_false) ->
    annotated_functions condition
    @ annotated_functions if_true
    @ annotated_functions if_false
  | Pre.LET (_, value, body) ->
    annotated_functions value @ annotated_functions body
  | Pre.LETFN (fid, name, params, function_body, body) ->
    (name, fid, params, function_body)
    :: (annotated_functions function_body @ annotated_functions body)

let function_definition annotated name =
  match
    List.find_opt
      (fun (candidate, _, _, _) -> candidate = name)
      (annotated_functions annotated)
  with
  | Some (_, fid, params, body) -> (fid, params, body)
  | None -> failwith ("missing annotated function " ^ name)

let single_parameter name = function
  | [ (eid, _) ] -> eid
  | _ -> failwith (name ^ ": expected one parameter")

let new_state () : Optimized.eval_state =
  {
    function_traces = Hashtbl.create 4;
    active_trace = None;
    debug = false;
  }

let evaluate_with_state state annotated =
  Optimized.eval_with_state
    state
    Pre.emptyMemory
    Env.empty
    Pre.emptyTrace
    (Cache.create ())
    Pre.emptyChangeEnv
    (Cache.create ())
    (Optimized.from_pre annotated)

let cache_storage name = function
  | Cache.Array storage -> storage
  | Cache.Empty -> failwith (name ^ ": expected allocated cache storage")

let sorted_change_eids change_fns =
  Hashtbl.fold
    (fun eid _ eids -> eid :: eids)
    change_fns
    []
  |> List.sort Int.compare

let expect_dense_change_domain fid (_, change_fns) =
  let actual = sorted_change_eids change_fns in
  let expected = List.init (List.length actual) Fun.id in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "fid %d expression eids are not dense from zero"
         fid)

let test_parameter_eids_and_argument_trace () =
  let annotated = Pre.from_pre2 duplicate_numeric_formal_call in
  let change_fn_tables = Pre.compile_change_fns annotated in
  Hashtbl.iter expect_dense_change_domain change_fn_tables;
  let fid, params, function_body_eid =
    match annotated with
    | _, Pre.LET
        (_, _,
          (_, Pre.LET
            (_, _,
              (_,
                Pre.LETFN
                  (fid, _, params, (function_body_eid, _), _))))) ->
      (fid, params, function_body_eid)
    | _ -> failwith "unexpected annotated duplicate-formal program shape"
  in
  let (_, function_change_fns) =
    Hashtbl.find change_fn_tables fid
  in
  let parameter_start = Hashtbl.length function_change_fns in
  let first_eid, second_eid =
    match params with
    | [ (first_eid, "x"); (second_eid, "x") ]
      when first_eid <> second_eid ->
      (first_eid, second_eid)
    | _ -> failwith "duplicate formal occurrences do not have distinct eids"
  in
  if function_body_eid <> Pre.root_eid
     || first_eid <> parameter_start
     || second_eid <> first_eid + 1
  then failwith "function-local expression and parameter eids are invalid";
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
  let trace =
    match Optimized.function_previous_trace state fid with
    | Some trace -> trace
    | None -> failwith "function did not retain a completed trace"
  in
  expect_argument_trace "first formal trace" 1 trace first_eid;
  expect_argument_trace "second formal trace" 2 trace second_eid

let alternating_calls =
  Pre2.LET
    ( "one",
      Pre2.NUM 1,
      Pre2.LET
        ( "two",
          Pre2.NUM 2,
          Pre2.LET
            ( "three",
              Pre2.NUM 3,
              Pre2.LETFN
                ( "identity",
                  [ "x" ],
                  Pre2.VAR "x",
                  Pre2.LET
                    ( "first",
                      Pre2.CALL ("identity", [ "one" ]),
                      Pre2.LET
                        ( "second",
                          Pre2.CALL ("identity", [ "two" ]),
                          Pre2.CALL ("identity", [ "three" ]) ) ) ) ) ) )

let test_alternating_function_buffers () =
  let annotated = Pre.from_pre2 alternating_calls in
  let fid, params, _ = function_definition annotated "identity" in
  let param_eid = single_parameter "identity" params in
  let state = new_state () in
  let value, _, _, _ = evaluate_with_state state annotated in
  expect_num_value "alternating buffers" "Smm.eval" 3 value;
  let traces = Hashtbl.find state.function_traces fid in
  if not traces.has_previous_trace || traces.previous_trace <> 0 then
    failwith "three calls did not alternate back to the first value buffer";
  if traces.value_traces.(0) == traces.value_traces.(1) then
    failwith "function value buffers unexpectedly alias";
  expect_argument_trace
    "latest alternating buffer" 3 traces.value_traces.(0) param_eid;
  expect_argument_trace
    "previous alternating buffer" 2 traces.value_traces.(1) param_eid;
  let value_cells =
    Array.map
      (fun trace -> (cache_storage "value trace" trace).cells)
      traces.value_traces
  in
  let change_cells = (cache_storage "change trace" traces.change_trace).cells in
  Optimized.reset_function_trace state fid;
  if Hashtbl.find state.function_traces fid != traces then
    failwith "function reset replaced its retained trace state";
  if traces.has_previous_trace then
    failwith "function reset left a previous trace active";
  Array.iteri
    (fun index trace ->
      if (cache_storage "reset value trace" trace).cells != value_cells.(index)
      then failwith "function reset replaced a value backing array";
      if Cache.lookup trace param_eid <> None then
        failwith "function reset left a value trace populated")
    traces.value_traces;
  if (cache_storage "reset change trace" traces.change_trace).cells
     != change_cells
  then failwith "function reset replaced the change backing array"

let nested_calls =
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
              Pre2.LETFN
                ( "twice",
                  [ "x" ],
                  Pre2.LET
                    ( "incremented",
                      Pre2.CALL ("inc", [ "x" ]),
                      Pre2.CALL ("inc", [ "incremented" ]) ),
                  Pre2.CALL ("twice", [ "five" ]) ) ) ) )

let test_nested_function_buffers () =
  let annotated = Pre.from_pre2 nested_calls in
  let inc_fid, inc_params, _ = function_definition annotated "inc" in
  let twice_fid, twice_params, _ = function_definition annotated "twice" in
  let state = new_state () in
  let value, _, _, _ = evaluate_with_state state annotated in
  expect_num_value "nested function buffers" "Smm.eval" 7 value;
  let previous fid =
    match Optimized.function_previous_trace state fid with
    | Some trace -> trace
    | None -> failwith "nested call did not complete a function trace"
  in
  expect_argument_trace
    "nested inc argument" 6 (previous inc_fid)
    (single_parameter "inc" inc_params);
  expect_argument_trace
    "nested twice argument" 5 (previous twice_fid)
    (single_parameter "twice" twice_params)

let dynamic_closures =
  Pre2.LET
    ( "one",
      Pre2.NUM 1,
      Pre2.LET
        ( "two",
          Pre2.NUM 2,
          Pre2.LETFN
            ( "outer",
              [ "x" ],
              Pre2.LETFN
                ( "inner",
                  [],
                  Pre2.VAR "x",
                  Pre2.CALL ("inner", []) ),
              Pre2.LET
                ( "first",
                  Pre2.CALL ("outer", [ "one" ]),
                  Pre2.CALL ("outer", [ "two" ]) ) ) ) )

let test_dynamic_closure_reset () =
  let annotated = Pre.from_pre2 dynamic_closures in
  let inner_fid, _, (inner_body_eid, _) =
    function_definition annotated "inner"
  in
  let state = new_state () in
  let value, _, _, _ = evaluate_with_state state annotated in
  expect_num_value "dynamic closure reset" "Smm.eval" 2 value;
  let trace =
    match Optimized.function_previous_trace state inner_fid with
    | Some trace -> trace
    | None -> failwith "dynamic inner call did not complete a trace"
  in
  expect_argument_trace
    "dynamic inner body" 2 trace inner_body_eid

let failing_second_call =
  Pre2.LET
    ( "zero",
      Pre2.NUM 0,
      Pre2.LET
        ( "one",
          Pre2.NUM 1,
          Pre2.LETFN
            ( "risky",
              [ "x" ],
              Pre2.IF
                ( Pre2.EQUAL (Pre2.VAR "x", Pre2.VAR "zero"),
                  Pre2.DIV (Pre2.VAR "one", Pre2.VAR "zero"),
                  Pre2.VAR "x" ),
              Pre2.LET
                ( "first",
                  Pre2.CALL ("risky", [ "one" ]),
                  Pre2.CALL ("risky", [ "zero" ]) ) ) ) )

let test_failed_call_preserves_previous_trace () =
  let annotated = Pre.from_pre2 failing_second_call in
  let fid, params, _ = function_definition annotated "risky" in
  let param_eid = single_parameter "risky" params in
  let state = new_state () in
  begin
    match evaluate_with_state state annotated with
    | _ -> failwith "risky call unexpectedly succeeded"
    | exception Division_by_zero -> ()
  end;
  let traces = Hashtbl.find state.function_traces fid in
  if not traces.has_previous_trace || traces.previous_trace <> 0 then
    failwith "failed call replaced the previous value buffer";
  let previous =
    match Optimized.function_previous_trace state fid with
    | Some trace -> trace
    | None -> failwith "failed call discarded the completed trace"
  in
  expect_argument_trace "failed call previous argument" 1 previous param_eid;
  match state.active_trace with
  | None -> ()
  | Some _ -> failwith "failed call did not restore its caller trace"

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
  test_parameter_eids_and_argument_trace ();
  test_alternating_function_buffers ();
  test_nested_function_buffers ();
  test_dynamic_closure_reset ();
  test_failed_call_preserves_previous_trace ()
