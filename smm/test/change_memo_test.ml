open Smm_

module Cache = Smm.Cache
module Env = Smm.Env
module Mem = Smm.Mem
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

let string_of_change = function
  | Pre.Same -> "Same"
  | Pre.Diff -> "Diff"
  | Pre.Unknown -> "Unknown"

let expect_change name expected entry =
  let actual = Pre.cent_change entry in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "%s: expected %s, got %s"
         name
         (string_of_change expected)
         (string_of_change actual))

let expect_cached name expected cache eid =
  match Cache.lookup cache eid with
  | Some entry -> expect_change name expected entry
  | None ->
    failwith
      (Printf.sprintf "%s: expected eid %d to be cached" name eid)

let expect_absent name cache eid =
  match Cache.lookup cache eid with
  | None -> ()
  | Some entry ->
    failwith
      (Printf.sprintf
         "%s: eid %d unexpectedly cached as %s"
         name
         eid
         (Pre.cent_change entry |> string_of_change))

let expect_num name expected = function
  | Pre.Num actual when actual = expected -> ()
  | Pre.Num actual ->
    failwith
      (Printf.sprintf "%s: expected %d, got %d" name expected actual)
  | Pre.Bool actual ->
    failwith
      (Printf.sprintf "%s: expected a number, got boolean %b" name actual)

let test_definitive_result_reuse change eid =
  let expression : Pre.exp = (eid, Pre.VAR "x") in
  let change_fn = Pre.eval_change expression in
  let ctrace = Cache.create () in
  let cenv = Env.bind Pre.emptyChangeEnv "x" (Pre.Value change) in
  change_fn (Pre.emptyTrace, cenv, ctrace)
  |> expect_change "first definitive result" change;
  expect_cached "first definitive cache entry" change ctrace eid;
  (* A missing environment would fail if this call recomputed the VAR. *)
  change_fn (Pre.emptyTrace, Pre.emptyChangeEnv, ctrace)
  |> expect_change "reused definitive result" change

let test_unknown_refinement () =
  let root_eid = 200 in
  let x_eid = 201 in
  let y_eid = 202 in
  let expression : Pre.exp =
    (root_eid, Pre.ADD ((x_eid, Pre.VAR "x"), (y_eid, Pre.VAR "y")))
  in
  let change_fn = Pre.eval_change expression in
  let cenv =
    Pre.emptyChangeEnv
    |> fun cenv -> Env.bind cenv "x" (Pre.Value Pre.Unknown)
    |> fun cenv -> Env.bind cenv "y" (Pre.Value Pre.Same)
  in
  let refine expected =
    let ctrace = Cache.create () in
    change_fn (Pre.emptyTrace, cenv, ctrace)
    |> expect_change "initial unknown result" Pre.Unknown;
    expect_absent "unknown root" ctrace root_eid;
    expect_absent "unknown child" ctrace x_eid;
    Cache.bind ctrace x_eid (Pre.Value expected) |> ignore;
    change_fn (Pre.emptyTrace, cenv, ctrace)
    |> expect_change "refined result" expected;
    expect_cached "refined root" expected ctrace root_eid;
    change_fn (Pre.emptyTrace, Pre.emptyChangeEnv, ctrace)
    |> expect_change "reused refined result" expected
  in
  refine Pre.Same;
  refine Pre.Diff

let test_fresh_activation_traces () =
  let eid = 300 in
  let expression : Pre.exp = (eid, Pre.VAR "argument") in
  let change_fn = Pre.eval_change expression in
  let evaluate trace change =
    let cenv =
      Env.bind Pre.emptyChangeEnv "argument" (Pre.Value change)
    in
    change_fn (Pre.emptyTrace, cenv, trace)
    |> expect_change "activation result" change
  in
  let first_trace = Cache.create () in
  let second_trace = Cache.create () in
  evaluate first_trace Pre.Same;
  evaluate second_trace Pre.Diff;
  expect_cached "first activation" Pre.Same first_trace eid;
  expect_cached "second activation" Pre.Diff second_trace eid

let new_state () : Optimized.eval_state =
  {
    function_traces = Hashtbl.create 1;
    active_trace = None;
    debug = false;
  }

let change_trace_with_sentinel () =
  let ctrace = Cache.create () in
  Cache.bind ctrace (-1) (Pre.Value Pre.Diff) |> ignore;
  ctrace

let expect_preserved name ctrace eid =
  expect_cached (name ^ " sentinel") Pre.Diff ctrace (-1);
  expect_cached (name ^ " node") Pre.Same ctrace eid

let test_literal_trace_preservation () =
  let eid = 400 in
  let expression = Optimized.from_pre (eid, Pre.NUM 7) in
  let ctrace = change_trace_with_sentinel () in
  let value, _, _, returned_ctrace =
    Optimized.eval_with_state
      (new_state ())
      Pre.emptyMemory
      Env.empty
      Pre.emptyTrace
      (Cache.create ())
      Pre.emptyChangeEnv
      ctrace
      expression
  in
  expect_num "literal value" 7 value;
  expect_preserved "literal original trace" ctrace eid;
  expect_preserved "literal returned trace" returned_ctrace eid

let test_early_return_trace_preservation () =
  let eid = 500 in
  let expression = Optimized.from_pre (eid, Pre.NUM 11) in
  let ptrace = Cache.create () in
  Cache.bind ptrace (Pre.Eid eid) (Pre.Num 11) |> ignore;
  let ctrace = change_trace_with_sentinel () in
  let value, _, _, returned_ctrace =
    Optimized.eval_with_state
      (new_state ())
      Pre.emptyMemory
      Env.empty
      ptrace
      (Cache.create ())
      Pre.emptyChangeEnv
      ctrace
      expression
  in
  expect_num "early-return value" 11 value;
  expect_preserved "early-return original trace" ctrace eid;
  expect_preserved "early-return returned trace" returned_ctrace eid

let test_variable_trace_preservation () =
  let eid = 600 in
  let expression = Optimized.from_pre (eid, Pre.VAR "x") in
  let location, memory = Mem.alloc Pre.emptyMemory in
  let memory = Mem.store memory location (Pre.Num 13) in
  let env = Env.bind Env.empty "x" (Optimized.Addr location) in
  let cenv = Env.bind Pre.emptyChangeEnv "x" (Pre.Value Pre.Same) in
  let ctrace = change_trace_with_sentinel () in
  let value, _, _, returned_ctrace =
    Optimized.eval_with_state
      (new_state ())
      memory
      env
      Pre.emptyTrace
      (Cache.create ())
      cenv
      ctrace
      expression
  in
  expect_num "variable value" 13 value;
  expect_preserved "variable original trace" ctrace eid;
  expect_preserved "variable returned trace" returned_ctrace eid

let () =
  test_definitive_result_reuse Pre.Same 100;
  test_definitive_result_reuse Pre.Diff 101;
  test_unknown_refinement ();
  test_fresh_activation_traces ();
  test_literal_trace_preservation ();
  test_early_return_trace_preservation ();
  test_variable_trace_preservation ()
