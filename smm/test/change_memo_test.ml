open Smm_

module Cache = Smm.Cache
module Env = Smm.Env
module Mem = Smm.Mem
module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

let string_of_change = function
  | Pre.Same -> "Same"
  | Pre.Diff -> "Diff"
  | Pre.Unknown -> "Unknown"

let expect_change name expected actual =
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
  | Some change ->
    failwith
      (Printf.sprintf
         "%s: eid %d unexpectedly cached as %s"
         name
         eid
         (string_of_change change))

let expect_num name expected = function
  | Pre.Num actual when actual = expected -> ()
  | Pre.Num actual ->
    failwith
      (Printf.sprintf "%s: expected %d, got %d" name expected actual)
  | Pre.Bool actual ->
    failwith
      (Printf.sprintf "%s: expected a number, got boolean %b" name actual)

let test_definitive_result_reuse change =
  let eid = Pre.root_eid in
  let expression : Pre.exp = (eid, Pre.VAR "x") in
  let change_fn = Pre.eval_change expression in
  let ctrace = Cache.create () in
  let cenv = Env.bind Pre.emptyChangeEnv "x" change in
  change_fn (Pre.emptyTrace, cenv, ctrace)
  |> expect_change "first definitive result" change;
  expect_cached "first definitive cache entry" change ctrace eid;
  (* A missing environment would fail if this call recomputed the VAR. *)
  change_fn (Pre.emptyTrace, Pre.emptyChangeEnv, ctrace)
  |> expect_change "reused definitive result" change

let test_unknown_refinement () =
  let root_eid = Pre.root_eid in
  let x_eid = 1 in
  let y_eid = 2 in
  let expression : Pre.exp =
    (root_eid, Pre.ADD ((x_eid, Pre.VAR "x"), (y_eid, Pre.VAR "y")))
  in
  let change_fn = Pre.eval_change expression in
  let cenv =
    Pre.emptyChangeEnv
    |> fun cenv -> Env.bind cenv "x" Pre.Unknown
    |> fun cenv -> Env.bind cenv "y" Pre.Same
  in
  let refine expected =
    let ctrace = Cache.create () in
    change_fn (Pre.emptyTrace, cenv, ctrace)
    |> expect_change "initial unknown result" Pre.Unknown;
    expect_absent "unknown root" ctrace root_eid;
    expect_absent "unknown child" ctrace x_eid;
    Cache.bind ctrace x_eid expected |> ignore;
    change_fn (Pre.emptyTrace, cenv, ctrace)
    |> expect_change "refined result" expected;
    expect_cached "refined root" expected ctrace root_eid;
    change_fn (Pre.emptyTrace, Pre.emptyChangeEnv, ctrace)
    |> expect_change "reused refined result" expected
  in
  refine Pre.Same;
  refine Pre.Diff

let test_fresh_activation_traces () =
  let eid = Pre.root_eid in
  let expression : Pre.exp = (eid, Pre.VAR "argument") in
  let change_fn = Pre.eval_change expression in
  let evaluate trace change =
    let cenv =
      Env.bind Pre.emptyChangeEnv "argument" change
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

let expect_domain domains fid expected_params expected_eids =
  if fid < 0 || fid >= Array.length domains then
    failwith (Printf.sprintf "missing change-function domain fid %d" fid)
  else
    let (params, change_fns) = domains.(fid) in
    if params <> expected_params then
      failwith
        (Printf.sprintf
           "fid %d has unexpected parameters"
           fid);
    let actual_eids = List.init (Array.length change_fns) Fun.id in
    if actual_eids <> expected_eids then
      failwith
        (Printf.sprintf
           "fid %d has unexpected expression ids"
           fid)

let domain_fixture =
  Pre2.LETFN
    ( "outer",
      [ "x" ],
      Pre2.LETFN
        ( "inner",
          [ "y" ],
          Pre2.ADD (Pre2.VAR "x", Pre2.VAR "y"),
          Pre2.VAR "x" ),
      Pre2.IF
        ( Pre2.TRUE,
          Pre2.NUM 0,
          Pre2.LETFN ("dormant", [], Pre2.NUM 1, Pre2.NUM 2) ) )

let test_function_change_separation () =
  let expression =
    Pre.from_pre2
      (Pre2.LETFN
         ( "f",
           [],
           Pre2.VAR "captured",
           Pre2.VAR "f" ))
  in
  let domains = Pre.compile_change_fns expression in
  expect_domain domains Pre.root_fid [] [ 0; 1 ];
  expect_domain domains 1 [] [ 0 ];
  let (_, root_change_fns) =
    domains.(Pre.root_fid)
  in
  let root_change =
    root_change_fns.(Pre.root_eid)
  in
  let captured_diff =
    Env.bind Pre.emptyChangeEnv "captured" Pre.Diff
  in
  root_change
    (Pre.emptyTrace, captured_diff, Cache.create ())
  |> expect_change "function binding change" Pre.Diff;
  let (_, body_change_fns) = domains.(1) in
  let body_change =
    body_change_fns.(Pre.root_eid)
  in
  let captured_same =
    Env.bind Pre.emptyChangeEnv "captured" Pre.Same
  in
  body_change
    (Pre.emptyTrace, captured_same, Cache.create ())
  |> expect_change "function body change" Pre.Same

let test_change_fn_domains () =
  let check () =
    let annotated = Pre.from_pre2 domain_fixture in
    let domains = Pre.compile_change_fns annotated in
    if Array.length domains <> 4 then
      failwith "expected root and three function change domains";
    expect_domain
      domains Pre.root_fid []
      [ 0; 1; 2; 3; 4; 5 ];
    expect_domain domains 1 [ (2, "x") ] [ 0; 1 ];
    expect_domain domains 2 [ (3, "y") ] [ 0; 1; 2 ];
    expect_domain domains 3 [] [ 0 ];
    let root_change = Pre.eval_change annotated in
    root_change
      (Pre.emptyTrace, Pre.emptyChangeEnv, Cache.create ())
    |> expect_change "root domain result" Pre.Same;
    let (_, outer_change_fns) = domains.(1) in
    let outer_change =
      outer_change_fns.(Pre.root_eid)
    in
    let outer_cenv =
      Env.bind Pre.emptyChangeEnv "x" Pre.Diff
    in
    outer_change
      (Pre.emptyTrace, outer_cenv, Cache.create ())
    |> expect_change "function domain result" Pre.Diff
  in
  check ();
  (* Function ids must restart at one for every annotated program. *)
  check ()

let new_state () : Optimized.eval_state =
  {
    function_traces = Hashtbl.create 1;
    active_trace = None;
    debug = false;
  }

let sentinel_eid = 1

let change_trace_with_sentinel () =
  let ctrace = Cache.create () in
  Cache.bind ctrace sentinel_eid Pre.Diff |> ignore;
  ctrace

let expect_preserved name ctrace eid =
  expect_cached (name ^ " sentinel") Pre.Diff ctrace sentinel_eid;
  expect_cached (name ^ " node") Pre.Same ctrace eid

let test_literal_trace_preservation () =
  let eid = Pre.root_eid in
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
  let eid = Pre.root_eid in
  let expression = Optimized.from_pre (eid, Pre.NUM 11) in
  let ptrace = Cache.create () in
  Cache.bind ptrace eid (Pre.Num 11) |> ignore;
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
  let eid = Pre.root_eid in
  let expression = Optimized.from_pre (eid, Pre.VAR "x") in
  let location, memory = Mem.alloc Pre.emptyMemory in
  let memory = Mem.store memory location (Pre.Num 13) in
  let env = Env.bind Env.empty "x" (Optimized.Addr location) in
  let cenv = Env.bind Pre.emptyChangeEnv "x" Pre.Same in
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
  test_definitive_result_reuse Pre.Same;
  test_definitive_result_reuse Pre.Diff;
  test_unknown_refinement ();
  test_fresh_activation_traces ();
  test_function_change_separation ();
  test_change_fn_domains ();
  test_literal_trace_preservation ();
  test_early_return_trace_preservation ();
  test_variable_trace_preservation ()
