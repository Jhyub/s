open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

let failf format = Printf.ksprintf failwith format

let expect_num benchmark evaluator expected = function
  | Pre.Num actual when actual = expected -> ()
  | Pre.Num actual ->
    failf "%s: %s returned %d, expected %d" benchmark evaluator actual expected
  | Pre.Bool actual ->
    failf "%s: %s returned boolean %b, expected %d" benchmark evaluator actual
      expected

let rec count_calls function_name expression =
  match expression with
  | Pre2.NUM _ | Pre2.TRUE | Pre2.FALSE | Pre2.VAR _ -> 0
  | Pre2.ADD (left, right)
  | Pre2.SUB (left, right)
  | Pre2.MUL (left, right)
  | Pre2.DIV (left, right)
  | Pre2.MOD (left, right)
  | Pre2.EQUAL (left, right)
  | Pre2.LESS (left, right) ->
    count_calls function_name left + count_calls function_name right
  | Pre2.NOT body -> count_calls function_name body
  | Pre2.IF (condition, if_true, if_false) ->
    count_calls function_name condition
    + count_calls function_name if_true
    + count_calls function_name if_false
  | Pre2.CALL (called, _) -> if called = function_name then 1 else 0
  | Pre2.LET (_, value, body) ->
    count_calls function_name value + count_calls function_name body
  | Pre2.LETFN (_, _, function_body, body) ->
    count_calls function_name function_body + count_calls function_name body

let parse source = Parser.program Lexer.start (Lexing.from_string source)

let has_prefix ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let rec aggregate_body calls expression =
  match expression with
  | Pre2.LET (name, _, body)
    when not (has_prefix ~prefix:"arg_" name)
         && count_calls "f" body = calls ->
    aggregate_body calls body
  | Pre2.LETFN (_, _, _, body) when count_calls "f" body = calls ->
    aggregate_body calls body
  | body -> body

let reduction_arguments expression =
  let rec leaf bindings = function
    | Pre2.LET (name, Pre2.NUM value, body)
      when has_prefix ~prefix:"arg_" name ->
      leaf ((name, value) :: bindings) body
    | Pre2.CALL ("f", ids) ->
      ( List.map
          (fun id ->
            match List.assoc_opt id bindings with
            | Some value -> value
            | None -> failf "call argument %s has no local numeric binding" id)
          ids,
        0 )
    | _ -> failwith "aggregate leaf is not a locally bound call to f"
  in
  let rec walk = function
    | Pre2.ADD (left, right) ->
      let left_arguments, left_depth = walk left in
      let right_arguments, right_depth = walk right in
      left_arguments @ right_arguments, 1 + max left_depth right_depth
    | expression ->
      let arguments, depth = leaf [] expression in
      [ arguments ], depth
  in
  walk expression

let ceil_log2 value =
  let rec loop power depth =
    if power >= value then depth else loop (power * 2) (depth + 1)
  in
  loop 1 0

let find name workloads =
  match List.find_opt (fun workload -> workload.Workloads.name = name) workloads with
  | Some workload -> workload
  | None -> failf "missing generated workload %s" name

let assert_same_expected workloads left right =
  let left = find left workloads in
  let right = find right workloads in
  if left.expected <> right.expected then
    failf "%s and %s should have identical aggregate results" left.name right.name

let () =
  let calls = 17 in
  let seed = 12_345 in
  let workloads = Workloads.generate ~calls ~seed in
  let repeated = Workloads.generate ~calls ~seed in
  if List.length workloads <> 23 then
    failf "generated %d workloads, expected 23" (List.length workloads);
  if workloads <> repeated then failwith "generation is not deterministic";
  let names = Hashtbl.create 23 in
  let generated_arguments = Hashtbl.create 23 in
  List.iter
    (fun workload ->
      if Hashtbl.mem names workload.Workloads.name then
        failf "duplicate workload name %s" workload.name;
      Hashtbl.add names workload.name ();
      if workload.calls <> calls || workload.seed <> seed then
        failf "%s has incorrect metadata" workload.name;
      let program = parse workload.source in
      let outer_calls = count_calls "f" program in
      if outer_calls <> calls then
        failf "%s contains %d contributing f calls, expected %d" workload.name
          outer_calls calls;
      let arguments, reduction_depth =
        aggregate_body calls program |> reduction_arguments
      in
      if List.length arguments <> calls then
        failf "%s reduction contains %d leaves, expected %d" workload.name
          (List.length arguments) calls;
      let expected_depth = ceil_log2 calls in
      if reduction_depth <> expected_depth then
        failf "%s reduction depth is %d, expected balanced depth %d"
          workload.name reduction_depth expected_depth;
      Hashtbl.add generated_arguments workload.name arguments;
      let annotated = Pre.from_pre2 program in
      Pre.run annotated
      |> expect_num workload.name "Smm_pre.run" workload.expected;
      let actual, _, _, _ = Optimized.eval ~debug:false (Optimized.from_pre annotated) in
      expect_num workload.name "Smm.eval" workload.expected actual)
    workloads;
  assert_same_expected workloads "arithmetic_linear" "arithmetic_random";
  assert_same_expected workloads "branch_linear" "branch_random";
  assert_same_expected workloads "captured_linear" "captured_random";
  assert_same_expected workloads "nested_linear" "nested_random";
  assert_same_expected workloads "heavy_linear" "heavy_random";
  assert_same_expected workloads "complex_64_constant"
    "complex_64_inline_constant";
  let arguments name = Hashtbl.find generated_arguments name in
  let linear = arguments "arithmetic_linear" in
  let expected_linear = List.init calls (fun index -> [ index + 1 ]) in
  if linear <> expected_linear then
    failwith "arithmetic_linear does not preserve ascending call order";
  let random = arguments "arithmetic_random" in
  if random = linear then failwith "arithmetic_random generated the identity order";
  let random_values = List.map List.hd random |> List.sort Int.compare in
  if random_values <> List.init calls (fun index -> index + 1) then
    failwith "arithmetic_random is not a permutation of the linear inputs";
  List.iter
    (fun name ->
      if arguments name <> random then
        failf "%s does not share the suite's deterministic random order" name)
    [ "branch_random";
      "captured_random";
      "nested_random";
      "heavy_random" ];
  let expected_complex_names =
    [ "complex_16_constant";
      "complex_24_constant";
      "complex_32_constant";
      "complex_64_constant";
      "complex_64_inline_constant";
      "complex_32_linear_misses";
      "complex_64_bursty_eight_runs" ]
  in
  let actual_complex_names =
    List.filter_map
      (fun workload ->
        if has_prefix ~prefix:"complex_" workload.Workloads.name then
          Some workload.name
        else None)
      workloads
  in
  if actual_complex_names <> expected_complex_names then
    failwith "complex workloads are missing or are not appended in suite order";
  let expected_constant = List.init calls (fun _ -> [ 100 ]) in
  List.iter
    (fun name ->
      if arguments name <> expected_constant then
        failf "%s does not use constant input 100" name)
    [ "complex_16_constant";
      "complex_24_constant";
      "complex_32_constant";
      "complex_64_constant";
      "complex_64_inline_constant" ];
  if arguments "complex_32_linear_misses" <> expected_linear then
    failwith "complex_32_linear_misses does not use unique ascending inputs";
  let expected_bursty =
    List.init calls (fun index -> [ ((index * 8) / calls) + 1 ])
  in
  if arguments "complex_64_bursty_eight_runs" <> expected_bursty then
    failwith "complex_64_bursty_eight_runs does not use eight contiguous runs";
  List.iter
    (fun name ->
      let helper_calls =
        count_calls "complex" (parse (find name workloads).source)
      in
      if helper_calls <> 1 then
        failf "%s contains %d static helper calls, expected 1" name helper_calls)
    [ "complex_16_constant";
      "complex_24_constant";
      "complex_32_constant";
      "complex_64_constant";
      "complex_32_linear_misses";
      "complex_64_bursty_eight_runs" ];
  let inline =
    find "complex_64_inline_constant" workloads
    |> fun workload -> parse workload.source
  in
  if count_calls "complex" inline <> 0 then
    failwith "complex_64_inline_constant unexpectedly calls the helper"
