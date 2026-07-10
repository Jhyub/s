open Smm_

module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

type evaluator =
  | Baseline
  | Change_aware

type worker_phase =
  | Timing
  | Memory

type memory_status = {
  rss : int64 option;
  hwm : int64 option;
}

type metrics = {
  median_seconds : float;
  min_seconds : float;
  max_seconds : float;
  median_allocated_bytes : float;
  rss_before_bytes : int64 option;
  peak_rss_bytes : int64 option;
  peak_growth_bytes : int64 option;
}

type benchmark_result = {
  written : Workloads.written;
  baseline : metrics;
  change_aware : metrics;
}

let default_samples = 7
let default_warmups = 1
let default_output_dir = "_build/bench/generated"
let default_csv_path = "_build/bench/results.csv"

let evaluator_name = function
  | Baseline -> "Smm_pre"
  | Change_aware -> "Smm"

let evaluator_arg = function
  | Baseline -> "pre"
  | Change_aware -> "smm"

let evaluator_of_arg = function
  | "pre" -> Baseline
  | "smm" -> Change_aware
  | value ->
    failwith
      (Printf.sprintf
         "unknown evaluator %S (expected \"pre\" or \"smm\")"
         value)

let phase_arg = function
  | Timing -> "timing"
  | Memory -> "memory"

let phase_of_arg = function
  | "timing" -> Timing
  | "memory" -> Memory
  | value ->
    failwith
      (Printf.sprintf
         "unknown worker phase %S (expected \"timing\" or \"memory\")"
         value)

let has_prefix ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let parse_kib_line line =
  match String.index_opt line ':' with
  | None -> None
  | Some colon ->
    let rest =
      String.sub line (colon + 1) (String.length line - colon - 1)
    in
    (match Scanf.sscanf rest " %Ld %s" (fun kib _unit -> kib) with
     | kib -> Some (Int64.mul kib 1024L)
     | exception _ -> None)

let read_memory_status () =
  let rss = ref None in
  let hwm = ref None in
  let read_channel input =
    try
      while true do
        let line = input_line input in
        if has_prefix ~prefix:"VmRSS:" line then rss := parse_kib_line line
        else if has_prefix ~prefix:"VmHWM:" line then hwm := parse_kib_line line
      done
    with End_of_file -> ()
  in
  match open_in "/proc/self/status" with
  | input ->
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () -> read_channel input);
    { rss = !rss; hwm = !hwm }
  | exception Sys_error _ -> { rss = None; hwm = None }

let subtract_nonnegative after before =
  match after, before with
  | Some after, Some before ->
    Some (if Int64.compare after before > 0 then Int64.sub after before else 0L)
  | _ -> None

let median sorted =
  let values = Array.of_list sorted in
  let count = Array.length values in
  if count = 0 then invalid_arg "median: empty sample";
  if count mod 2 = 1 then values.(count / 2)
  else (values.((count / 2) - 1) +. values.(count / 2)) /. 2.

let summarize values =
  let sorted = List.sort Float.compare values in
  match sorted with
  | [] -> invalid_arg "summarize: empty sample"
  | minimum :: _ ->
    let maximum = List.hd (List.rev sorted) in
    median sorted, minimum, maximum

let parse_program path =
  let input = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      let lexbuf = Lexing.from_channel input in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path };
      Parser.program Lexer.start lexbuf)

let validate_value ~evaluator ~expected = function
  | Pre.Num actual when actual = expected -> ()
  | Pre.Num actual ->
    failwith
      (Printf.sprintf
         "%s produced %d, expected %d"
         (evaluator_name evaluator)
         actual
         expected)
  | Pre.Bool actual ->
    failwith
      (Printf.sprintf
         "%s produced boolean %b, expected integer %d"
         (evaluator_name evaluator)
         actual
         expected)

let prepare_evaluator ~evaluator path =
  let annotated = parse_program path |> Pre.from_pre2 in
  match evaluator with
  | Baseline -> fun () -> Pre.run annotated
  | Change_aware ->
    let optimized = Optimized.from_pre annotated in
    fun () ->
      let value, _, _, _ = Optimized.eval ~debug:false optimized in
      value

let measure_timing_worker ~evaluator ~path ~expected ~samples ~warmups =
  let evaluate = prepare_evaluator ~evaluator path in
  for _ = 1 to warmups do
    Gc.full_major ();
    let value = evaluate () in
    validate_value ~evaluator ~expected value
  done;
  let rec measure remaining elapsed_samples allocation_samples =
    if remaining = 0 then elapsed_samples, allocation_samples
    else begin
      Gc.full_major ();
      let allocated_before = Gc.allocated_bytes () in
      let started = Unix.gettimeofday () in
      let value = evaluate () in
      let elapsed = Unix.gettimeofday () -. started in
      let allocated = Gc.allocated_bytes () -. allocated_before in
      validate_value ~evaluator ~expected value;
      measure
        (remaining - 1)
        (elapsed :: elapsed_samples)
        (allocated :: allocation_samples)
    end
  in
  let elapsed_samples, allocation_samples = measure samples [] [] in
  let median_seconds, min_seconds, max_seconds = summarize elapsed_samples in
  let median_allocated_bytes, _, _ = summarize allocation_samples in
  {
    median_seconds;
    min_seconds;
    max_seconds;
    median_allocated_bytes;
    rss_before_bytes = None;
    peak_rss_bytes = None;
    peak_growth_bytes = None;
  }

let measure_memory_worker ~evaluator ~path ~expected =
  let evaluate = prepare_evaluator ~evaluator path in
  Gc.full_major ();
  let before = read_memory_status () in
  let value = evaluate () in
  let after = read_memory_status () in
  validate_value ~evaluator ~expected value;
  {
    median_seconds = 0.;
    min_seconds = 0.;
    max_seconds = 0.;
    median_allocated_bytes = 0.;
    rss_before_bytes = before.rss;
    peak_rss_bytes = after.hwm;
    peak_growth_bytes = subtract_nonnegative after.hwm before.hwm;
  }

let protocol_option = function
  | Some bytes -> Int64.to_string bytes
  | None -> "NA"

let print_worker_result phase evaluator metrics =
  Printf.printf
    "RESULT\t%s\t%s\t%.17g\t%.17g\t%.17g\t%.17g\t%s\t%s\t%s\n%!"
    (phase_arg phase)
    (evaluator_arg evaluator)
    metrics.median_seconds
    metrics.min_seconds
    metrics.max_seconds
    metrics.median_allocated_bytes
    (protocol_option metrics.rss_before_bytes)
    (protocol_option metrics.peak_rss_bytes)
    (protocol_option metrics.peak_growth_bytes)

let parse_protocol_option value =
  if value = "NA" then None else Some (Int64.of_string value)

let parse_worker_result expected_phase expected_evaluator lines =
  let protocol_lines =
    List.filter (has_prefix ~prefix:"RESULT\t") lines
  in
  let line =
    match List.rev protocol_lines with
    | line :: _ -> line
    | [] -> failwith "benchmark worker did not emit a RESULT record"
  in
  match String.split_on_char '\t' line with
  | [ "RESULT";
      phase;
      evaluator;
      median_seconds;
      min_seconds;
      max_seconds;
      median_allocated_bytes;
      rss_before_bytes;
      peak_rss_bytes;
      peak_growth_bytes ] ->
    let actual_phase = phase_of_arg phase in
    if actual_phase <> expected_phase then
      failwith
        (Printf.sprintf
           "benchmark worker returned phase %S, expected %S"
           phase
           (phase_arg expected_phase));
    let actual_evaluator = evaluator_of_arg evaluator in
    if actual_evaluator <> expected_evaluator then
      failwith
        (Printf.sprintf
           "benchmark worker returned evaluator %S, expected %S"
           evaluator
           (evaluator_arg expected_evaluator));
    {
      median_seconds = float_of_string median_seconds;
      min_seconds = float_of_string min_seconds;
      max_seconds = float_of_string max_seconds;
      median_allocated_bytes = float_of_string median_allocated_bytes;
      rss_before_bytes = parse_protocol_option rss_before_bytes;
      peak_rss_bytes = parse_protocol_option peak_rss_bytes;
      peak_growth_bytes = parse_protocol_option peak_growth_bytes;
    }
  | _ -> failwith (Printf.sprintf "malformed benchmark worker record: %S" line)

let string_of_process_status = function
  | Unix.WEXITED code -> Printf.sprintf "exited with status %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "was killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "was stopped by signal %d" signal

let read_all_lines input =
  let rec loop lines =
    match input_line input with
    | line -> loop (line :: lines)
    | exception End_of_file -> List.rev lines
  in
  loop []

let run_worker_process ~phase ~evaluator ~path ~expected ~samples ~warmups =
  let program = Sys.executable_name in
  let arguments =
    [| program;
       "--worker";
       "--phase";
       phase_arg phase;
       "--evaluator";
       evaluator_arg evaluator;
       "--source";
       path;
       "--expected";
       string_of_int expected;
       "--samples";
       string_of_int samples;
       "--warmups";
       string_of_int warmups |]
  in
  let input = Unix.open_process_args_in program arguments in
  let lines = read_all_lines input in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 -> parse_worker_result phase evaluator lines
  | status ->
    let output = String.concat "\n" lines in
    failwith
      (Printf.sprintf
         "%s %s worker for %s %s%s"
         (evaluator_name evaluator)
         (phase_arg phase)
         path
         (string_of_process_status status)
         (if output = "" then "" else ":\n" ^ output))

let run_evaluator_workers ~evaluator ~path ~expected ~samples ~warmups =
  let timing =
    run_worker_process
      ~phase:Timing
      ~evaluator
      ~path
      ~expected
      ~samples
      ~warmups
  in
  let memory =
    run_worker_process
      ~phase:Memory
      ~evaluator
      ~path
      ~expected
      ~samples
      ~warmups
  in
  {
    timing with
    rss_before_bytes = memory.rss_before_bytes;
    peak_rss_bytes = memory.peak_rss_bytes;
    peak_growth_bytes = memory.peak_growth_bytes;
  }

let ratio numerator denominator =
  if denominator > 0. then Some (numerator /. denominator) else None

let ratio_int64 numerator denominator =
  match numerator, denominator with
  | Some numerator, Some denominator when denominator <> 0L ->
    Some (Int64.to_float numerator /. Int64.to_float denominator)
  | _ -> None

let format_ratio = function
  | Some value -> Printf.sprintf "%.3gx" value
  | None -> "NA"

let format_seconds value = Printf.sprintf "%.3f" (value *. 1000.)

let format_time_range metrics =
  Printf.sprintf
    "%s [%s..%s]"
    (format_seconds metrics.median_seconds)
    (format_seconds metrics.min_seconds)
    (format_seconds metrics.max_seconds)

let mebibytes bytes = bytes /. (1024. *. 1024.)

let format_allocated bytes = Printf.sprintf "%.1f" (mebibytes bytes)

let format_memory = function
  | Some bytes -> Printf.sprintf "%.1f" (mebibytes (Int64.to_float bytes))
  | None -> "NA"

let print_table results =
  let columns =
    [ "benchmark", 31;
      "calls", 7;
      "Smm_pre ms median [min..max]", 31;
      "Smm ms median [min..max]", 31;
      "speedup pre/smm", 15;
      "alloc MiB pre/smm", 21;
      "pre-eval RSS MiB pre/smm", 27;
      "peak RSS MiB pre/smm", 23;
      "peak grow MiB pre/smm", 24 ]
  in
  let border =
    "+"
    ^ String.concat "+" (List.map (fun (_, width) -> String.make (width + 2) '-') columns)
    ^ "+"
  in
  let print_row values =
    print_char '|';
    List.iter2
      (fun (_, width) value -> Printf.printf " %-*s |" width value)
      columns
      values;
    print_newline ()
  in
  print_endline border;
  print_row (List.map fst columns);
  print_endline border;
  List.iter
    (fun result ->
      let workload = result.written.workload in
      print_row
        [ workload.name;
          string_of_int workload.calls;
          format_time_range result.baseline;
          format_time_range result.change_aware;
          format_ratio
            (ratio
               result.baseline.median_seconds
               result.change_aware.median_seconds);
          Printf.sprintf
            "%s / %s"
            (format_allocated result.baseline.median_allocated_bytes)
            (format_allocated result.change_aware.median_allocated_bytes);
          Printf.sprintf
            "%s / %s"
            (format_memory result.baseline.rss_before_bytes)
            (format_memory result.change_aware.rss_before_bytes);
          Printf.sprintf
            "%s / %s"
            (format_memory result.baseline.peak_rss_bytes)
            (format_memory result.change_aware.peak_rss_bytes);
          Printf.sprintf
            "%s / %s"
            (format_memory result.baseline.peak_growth_bytes)
            (format_memory result.change_aware.peak_growth_bytes) ])
    results;
  print_endline border;
  print_endline "Times are milliseconds; allocation and RSS columns are MiB.";
  print_endline "Speedup is Smm_pre / Smm; values above 1 favor Smm."

let rec mkdir_p path =
  if path = "" || path = "." || path = Filename.dirname path then ()
  else if Sys.file_exists path then begin
    if not (Sys.is_directory path) then
      failwith (Printf.sprintf "%s exists but is not a directory" path)
  end else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let csv_escape field =
  let needs_quotes =
    String.exists
      (function ',' | '"' | '\n' | '\r' -> true | _ -> false)
      field
  in
  if not needs_quotes then field
  else begin
    let buffer = Buffer.create (String.length field + 2) in
    Buffer.add_char buffer '"';
    String.iter
      (fun character ->
        if character = '"' then Buffer.add_string buffer "\"\""
        else Buffer.add_char buffer character)
      field;
    Buffer.add_char buffer '"';
    Buffer.contents buffer
  end

let csv_float value = Printf.sprintf "%.9f" value
let csv_allocation value = Printf.sprintf "%.0f" value

let csv_option_int64 = function
  | Some value -> Int64.to_string value
  | None -> "NA"

let csv_ratio = function
  | Some value -> Printf.sprintf "%.6f" value
  | None -> "NA"

let timestamp () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let write_csv ~samples ~warmups path results =
  mkdir_p (Filename.dirname path);
  let output = open_out path in
  let write_row fields =
    output_string output (String.concat "," (List.map csv_escape fields));
    output_char output '\n'
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      write_row
        [ "benchmark";
          "source";
          "calls";
          "seed";
          "expected";
          "samples";
          "warmups";
          "smm_pre_median_seconds";
          "smm_pre_min_seconds";
          "smm_pre_max_seconds";
          "smm_pre_median_allocated_bytes";
          "smm_pre_rss_before_bytes";
          "smm_pre_peak_rss_bytes";
          "smm_pre_peak_growth_bytes";
          "smm_median_seconds";
          "smm_min_seconds";
          "smm_max_seconds";
          "smm_median_allocated_bytes";
          "smm_rss_before_bytes";
          "smm_peak_rss_bytes";
          "smm_peak_growth_bytes";
          "speedup_smm_pre_over_smm";
          "allocated_ratio_smm_pre_over_smm";
          "peak_rss_ratio_smm_pre_over_smm";
          "peak_growth_ratio_smm_pre_over_smm";
          "ocaml_version";
          "timestamp_utc" ];
      let generated_at = timestamp () in
      List.iter
        (fun result ->
          let workload = result.written.workload in
          let baseline = result.baseline in
          let change_aware = result.change_aware in
          write_row
            [ workload.name;
              result.written.path;
              string_of_int workload.calls;
              string_of_int workload.seed;
              string_of_int workload.expected;
              string_of_int samples;
              string_of_int warmups;
              csv_float baseline.median_seconds;
              csv_float baseline.min_seconds;
              csv_float baseline.max_seconds;
              csv_allocation baseline.median_allocated_bytes;
              csv_option_int64 baseline.rss_before_bytes;
              csv_option_int64 baseline.peak_rss_bytes;
              csv_option_int64 baseline.peak_growth_bytes;
              csv_float change_aware.median_seconds;
              csv_float change_aware.min_seconds;
              csv_float change_aware.max_seconds;
              csv_allocation change_aware.median_allocated_bytes;
              csv_option_int64 change_aware.rss_before_bytes;
              csv_option_int64 change_aware.peak_rss_bytes;
              csv_option_int64 change_aware.peak_growth_bytes;
              csv_ratio
                (ratio baseline.median_seconds change_aware.median_seconds);
              csv_ratio
                (ratio
                   baseline.median_allocated_bytes
                   change_aware.median_allocated_bytes);
              csv_ratio
                (ratio_int64 baseline.peak_rss_bytes change_aware.peak_rss_bytes);
              csv_ratio
                (ratio_int64
                   baseline.peak_growth_bytes
                   change_aware.peak_growth_bytes);
              Sys.ocaml_version;
              generated_at ])
        results)

let validate_counts ~calls ~samples ~warmups =
  if calls <= 0 then invalid_arg "--calls must be greater than zero";
  if samples <= 0 then invalid_arg "--samples must be greater than zero";
  if warmups < 0 then invalid_arg "--warmups must not be negative"

let run_parent () =
  let calls = ref Workloads.default_calls in
  let seed = ref Workloads.default_seed in
  let output_dir = ref default_output_dir in
  let samples = ref default_samples in
  let warmups = ref default_warmups in
  let csv_path = ref default_csv_path in
  let options =
    [ "--calls", Arg.Set_int calls, "N calls generated per benchmark";
      "--seed", Arg.Set_int seed, "N deterministic shuffle seed";
      "--output-dir", Arg.Set_string output_dir, "DIR generated source directory";
      "--samples", Arg.Set_int samples, "N measured evaluations per worker";
      "--warmups", Arg.Set_int warmups, "N warmup evaluations per worker";
      "--csv", Arg.Set_string csv_path, "FILE summary CSV path" ]
  in
  Arg.parse
    options
    (fun argument -> raise (Arg.Bad ("unexpected argument: " ^ argument)))
    "Usage: run_benchmarks.exe [OPTIONS]";
  validate_counts ~calls:!calls ~samples:!samples ~warmups:!warmups;
  if !output_dir = "" then invalid_arg "--output-dir must not be empty";
  if !csv_path = "" then invalid_arg "--csv must not be empty";
  let written =
    Workloads.write_suite
      ~output_dir:!output_dir
      ~calls:!calls
      ~seed:!seed
  in
  if written = [] then failwith "workload generator returned no benchmarks";
  Printf.eprintf
    "Generated %d workloads in %s\n%!"
    (List.length written)
    !output_dir;
  let total = List.length written in
  let results =
    List.mapi
      (fun index (written : Workloads.written) ->
        let workload = written.workload in
        Printf.eprintf
          "[%d/%d] %s: Smm_pre\n%!"
          (index + 1)
          total
          workload.name;
        let baseline =
          run_evaluator_workers
            ~evaluator:Baseline
            ~path:written.path
            ~expected:workload.expected
            ~samples:!samples
            ~warmups:!warmups
        in
        Printf.eprintf
          "[%d/%d] %s: Smm\n%!"
          (index + 1)
          total
          workload.name;
        let change_aware =
          run_evaluator_workers
            ~evaluator:Change_aware
            ~path:written.path
            ~expected:workload.expected
            ~samples:!samples
            ~warmups:!warmups
        in
        { written; baseline; change_aware })
      written
  in
  print_table results;
  write_csv ~samples:!samples ~warmups:!warmups !csv_path results;
  Printf.printf "CSV: %s\n%!" !csv_path

let run_worker () =
  let phase = ref "" in
  let evaluator = ref "" in
  let source = ref "" in
  let expected = ref 0 in
  let expected_set = ref false in
  let samples = ref default_samples in
  let warmups = ref default_warmups in
  let options =
    [ "--worker", Arg.Unit (fun () -> ()), "";
      "--phase", Arg.Set_string phase, "";
      "--evaluator", Arg.Set_string evaluator, "";
      "--source", Arg.Set_string source, "";
      ( "--expected",
        Arg.String
          (fun value ->
            expected := int_of_string value;
            expected_set := true),
        "" );
      "--samples", Arg.Set_int samples, "";
      "--warmups", Arg.Set_int warmups, "" ]
  in
  Arg.parse
    options
    (fun argument -> raise (Arg.Bad ("unexpected worker argument: " ^ argument)))
    "internal benchmark worker";
  if !source = "" then invalid_arg "worker source path is missing";
  if not !expected_set then invalid_arg "worker expected value is missing";
  validate_counts ~calls:1 ~samples:!samples ~warmups:!warmups;
  let phase = phase_of_arg !phase in
  let evaluator = evaluator_of_arg !evaluator in
  let metrics =
    match phase with
    | Timing ->
      measure_timing_worker
        ~evaluator
        ~path:!source
        ~expected:!expected
        ~samples:!samples
        ~warmups:!warmups
    | Memory ->
      measure_memory_worker
        ~evaluator
        ~path:!source
        ~expected:!expected
  in
  print_worker_result phase evaluator metrics

let is_worker =
  Array.exists (fun argument -> argument = "--worker") Sys.argv

let () =
  Printexc.record_backtrace true;
  match (if is_worker then run_worker else run_parent) () with
  | () -> ()
  | exception exn ->
    Printf.eprintf "benchmark error: %s\n%!" (Printexc.to_string exn);
    let backtrace = Printexc.get_backtrace () in
    if backtrace <> "" then Printf.eprintf "%s%!" backtrace;
    exit 2
