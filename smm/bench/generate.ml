let output_dir = ref "_build/bench/generated"
let calls = ref Workloads.default_calls
let seed = ref Workloads.default_seed

let options =
  [
    ( "--output-dir",
      Arg.Set_string output_dir,
      "DIR Write generated S-- sources and manifest.csv to DIR" );
    ("-o", Arg.Set_string output_dir, "DIR Alias for --output-dir");
    ( "--calls",
      Arg.Set_int calls,
      Printf.sprintf "N Generate N contributing calls per workload (default: %d)"
        Workloads.default_calls );
    ( "--seed",
      Arg.Set_int seed,
      Printf.sprintf "N Use deterministic generator seed N (default: %d)"
        Workloads.default_seed );
  ]

let reject_argument argument =
  raise (Arg.Bad (Printf.sprintf "unexpected positional argument: %s" argument))

let () =
  Arg.parse options reject_argument
    "Generate deterministic S-- benchmark workloads.";
  if !calls <= 0 then begin
    prerr_endline "generate: --calls must be positive";
    exit 2
  end;
  let written =
    Workloads.write_suite ~output_dir:!output_dir ~calls:!calls ~seed:!seed
  in
  Printf.printf "Generated %d workloads with %d calls each in %s\n%!"
    (List.length written) !calls !output_dir
