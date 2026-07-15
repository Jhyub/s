type t = {
  name : string;
  filename : string;
  calls : int;
  seed : int;
  expected : int;
  source : string;
}

type written = {
  workload : t;
  path : string;
}

let default_calls = 4_096
let default_seed = 20_260_710

module Prng = struct
  (* Park-Miller's minimal-standard generator. Using Int64 arithmetic here
     keeps generated permutations independent of OCaml's Random module. *)
  type t = { mutable state : int64 }

  let modulus = 2_147_483_647L
  let span = Int64.pred modulus
  let multiplier = 48_271L

  let create seed =
    let state = Int64.rem (Int64.of_int seed) span in
    let state = if Int64.compare state 0L < 0 then Int64.add state span else state in
    { state = Int64.succ state }

  let next_bound generator bound =
    if bound <= 0 then invalid_arg "Prng.next_bound: non-positive bound";
    generator.state <-
      Int64.rem (Int64.mul generator.state multiplier) modulus;
    Int64.to_int (Int64.rem generator.state (Int64.of_int bound))
end

let range count = Array.init count (fun index -> index + 1)

let shuffle_in_place generator values =
  for index = Array.length values - 1 downto 1 do
    let other = Prng.next_bound generator (index + 1) in
    let saved = values.(index) in
    values.(index) <- values.(other);
    values.(other) <- saved
  done

let permutation generator count =
  let values = range count in
  shuffle_in_place generator values;
  values

let bucket_shuffle generator count =
  let values = range count in
  let rec shuffle_bucket first =
    if first < count then begin
      let quotient = values.(first) / 10 in
      let last = ref (first + 1) in
      while !last < count && values.(!last) / 10 = quotient do
        incr last
      done;
      for index = !last - 1 downto first + 1 do
        let other = first + Prng.next_bound generator (index - first + 1) in
        let saved = values.(index) in
        values.(index) <- values.(other);
        values.(other) <- saved
      done;
      shuffle_bucket !last
    end
  in
  shuffle_bucket 0;
  values

let repeated_eight count = Array.init count (fun index -> (index / 8) + 1)

let eight_contiguous_runs count =
  Array.init count (fun index -> ((index * 8) / count) + 1)

let alternating_extremes count =
  Array.init count (fun index ->
      if index mod 2 = 0 then (index / 2) + 1 else count - (index / 2))

let indent buffer depth = Buffer.add_string buffer (String.make (depth * 2) ' ')

let render_balanced leaves =
  let count = Array.length leaves in
  if count = 0 then invalid_arg "render_balanced: empty expression list";
  let buffer = Buffer.create (count * 80) in
  let rec render first last depth =
    if last - first = 1 then begin
      indent buffer depth;
      Buffer.add_string buffer leaves.(first)
    end else begin
      let middle = first + ((last - first) / 2) in
      indent buffer depth;
      Buffer.add_string buffer "(\n";
      render first middle (depth + 1);
      Buffer.add_string buffer " +\n";
      render middle last (depth + 1);
      Buffer.add_char buffer '\n';
      indent buffer depth;
      Buffer.add_char buffer ')'
    end
  in
  render 0 count 0;
  Buffer.contents buffer

let call_leaf index arguments =
  let bindings = Buffer.create 80 in
  let names =
    List.mapi
      (fun argument_index value ->
        let name = Printf.sprintf "arg_%d_%d" index argument_index in
        Buffer.add_string bindings
          (Printf.sprintf "let %s := %d in " name value);
        name)
      arguments
  in
  Printf.sprintf "(%sf(%s))" (Buffer.contents bindings)
    (String.concat ", " names)

let make ~name ~calls ~seed ~preamble ~arguments ~evaluate =
  if Array.length arguments <> calls then
    invalid_arg (Printf.sprintf "%s: argument count does not match calls" name);
  let expected =
    Array.fold_left
      (fun total call_arguments -> total + evaluate call_arguments)
      0 arguments
  in
  let leaves = Array.mapi call_leaf arguments in
  let body = render_balanced leaves in
  let source =
    Printf.sprintf
      "(* Generated benchmark: %s; calls=%d; seed=%d. *)\n%s\n%s\n"
      name calls seed preamble body
  in
  { name; filename = name ^ ".s--"; calls; seed; expected; source }

let single_arguments values = Array.map (fun value -> [ value ]) values

let arithmetic = function
  | [ x ] -> (x / 10 * 5) + 2
  | _ -> invalid_arg "arithmetic: expected one argument"

let branching = function
  | [ x ] -> if x mod 20 < 10 then (x / 10) + 3 else (x / 10) - 3
  | _ -> invalid_arg "branching: expected one argument"

let multi_argument = function
  | [ x; y ] -> (x / 10 * y) + 2
  | _ -> invalid_arg "multi_argument: expected two arguments"

let heavy = function
  | [ x ] -> (x / 10 * 5) + 2 + (x mod 7)
  | _ -> invalid_arg "heavy: expected one argument"

let staged_modulus = 1_000_003

let staged_arithmetic stages = function
  | [ x ] ->
    let rec fold stage value =
      if stage > stages then value
      else
        fold (stage + 1) (((value * 17) + stage) mod staged_modulus)
    in
    fold 1 x
  | _ -> invalid_arg "staged_arithmetic: expected one argument"

let render_staged_body stages =
  if stages <= 0 then invalid_arg "render_staged_body: non-positive stages";
  let buffer = Buffer.create (stages * 64) in
  for stage = 1 to stages do
    let previous =
      if stage = 1 then "x" else Printf.sprintf "v_%d" (stage - 1)
    in
    Buffer.add_string buffer
      (Printf.sprintf
         "  let v_%d := (%s * 17 + %d) %% %d in\n"
         stage previous stage staged_modulus)
  done;
  Buffer.add_string buffer (Printf.sprintf "  v_%d" stages);
  Buffer.contents buffer

let helper_staged_preamble stages =
  Printf.sprintf
    "let fn complex(x) =>\n%s in\nlet fn f(x) => complex(x) in"
    (render_staged_body stages)

let inline_staged_preamble stages =
  Printf.sprintf "let fn f(x) =>\n%s in" (render_staged_body stages)

let generate ~calls ~seed =
  if calls <= 0 then invalid_arg "Workloads.generate: calls must be positive";
  let generator = Prng.create seed in
  let random_values = permutation generator calls in
  let linear () = range calls in
  let random () = Array.copy random_values in
  let arithmetic_preamble = "let fn f(x) => x / 10 * 5 + 2 in" in
  let branch_preamble =
    "let fn f(x) => if x % 20 < 10 then x / 10 + 3 else x / 10 - 3 in"
  in
  let captured_preamble =
    "let scale := 5 in\n\
     let bias := 2 in\n\
     let fn f(x) => x / 10 * scale + bias in"
  in
  let multiarg_preamble = "let fn f(x, y) => x / 10 * y + 2 in" in
  let nested_preamble =
    "let fn g(x) => x / 10 in\n\
     let fn f(x) => g(x) * 5 + 2 in"
  in
  let heavy_preamble =
    "let fn f(x) =>\n\
     (let quotient := x / 10 in\n\
     let scaled := quotient * 5 + 2 in\n\
     scaled + x % 7) in"
  in
  let make_single name preamble values evaluate =
    make ~name ~calls ~seed ~preamble ~arguments:(single_arguments values)
      ~evaluate
  in
  let arithmetic_linear = linear () in
  let arithmetic_random = random () in
  let arithmetic_constant = Array.make calls 100 in
  let arithmetic_repeated = repeated_eight calls in
  let arithmetic_alternating = alternating_extremes calls in
  let arithmetic_bucketed = bucket_shuffle generator calls in
  let branch_linear = linear () in
  let branch_random = random () in
  let captured_linear = linear () in
  let captured_random = random () in
  let multiarg_stable =
    Array.init calls (fun index -> [ index + 1; 5 ])
  in
  let multiarg_alternating =
    Array.init calls (fun index -> [ index + 1; if index mod 2 = 0 then 5 else 7 ])
  in
  let nested_linear = linear () in
  let nested_random = random () in
  let heavy_linear = linear () in
  let heavy_random = random () in
  let complex_constant = Array.make calls 100 in
  let complex_linear_misses = linear () in
  let complex_bursty_eight_runs = eight_contiguous_runs calls in
  [
    make_single "arithmetic_linear" arithmetic_preamble arithmetic_linear
      arithmetic;
    make_single "arithmetic_random" arithmetic_preamble arithmetic_random
      arithmetic;
    make_single "arithmetic_constant" arithmetic_preamble arithmetic_constant
      arithmetic;
    make_single "arithmetic_repeated_eight" arithmetic_preamble
      arithmetic_repeated arithmetic;
    make_single "arithmetic_alternating_extremes" arithmetic_preamble
      arithmetic_alternating arithmetic;
    make_single "arithmetic_bucket_shuffle" arithmetic_preamble
      arithmetic_bucketed arithmetic;
    make_single "branch_linear" branch_preamble branch_linear branching;
    make_single "branch_random" branch_preamble branch_random branching;
    make_single "captured_linear" captured_preamble captured_linear arithmetic;
    make_single "captured_random" captured_preamble captured_random arithmetic;
    make ~name:"multiarg_stable" ~calls ~seed ~preamble:multiarg_preamble
      ~arguments:multiarg_stable ~evaluate:multi_argument;
    make ~name:"multiarg_alternating" ~calls ~seed ~preamble:multiarg_preamble
      ~arguments:multiarg_alternating ~evaluate:multi_argument;
    make_single "nested_linear" nested_preamble nested_linear arithmetic;
    make_single "nested_random" nested_preamble nested_random arithmetic;
    make_single "heavy_linear" heavy_preamble heavy_linear heavy;
    make_single "heavy_random" heavy_preamble heavy_random heavy;
    make_single "complex_16_constant" (helper_staged_preamble 16)
      complex_constant (staged_arithmetic 16);
    make_single "complex_24_constant" (helper_staged_preamble 24)
      complex_constant (staged_arithmetic 24);
    make_single "complex_32_constant" (helper_staged_preamble 32)
      complex_constant (staged_arithmetic 32);
    make_single "complex_64_constant" (helper_staged_preamble 64)
      complex_constant (staged_arithmetic 64);
    make_single "complex_64_inline_constant" (inline_staged_preamble 64)
      complex_constant (staged_arithmetic 64);
    make_single "complex_32_linear_misses" (helper_staged_preamble 32)
      complex_linear_misses (staged_arithmetic 32);
    make_single "complex_64_bursty_eight_runs" (helper_staged_preamble 64)
      complex_bursty_eight_runs (staged_arithmetic 64);
  ]

let rec ensure_directory path =
  if Sys.file_exists path then begin
    if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
      invalid_arg (Printf.sprintf "%s exists and is not a directory" path)
  end else begin
    let parent = Filename.dirname path in
    if parent <> path then ensure_directory parent;
    try Unix.mkdir path 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let csv_field value =
  let needs_quotes =
    let rec loop index =
      index < String.length value
      &&
      match value.[index] with
      | ',' | '"' | '\n' | '\r' -> true
      | _ -> loop (index + 1)
    in
    loop 0
  in
  if not needs_quotes then value
  else
    let escaped = String.split_on_char '"' value |> String.concat "\"\"" in
    "\"" ^ escaped ^ "\""

let write_suite ~output_dir ~calls ~seed =
  ensure_directory output_dir;
  let workloads = generate ~calls ~seed in
  let written =
    List.map
      (fun workload ->
        let path = Filename.concat output_dir workload.filename in
        write_file path workload.source;
        { workload; path })
      workloads
  in
  let manifest = Buffer.create 2_048 in
  Buffer.add_string manifest "name,path,calls,seed,expected\n";
  List.iter
    (fun { workload; path } ->
      Buffer.add_string manifest
        (Printf.sprintf "%s,%s,%d,%d,%d\n"
           (csv_field workload.name) (csv_field path) workload.calls
           workload.seed workload.expected))
    written;
  write_file (Filename.concat output_dir "manifest.csv")
    (Buffer.contents manifest);
  written
