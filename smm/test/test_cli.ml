let read_all ic =
  let buffer = Buffer.create 128 in
  (try
     while true do
       Buffer.add_string buffer (input_line ic);
       Buffer.add_char buffer '\n'
     done
   with
   | End_of_file -> ());
  Buffer.contents buffer

let string_of_status status =
  match status with
  | Unix.WEXITED n -> Printf.sprintf "exited %d" n
  | Unix.WSIGNALED n -> Printf.sprintf "signaled %d" n
  | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n

let assert_exited label status =
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith (Printf.sprintf "%s: process %s" label (string_of_status status))

let assert_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let contains haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop i =
    needle_len = 0
    || (i + needle_len <= haystack_len
        && (String.equal (String.sub haystack i needle_len) needle || loop (i + 1)))
  in
  loop 0

let assert_contains label needle haystack =
  if not (contains haystack needle) then
    failwith
      (Printf.sprintf "%s: expected %S to contain %S" label haystack needle)

let run smm args input =
  let argv = Array.of_list (smm :: args) in
  let stdout, stdin, stderr =
    Unix.open_process_args_full smm argv (Unix.environment ())
  in
  output_string stdin input;
  close_out stdin;
  let out = read_all stdout in
  let err = read_all stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  (status, out, err)

let with_program source f =
  let path = Filename.temp_file "smm-cli-" ".s--" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc source);
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path)

let () =
  let smm =
    match Array.to_list Sys.argv with
    | _ :: smm :: _ -> smm
    | _ -> failwith "usage: test_cli <smm>"
  in
  with_program "x + y" (fun path ->
    let status, out, err = run smm [ "--silent"; path ] "2 3\n4 5\n" in
    assert_exited "silent" status;
    assert_equal "silent stdout" "5\n9\n" out;
    assert_equal "silent stderr" "" err;

    let status, out, err = run smm [ "--silent"; path ] "2 3\n2 3\n" in
    assert_exited "silent optimization hit" status;
    assert_equal "silent optimization stdout" "5\n5\n" out;
    assert_equal
      "silent optimization stderr"
      "Optimization hit: reusing previous result\n"
      err;

    let status, out, err = run smm [ path ] "2\n3\n2\n3\n" in
    assert_exited "interactive" status;
    assert_contains "interactive header" "Free variables: (x, y)\n" out;
    assert_contains "interactive run" "Run #1\n" out;
    assert_contains "interactive second run" "Run #2\n" out;
    assert_contains "interactive x prompt" "Value for x:" out;
    assert_contains "interactive y prompt" "Value for y:" out;
    assert_contains "interactive result" "Result: 5\n" out;
    assert_equal
      "interactive stderr"
      "Optimization hit: reusing previous result\n"
      err;

    let status, out, err = run smm [ "--silent"; path ] "bad 2 3\n" in
    assert_exited "invalid input" status;
    assert_equal "invalid input stdout" "5\n" out;
    assert_equal "invalid input stderr" "Error: invalid input: bad\n" err);
  with_program "let y := x in 1" (fun path ->
    let status, out, err = run smm [ "--silent"; path ] "1\n2\n" in
    assert_exited "ignored variable optimization hit" status;
    assert_equal "ignored variable stdout" "1\n1\n" out;
    assert_equal
      "ignored variable stderr"
      "Optimization hit: reusing previous result\n"
      err);
  with_program "x / y" (fun path ->
    let status, out, err = run smm [ "--silent"; path ] "4 2\n1 0\n4 2\n" in
    assert_exited "runtime error clears cache" status;
    assert_equal "runtime error stdout" "2\n2\n" out;
    assert_equal "runtime error stderr" "Error: division by zero\n" err);
  print_endline "CLI tests passed"
