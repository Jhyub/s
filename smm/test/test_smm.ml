open Smm_

let eval_string s =
  let pgm = Parser.program Lexer.start (Lexing.from_string s) in
  Smm.Smm.run pgm

let assert_string expected src =
  let actual = Pp.string_of_value (eval_string src) in
  if not (String.equal expected actual) then
    failwith (Printf.sprintf "for %S: expected %S, got %S" src expected actual)

let assert_raises expected_exn src =
  match eval_string src with
  | _ -> failwith (Printf.sprintf "for %S: expected exception, got a value" src)
  | exception e ->
    if not (expected_exn e) then
      failwith (Printf.sprintf "for %S: got unexpected exception %s" src (Printexc.to_string e))

let () =
  (* arithmetic / precedence *)
  assert_string "14" "2 + 3 * 4";
  assert_string "20" "(2 + 3) * 4";
  assert_string "0" "7 % 3 - 1";
  assert_string "-3" "-3";
  assert_string "3" "0 - -3";

  (* let *)
  assert_string "5" "let x := 2 in let y := 3 in x + y";
  assert_string "3" "let x := 2 in let x := 3 in x";

  (* if / comparisons *)
  assert_string "1" "if 1 < 2 then 1 else 0";
  assert_string "0" "if 2 < 1 then 1 else 0";
  assert_string "true" "not false";
  assert_string "true" "1 = 1";
  assert_string "false" "1 = 2";

  (* cross-type equality is always false *)
  assert_string "false" "1 = true";
  assert_string "false" "false = 0";

  (* let fn + call (call arguments must be identifiers, per grammar) *)
  assert_string
    "7"
    "let three := 3 in let four := 4 in let fn add(x, y) => x + y in add(three, four)";
  assert_string
    "9"
    "let a := 4 in let b := 5 in let fn add(x, y) => x + y in add(a, b)";

  (* passing a function by name as an argument *)
  assert_string
    "12"
    "let five := 5 in let seven := 7 in \
     let fn add(x, y) => x + y in \
     let fn apply(f, a, b) => f(a, b) in \
     apply(add, five, seven)";

  (* nested let fn *)
  assert_string
    "10"
    "let five := 5 in \
     let fn f(x) => \
       let fn g(y) => x + y in \
       g(x) \
     in f(five)";

  (* non-recursion: f is not visible inside its own body *)
  assert_raises
    (function Smm.Env.Not_bound -> true | _ -> false)
    "let n := 1 in let fn f(m) => f(m) in f(n)";

  (* shadowing a function definition *)
  assert_string
    "1"
    "let one := 1 in \
     let fn f(x) => x + 1 in \
     let fn f(x) => x in \
     f(one)";

  (* unbound identifier *)
  assert_raises (function Smm.Env.Not_bound -> true | _ -> false) "x";

  (* division by zero *)
  assert_raises (function Division_by_zero -> true | _ -> false) "1 / 0";

  (* calling a non-function *)
  assert_raises
    (function Smm.Smm.Error _ -> true | _ -> false)
    "let x := 1 in x()";

  print_endline "All tests passed"
