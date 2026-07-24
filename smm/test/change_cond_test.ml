open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre

let string_of_change = function
  | Pre.Same -> "Same"
  | Pre.Diff -> "Diff"
  | Pre.Unknown -> "Unknown"

let string_of_value = function
  | Pre.Num n -> string_of_int n
  | Pre.Bool b -> string_of_bool b

let rec string_of_cond = function
  | Pre.Atom atom -> string_of_atom atom
  | Pre.And (left, right) ->
    Printf.sprintf "And (%s, %s)" (string_of_cond left) (string_of_cond right)
  | Pre.Or (left, right) ->
    Printf.sprintf "Or (%s, %s)" (string_of_cond left) (string_of_cond right)
  | Pre.Not condition ->
    Printf.sprintf "Not (%s)" (string_of_cond condition)
  | Pre.Always -> "Always"
  | Pre.Impossible -> "Impossible"

and string_of_atom = function
  | Pre.CngIs (eid, change) ->
    Printf.sprintf "CngIs (%d, %s)" eid (string_of_change change)
  | Pre.CEnvIs (id, change) ->
    Printf.sprintf "CEnvIs (%S, %s)" id (string_of_change change)
  | Pre.ValIs (eid, value) ->
    Printf.sprintf "ValIs (%d, %s)" eid (string_of_value value)

let string_of_change_cond (same_if, diff_if) =
  Printf.sprintf
    "(same_if = %s, diff_if = %s)"
    (string_of_cond same_if)
    (string_of_cond diff_if)

let expect_change_cond name expected actual =
  if actual <> expected then
    failwith
      (Printf.sprintf
         "%s:\nexpected %s\n     got %s"
         name
         (string_of_change_cond expected)
         (string_of_change_cond actual))

let expect_change_cond_array name expected actual =
  if Array.length actual <> Array.length expected then
    failwith
      (Printf.sprintf
         "%s: expected %d conditions, got %d"
         name
         (Array.length expected)
         (Array.length actual));
  Array.iteri
    (fun eid expected_condition ->
      expect_change_cond
        (Printf.sprintf "%s eid %d" name eid)
        expected_condition
        actual.(eid))
    expected

let true_cond = Pre.Always

let cng eid change =
  Pre.Atom (Pre.CngIs (eid, change))

let cenv id change =
  Pre.Atom (Pre.CEnvIs (id, change))

let value eid previous =
  Pre.Atom (Pre.ValIs (eid, previous))

let all = function
  | [] -> true_cond
  | condition :: conditions ->
    List.fold_left
      (fun conjunction condition -> Pre.And (conjunction, condition))
      condition
      conditions

let any = function
  | [] -> Pre.Impossible
  | condition :: conditions ->
    List.fold_left
      (fun disjunction condition -> Pre.Or (disjunction, condition))
      condition
      conditions

let one_changed left right =
  any
    [ all [ cng left Pre.Same; cng right Pre.Diff ];
      all [ cng left Pre.Diff; cng right Pre.Same ] ]

let literal_change =
  (true_cond, Pre.Impossible)

let additive_change left right =
  (all [ cng left Pre.Same; cng right Pre.Same ],
   one_changed left right)

let stable_binary_change left right =
  (all [ cng left Pre.Same; cng right Pre.Same ],
   Pre.Impossible)

let if_change condition if_true if_false =
  let was_true = value condition (Pre.Bool true) in
  let was_false = value condition (Pre.Bool false) in
  let was_missing = Pre.Not (any [ was_true; was_false ]) in
  ( any
      [ all
          [ was_missing;
            cng condition Pre.Same;
            cng if_true Pre.Same;
            cng if_false Pre.Same ];
        all [ was_true; cng condition Pre.Same; cng if_true Pre.Same ];
        all [ was_false; cng condition Pre.Same; cng if_false Pre.Same ] ],
    any
      [ all [ was_true; cng condition Pre.Same; cng if_true Pre.Diff ];
        all [ was_false; cng condition Pre.Same; cng if_false Pre.Diff ] ] )

let compile program =
  program
  |> Pre.from_pre2
  |> Pre.compile_change_conds

let root_change program =
  let domains = compile program in
  let params, conditions = domains.(Pre.root_fid) in
  if params <> [] then failwith "root domain unexpectedly has parameters";
  conditions.(Pre.root_eid)

let expect_root name expected program =
  root_change program |> expect_change_cond name expected

let test_expression_conditions () =
  List.iter
    (fun (name, expression) ->
      expect_root name literal_change expression)
    [ ("NUM", Pre2.NUM 7);
      ("TRUE", Pre2.TRUE);
      ("FALSE", Pre2.FALSE) ];
  expect_root
    "VAR"
    (cenv "x" Pre.Same, cenv "x" Pre.Diff)
    (Pre2.VAR "x");
  expect_root
    "ADD"
    (additive_change 1 2)
    (Pre2.ADD (Pre2.NUM 1, Pre2.NUM 2));
  expect_root
    "SUB"
    (additive_change 1 2)
    (Pre2.SUB (Pre2.NUM 1, Pre2.NUM 2));
  expect_root
    "MUL"
    (stable_binary_change 1 2)
    (Pre2.MUL (Pre2.NUM 1, Pre2.NUM 2));
  expect_root
    "MOD"
    (stable_binary_change 1 2)
    (Pre2.MOD (Pre2.NUM 1, Pre2.NUM 2));
  expect_root
    "LESS"
    (stable_binary_change 1 2)
    (Pre2.LESS (Pre2.NUM 1, Pre2.NUM 2));
  expect_root
    "DIV"
    ( all [ cng 1 Pre.Same; cng 2 Pre.Same ],
      all [ cng 1 Pre.Diff; cng 2 Pre.Same; value 2 (Pre.Num 1) ] )
    (Pre2.DIV (Pre2.NUM 8, Pre2.NUM 1));
  expect_root
    "EQUAL"
    ( all [ cng 1 Pre.Same; cng 2 Pre.Same ],
      all [ one_changed 1 2; value 0 (Pre.Bool true) ] )
    (Pre2.EQUAL (Pre2.NUM 1, Pre2.NUM 1));
  expect_root
    "NOT"
    (cng 1 Pre.Same, cng 1 Pre.Diff)
    (Pre2.NOT Pre2.TRUE);
  expect_root
    "IF"
    (if_change 1 2 3)
    (Pre2.IF (Pre2.TRUE, Pre2.NUM 1, Pre2.NUM 0));
  expect_root
    "CALL without arguments"
    (cenv "f" Pre.Same, Pre.Impossible)
    (Pre2.CALL ("f", []));
  expect_root
    "CALL with arguments"
    ( all
        [ cenv "f" Pre.Same;
          cenv "x" Pre.Same;
          cenv "y" Pre.Same ],
      Pre.Impossible )
    (Pre2.CALL ("f", [ "x"; "y" ]));
  expect_root
    "LET"
    (cng 2 Pre.Same, cng 2 Pre.Diff)
    (Pre2.LET ("x", Pre2.NUM 1, Pre2.VAR "x"));
  expect_root
    "LETFN"
    (cng 1 Pre.Same, cng 1 Pre.Diff)
    (Pre2.LETFN ("f", [], Pre2.NUM 1, Pre2.VAR "f"))

let test_immediate_child_references () =
  let domains =
    compile
      (Pre2.ADD
         ( Pre2.ADD (Pre2.VAR "x", Pre2.NUM 1),
           Pre2.SUB (Pre2.VAR "y", Pre2.NUM 2) ))
  in
  if Array.length domains <> 1 then
    failwith "nested arithmetic unexpectedly created another domain";
  let _, conditions = domains.(Pre.root_fid) in
  expect_change_cond_array
    "nested immediate-child conditions"
    [| additive_change 1 4;
       additive_change 2 3;
       (cenv "x" Pre.Same, cenv "x" Pre.Diff);
       literal_change;
       additive_change 5 6;
       (cenv "y" Pre.Same, cenv "y" Pre.Diff);
       literal_change |]
    conditions

let expect_domain
    domains fid expected_params expected_conditions =
  if fid < 0 || fid >= Array.length domains then
    failwith (Printf.sprintf "missing condition domain fid %d" fid);
  let params, conditions = domains.(fid) in
  if params <> expected_params then
    failwith
      (Printf.sprintf "fid %d has unexpected parameters" fid);
  expect_change_cond_array
    (Printf.sprintf "fid %d" fid)
    expected_conditions
    conditions

let test_letfn_domain_separation () =
  let domains =
    compile
      (Pre2.LETFN
         ( "f",
           [ "p" ],
           Pre2.ADD (Pre2.VAR "p", Pre2.VAR "captured"),
           Pre2.VAR "f" ))
  in
  if Array.length domains <> 2 then
    failwith "expected distinct root and function condition domains";
  expect_domain
    domains
    Pre.root_fid
    []
    [| (cng 1 Pre.Same, cng 1 Pre.Diff);
       (cenv "f" Pre.Same, cenv "f" Pre.Diff) |];
  expect_domain
    domains
    1
    [ (3, "p") ]
    [| additive_change 1 2;
       (cenv "p" Pre.Same, cenv "p" Pre.Diff);
       (cenv "captured" Pre.Same, cenv "captured" Pre.Diff) |]

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

let check_domain_fixture () =
  let annotated = Pre.from_pre2 domain_fixture in
  let domains = Pre.compile_change_conds annotated in
  if Array.length domains <> 4 then
    failwith "expected root and three function condition domains";
  expect_domain
    domains
    Pre.root_fid
    []
    [| (cng 1 Pre.Same, cng 1 Pre.Diff);
       if_change 2 3 4;
       literal_change;
       literal_change;
       (cng 5 Pre.Same, cng 5 Pre.Diff);
       literal_change |];
  expect_domain
    domains
    1
    [ (2, "x") ]
    [| (cng 1 Pre.Same, cng 1 Pre.Diff);
       (cenv "x" Pre.Same, cenv "x" Pre.Diff) |];
  expect_domain
    domains
    2
    [ (3, "y") ]
    [| additive_change 1 2;
       (cenv "x" Pre.Same, cenv "x" Pre.Diff);
       (cenv "y" Pre.Same, cenv "y" Pre.Diff) |];
  expect_domain domains 3 [] [| literal_change |]

let test_domain_layout_and_fid_reset () =
  check_domain_fixture ();
  (* Function ids must restart at one for every annotated program. *)
  check_domain_fixture ()

let test_parameter_eids () =
  let domains =
    compile
      (Pre2.LETFN
         ( "pair",
           [ "x"; "x" ],
           Pre2.ADD (Pre2.VAR "x", Pre2.VAR "x"),
           Pre2.NUM 0 ))
  in
  let params, conditions = domains.(1) in
  if params <> [ (3, "x"); (4, "x") ] then
    failwith "parameter occurrences do not have dense, distinct trailing eids";
  if Array.length conditions <> 3 then
    failwith "parameters unexpectedly occupy expression-condition slots"

let () =
  test_expression_conditions ();
  test_immediate_child_references ();
  test_letfn_domain_separation ();
  test_domain_layout_and_fid_reset ();
  test_parameter_eids ()
