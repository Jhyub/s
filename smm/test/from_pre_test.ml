open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Flat = Smm.Smm

let expect_domain name program fid expected_params expected_length =
  let params, expressions = program.(fid) in
  if params <> expected_params then
    failwith (name ^ ": unexpected parameters");
  if Array.length expressions <> expected_length then
    failwith
      (Printf.sprintf
         "%s: expected %d expressions, got %d"
         name
         expected_length
         (Array.length expressions));
  Array.iteri
    (fun expected_eid (eid, _, _, etype, _) ->
      if eid <> expected_eid then
        failwith
          (Printf.sprintf
             "%s: expected eid %d, got %d"
             name
             expected_eid
             eid);
      match etype with
      | Flat.Normal -> ()
      | Flat.SavPnt _ | Flat.CmpPnt _ ->
        failwith
          (Printf.sprintf
             "%s: eid %d does not have the Normal etype"
             name
             eid))
    expressions

let expect_body name program fid eid expected =
  let _, expressions = program.(fid) in
  let _, _, _, _, actual = expressions.(eid) in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "%s: fid %d eid %d has an unexpected body"
         name
         fid
         eid)

let expect_parents name program fid expected =
  let _, expressions = program.(fid) in
  if Array.length expressions <> Array.length expected then
    failwith (name ^ ": unexpected parent array length");
  Array.iteri
    (fun eid expected_parent_eid ->
      let _, parent_eid, _, _, _ = expressions.(eid) in
      if parent_eid <> expected_parent_eid then
        failwith
          (Printf.sprintf
             "%s: fid %d eid %d expected parent eid %d, got %d"
             name
             fid
             eid
             expected_parent_eid
             parent_eid))
    expected

let flatten program =
  program
  |> Pre.from_pre2
  |> Flat.from_pre

let test_flat_expression_references () =
  let program =
    flatten
      (Pre2.LET
         ( "x",
           Pre2.ADD (Pre2.NUM 1, Pre2.NUM 2),
           Pre2.IF
             ( Pre2.TRUE,
               Pre2.NOT Pre2.FALSE,
               Pre2.CALL ("f", [ "x" ]) ) ))
  in
  if Array.length program <> 1 then
    failwith "flat expression: expected only the root domain";
  expect_domain "flat expression" program Pre.root_fid [] 9;
  expect_parents
    "flat expression"
    program
    Pre.root_fid
    [| 0; 0; 1; 1; 0; 4; 4; 6; 4 |];
  expect_body "flat expression" program 0 0 (Flat.LET ("x", 1, 4));
  expect_body "flat expression" program 0 1 (Flat.ADD (2, 3));
  expect_body "flat expression" program 0 2 (Flat.NUM 1);
  expect_body "flat expression" program 0 3 (Flat.NUM 2);
  expect_body "flat expression" program 0 4 (Flat.IF (5, 6, 8));
  expect_body "flat expression" program 0 5 Flat.TRUE;
  expect_body "flat expression" program 0 6 (Flat.NOT 7);
  expect_body "flat expression" program 0 7 Flat.FALSE;
  expect_body
    "flat expression"
    program
    0
    8
    (Flat.CALL ("f", [ "x" ]))

let nested_functions =
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

let test_function_domains () =
  let program = flatten nested_functions in
  if Array.length program <> 4 then
    failwith "function domains: expected four fid domains";
  expect_domain "root domain" program 0 [] 6;
  expect_domain "outer domain" program 1 [ (2, "x") ] 2;
  expect_domain "inner domain" program 2 [ (3, "y") ] 3;
  expect_domain "dormant domain" program 3 [] 1;
  expect_parents "root domain" program 0 [| 0; 0; 1; 1; 1; 4 |];
  expect_parents "outer domain" program 1 [| 0; 0 |];
  expect_parents "inner domain" program 2 [| 0; 0; 0 |];
  expect_parents "dormant domain" program 3 [| 0 |];
  expect_body
    "root domain"
    program
    0
    0
    (Flat.LETFN (1, "outer", [ (2, "x") ], 0, 1));
  expect_body "root domain" program 0 1 (Flat.IF (2, 3, 4));
  expect_body "root domain" program 0 4
    (Flat.LETFN (3, "dormant", [], 0, 5));
  expect_body
    "outer domain"
    program
    1
    0
    (Flat.LETFN (2, "inner", [ (3, "y") ], 0, 1));
  expect_body "outer domain" program 1 1 (Flat.VAR "x");
  expect_body "inner domain" program 2 0 (Flat.ADD (1, 2));
  expect_body "inner domain" program 2 1 (Flat.VAR "x");
  expect_body "inner domain" program 2 2 (Flat.VAR "y");
  expect_body "dormant domain" program 3 0 (Flat.NUM 1)

let () =
  test_flat_expression_references ();
  test_function_domains ()
