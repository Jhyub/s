open Smm_

let lower n =
  Smm.Smm_pre2.NUM n
  |> Smm.Smm_pre.from_pre2
  |> Smm.Smm.from_pre

let expect_num expected exp =
  match Smm.Smm.eval exp with
  | Smm.Smm_pre.Num actual, _, _, _ when actual = expected -> ()
  | Smm.Smm_pre.Num actual, _, _, _ ->
    failwith
      ("expected " ^ string_of_int expected ^ ", got " ^ string_of_int actual)
  | Smm.Smm_pre.Bool _, _, _, _ ->
    failwith "expected a number, got a boolean"

let () =
  (* Each lowering starts at Eid 0; separate eval calls must not share traces. *)
  expect_num 1 (lower 1);
  expect_num 2 (lower 2)
