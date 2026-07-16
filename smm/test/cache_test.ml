open Smm_

module Cache = Smm.Cache

let string_of_option = function
  | Some value -> "Some " ^ string_of_int value
  | None -> "None"

let expect_lookup name expected cache key =
  let actual = Cache.lookup cache key in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "%s: key %d expected %s, got %s"
         name key (string_of_option expected) (string_of_option actual))

let expect_invalid_key name operation =
  match operation () with
  | _ -> failwith (name ^ ": expected Invalid_argument")
  | exception Invalid_argument _ -> ()
  | exception exn ->
    failwith
      (Printf.sprintf
         "%s: expected Invalid_argument, got %s"
         name
         (Printexc.to_string exn))

let test_empty_isolation () =
  let first = Cache.bind Cache.empty 0 1 in
  expect_lookup "bound empty" (Some 1) first 0;
  expect_lookup "shared empty after bind" None Cache.empty 0;
  let second = Cache.bind Cache.empty 1 2 in
  expect_lookup "first isolated from second" None first 1;
  expect_lookup "second isolated from first" None second 0;
  expect_lookup "second bound empty" (Some 2) second 1

let test_fresh_arrays_are_independent () =
  let first = Cache.create () in
  let second = Cache.create () in
  Cache.bind first 0 10 |> ignore;
  expect_lookup "fresh first" (Some 10) first 0;
  expect_lookup "fresh second" None second 0;
  Cache.bind second 1 20 |> ignore;
  expect_lookup "fresh second own value" (Some 20) second 1;
  expect_lookup "fresh first reverse isolation" None first 1

let test_bind_replaces_in_place () =
  let cache = Cache.create () in
  let alias = cache in
  Cache.bind cache 0 1 |> ignore;
  Cache.bind cache 0 2 |> ignore;
  expect_lookup "replacement through original" (Some 2) cache 0;
  expect_lookup "replacement through alias" (Some 2) alias 0

let test_growth_preserves_shared_storage () =
  let cache = Cache.create () in
  let alias = cache in
  Cache.bind cache 0 10 |> ignore;
  Cache.bind cache 32 20 |> ignore;
  expect_lookup "value before growth" (Some 10) alias 0;
  expect_lookup "grown value through alias" (Some 20) alias 32;
  Cache.bind alias 48 30 |> ignore;
  expect_lookup "second growth through original" (Some 30) cache 48

let test_merge_precedence_and_non_aliasing () =
  let cache1 = Cache.create () in
  let cache2 = Cache.create () in
  Cache.bind cache1 0 1 |> ignore;
  Cache.bind cache1 2 10 |> ignore;
  Cache.bind cache2 1 2 |> ignore;
  Cache.bind cache2 2 20 |> ignore;
  let merged = Cache.merge cache1 cache2 in
  expect_lookup "merge cache1 value" (Some 1) merged 0;
  expect_lookup "merge cache2 value" (Some 2) merged 1;
  expect_lookup "merge cache1 precedence" (Some 10) merged 2;
  Cache.bind cache1 2 11 |> ignore;
  Cache.bind cache1 3 3 |> ignore;
  Cache.bind cache2 1 22 |> ignore;
  expect_lookup "merge isolated from cache1 replacement" (Some 10) merged 2;
  expect_lookup "merge isolated from cache1 addition" None merged 3;
  expect_lookup "merge isolated from cache2 replacement" (Some 2) merged 1;
  Cache.bind merged 2 30 |> ignore;
  Cache.bind merged 4 4 |> ignore;
  expect_lookup "merge replacement" (Some 30) merged 2;
  expect_lookup "merge addition" (Some 4) merged 4;
  expect_lookup "cache1 isolated from merge replacement" (Some 11) cache1 2;
  expect_lookup "cache2 isolated from merge replacement" (Some 20) cache2 2;
  expect_lookup "cache1 isolated from merge addition" None cache1 4;
  expect_lookup "cache2 isolated from merge addition" None cache2 4

let test_merge_with_empty () =
  let source = Cache.create () in
  Cache.bind source 0 1 |> ignore;
  let empty_first = Cache.merge Cache.empty source in
  let empty_second = Cache.merge source Cache.empty in
  let both_empty = Cache.merge Cache.empty Cache.empty in
  expect_lookup "merge empty first" (Some 1) empty_first 0;
  expect_lookup "merge empty second" (Some 1) empty_second 0;
  expect_lookup "merge both empty" None both_empty 0;
  Cache.bind source 0 2 |> ignore;
  expect_lookup "merge empty first non-aliasing" (Some 1) empty_first 0;
  expect_lookup "merge empty second non-aliasing" (Some 1) empty_second 0;
  Cache.bind both_empty 1 3 |> ignore;
  expect_lookup "merge both empty is mutable" (Some 3) both_empty 1;
  expect_lookup "merge both empty leaves sentinel isolated" None Cache.empty 1

let test_negative_keys_are_rejected () =
  expect_invalid_key
    "negative lookup"
    (fun () -> Cache.lookup Cache.empty (-1));
  expect_invalid_key
    "negative bind"
    (fun () -> Cache.bind Cache.empty (-1) 1)

let () =
  test_empty_isolation ();
  test_fresh_arrays_are_independent ();
  test_bind_replaces_in_place ();
  test_growth_preserves_shared_storage ();
  test_merge_precedence_and_non_aliasing ();
  test_merge_with_empty ();
  test_negative_keys_are_rejected ()
