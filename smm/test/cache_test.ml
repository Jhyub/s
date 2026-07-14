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
         "%s: key %S expected %s, got %s"
         name key (string_of_option expected) (string_of_option actual))

let test_empty_isolation () =
  let first = Cache.bind Cache.empty "first" 1 in
  expect_lookup "bound empty" (Some 1) first "first";
  expect_lookup "shared empty after bind" None Cache.empty "first";
  let second = Cache.bind Cache.empty "second" 2 in
  expect_lookup "first isolated from second" None first "second";
  expect_lookup "second isolated from first" None second "first";
  expect_lookup "second bound empty" (Some 2) second "second"

let test_fresh_tables_are_independent () =
  let first = Cache.create () in
  let second = Cache.create () in
  Cache.bind first "key" 10 |> ignore;
  expect_lookup "fresh first" (Some 10) first "key";
  expect_lookup "fresh second" None second "key";
  Cache.bind second "other" 20 |> ignore;
  expect_lookup "fresh second own value" (Some 20) second "other";
  expect_lookup "fresh first reverse isolation" None first "other"

let test_bind_replaces_in_place () =
  let cache = Cache.create () in
  let alias = cache in
  Cache.bind cache "key" 1 |> ignore;
  Cache.bind cache "key" 2 |> ignore;
  expect_lookup "replacement through original" (Some 2) cache "key";
  expect_lookup "replacement through alias" (Some 2) alias "key"

let test_merge_precedence_and_non_aliasing () =
  let cache1 = Cache.create () in
  let cache2 = Cache.create () in
  Cache.bind cache1 "cache1" 1 |> ignore;
  Cache.bind cache1 "shared" 10 |> ignore;
  Cache.bind cache2 "cache2" 2 |> ignore;
  Cache.bind cache2 "shared" 20 |> ignore;
  let merged = Cache.merge cache1 cache2 in
  expect_lookup "merge cache1 value" (Some 1) merged "cache1";
  expect_lookup "merge cache2 value" (Some 2) merged "cache2";
  expect_lookup "merge cache1 precedence" (Some 10) merged "shared";
  Cache.bind cache1 "shared" 11 |> ignore;
  Cache.bind cache1 "later-cache1" 3 |> ignore;
  Cache.bind cache2 "cache2" 22 |> ignore;
  expect_lookup "merge isolated from cache1 replacement" (Some 10) merged "shared";
  expect_lookup "merge isolated from cache1 addition" None merged "later-cache1";
  expect_lookup "merge isolated from cache2 replacement" (Some 2) merged "cache2";
  Cache.bind merged "shared" 30 |> ignore;
  Cache.bind merged "later-merged" 4 |> ignore;
  expect_lookup "merge replacement" (Some 30) merged "shared";
  expect_lookup "merge addition" (Some 4) merged "later-merged";
  expect_lookup "cache1 isolated from merge replacement" (Some 11) cache1 "shared";
  expect_lookup "cache2 isolated from merge replacement" (Some 20) cache2 "shared";
  expect_lookup "cache1 isolated from merge addition" None cache1 "later-merged";
  expect_lookup "cache2 isolated from merge addition" None cache2 "later-merged"

let test_merge_with_empty () =
  let source = Cache.create () in
  Cache.bind source "source" 1 |> ignore;
  let empty_first = Cache.merge Cache.empty source in
  let empty_second = Cache.merge source Cache.empty in
  let both_empty = Cache.merge Cache.empty Cache.empty in
  expect_lookup "merge empty first" (Some 1) empty_first "source";
  expect_lookup "merge empty second" (Some 1) empty_second "source";
  expect_lookup "merge both empty" None both_empty "source";
  Cache.bind source "source" 2 |> ignore;
  expect_lookup "merge empty first non-aliasing" (Some 1) empty_first "source";
  expect_lookup "merge empty second non-aliasing" (Some 1) empty_second "source";
  Cache.bind both_empty "new" 3 |> ignore;
  expect_lookup "merge both empty is mutable" (Some 3) both_empty "new";
  expect_lookup "merge both empty leaves sentinel isolated" None Cache.empty "new"

let () =
  test_empty_isolation ();
  test_fresh_tables_are_independent ();
  test_bind_replaces_in_place ();
  test_merge_precedence_and_non_aliasing ();
  test_merge_with_empty ()
