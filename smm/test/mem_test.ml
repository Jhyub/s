open Smm_

module Loc = Smm.Loc
module Mem = Smm.Mem

let failf format = Printf.ksprintf failwith format

let expect_location name expected actual =
  if not (Loc.equal expected actual) then
    failf "%s: locations differ" name

let expect_load name expected memory location =
  match Mem.load memory location with
  | actual when actual = expected -> ()
  | actual -> failf "%s: expected %d, got %d" name expected actual
  | exception exn ->
    failf "%s: expected %d, got %s" name expected (Printexc.to_string exn)

let expect_not_allocated name operation =
  match operation () with
  | _ -> failf "%s: expected Mem.Not_allocated" name
  | exception Mem.Not_allocated -> ()
  | exception exn ->
    failf
      "%s: expected Mem.Not_allocated, got %s"
      name
      (Printexc.to_string exn)

let expect_not_initialized name operation =
  match operation () with
  | _ -> failf "%s: expected Mem.Not_initialized" name
  | exception Mem.Not_initialized -> ()
  | exception exn ->
    failf
      "%s: expected Mem.Not_initialized, got %s"
      name
      (Printexc.to_string exn)

let test_fresh_evaluation_isolation () =
  let first_location, first_memory = Mem.alloc Mem.empty in
  let first_memory = Mem.store first_memory first_location 11 in
  let second_location, second_memory = Mem.alloc Mem.empty in
  expect_location "fresh locations" first_location second_location;
  expect_not_initialized
    "second evaluation starts uninitialized"
    (fun () -> Mem.load second_memory second_location);
  let second_memory = Mem.store second_memory second_location 22 in
  expect_load "first evaluation stays isolated" 11 first_memory first_location;
  expect_load "second evaluation has its own value" 22 second_memory second_location

let test_sequential_allocation () =
  let first, memory = Mem.alloc Mem.empty in
  let second, memory = Mem.alloc memory in
  let third, memory = Mem.alloc memory in
  expect_location "first allocation" Loc.base first;
  expect_location "second allocation" (Loc.increase Loc.base 1) second;
  expect_location "third allocation" (Loc.increase Loc.base 2) third;
  let memory = Mem.store memory first 10 in
  let memory = Mem.store memory second 20 in
  let memory = Mem.store memory third 30 in
  expect_load "first sequential value" 10 memory first;
  expect_load "second sequential value" 20 memory second;
  expect_load "third sequential value" 30 memory third

let test_initialized_and_uninitialized_loads () =
  let location, memory = Mem.alloc Mem.empty in
  expect_not_initialized
    "allocated location is uninitialized"
    (fun () -> Mem.load memory location);
  let memory = Mem.store memory location 42 in
  expect_load "initialized location" 42 memory location

let test_store_replacement () =
  let location, memory = Mem.alloc Mem.empty in
  let memory = Mem.store memory location 1 in
  expect_load "initial stored value" 1 memory location;
  let alias = memory in
  let memory = Mem.store memory location 2 in
  expect_load "replacement value" 2 memory location;
  expect_load "replacement mutates shared storage" 2 alias location

let test_invalid_locations () =
  let below_base = Loc.increase Loc.base (-1) in
  let location, memory = Mem.alloc Mem.empty in
  let boundary = Loc.increase location 1 in
  let well_beyond_boundary = Loc.increase boundary 100 in
  let missing_memory =
    Mem.M (Loc.increase Loc.base 1, Mem.Table (Hashtbl.create 1))
  in
  expect_not_allocated
    "load from empty memory"
    (fun () -> Mem.load Mem.empty Loc.base);
  expect_not_allocated
    "store into empty memory"
    (fun () -> Mem.store Mem.empty Loc.base 1);
  expect_not_allocated
    "load below base"
    (fun () -> Mem.load memory below_base);
  expect_not_allocated
    "store below base"
    (fun () -> Mem.store memory below_base 1);
  expect_not_allocated
    "load at boundary"
    (fun () -> Mem.load memory boundary);
  expect_not_allocated
    "store at boundary"
    (fun () -> Mem.store memory boundary 1);
  expect_not_allocated
    "load beyond boundary"
    (fun () -> Mem.load memory well_beyond_boundary);
  expect_not_allocated
    "store beyond boundary"
    (fun () -> Mem.store memory well_beyond_boundary 1);
  expect_not_allocated
    "load missing allocated key"
    (fun () -> Mem.load missing_memory Loc.base);
  expect_not_allocated
    "store missing allocated key"
    (fun () -> Mem.store missing_memory Loc.base 1)

let test_older_boundary_protection () =
  let first, ancestor = Mem.alloc Mem.empty in
  let ancestor = Mem.store ancestor first 7 in
  let descendant_location, descendant = Mem.alloc ancestor in
  let descendant = Mem.store descendant descendant_location 9 in
  expect_load
    "descendant can read its allocation"
    9
    descendant
    descendant_location;
  expect_not_allocated
    "ancestor cannot load descendant allocation"
    (fun () -> Mem.load ancestor descendant_location);
  expect_not_allocated
    "ancestor cannot store into descendant allocation"
    (fun () -> Mem.store ancestor descendant_location 10);
  expect_load "ancestor retains its own allocation" 7 ancestor first;
  let reallocated_location, reallocated = Mem.alloc ancestor in
  expect_location
    "discarded descendant location is reused"
    descendant_location
    reallocated_location;
  expect_not_initialized
    "reused location is reset"
    (fun () -> Mem.load reallocated reallocated_location)

let () =
  test_fresh_evaluation_isolation ();
  test_sequential_allocation ();
  test_initialized_and_uninitialized_loads ();
  test_store_replacement ();
  test_invalid_locations ();
  test_older_boundary_protection ()
