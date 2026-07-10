(** Deterministic, generated S-- benchmark workloads. *)

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

val default_calls : int
val default_seed : int

(** [generate ~calls ~seed] builds all benchmark sources in memory.
    [calls] is the number of top-level calls whose results are summed in each
    workload. *)
val generate : calls:int -> seed:int -> t list

(** Generate the suite, create [output_dir] if necessary, write one S-- file
    per workload, and write [manifest.csv] in the same directory. *)
val write_suite : output_dir:string -> calls:int -> seed:int -> written list
