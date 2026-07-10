open Smm_

module Pre2 = Smm.Smm_pre2
module Pre = Smm.Smm_pre
module Optimized = Smm.Smm

let program () =
  Pre2.NUM 1
  |> Pre.from_pre2
  |> Optimized.from_pre

let () =
  match Array.to_list Sys.argv with
  | [ _; "default" ] ->
    Optimized.eval (program ()) |> ignore
  | [ _; "quiet" ] ->
    Optimized.eval ~debug:false (program ()) |> ignore
  | _ ->
    prerr_endline "Usage: debug_mode (default|quiet)";
    exit 2
