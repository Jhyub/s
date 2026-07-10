module Loc = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

module Mem = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

module Env = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun _ -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

module Cache = struct
  type ('a, 'b) t = C of ('a -> 'b option)

  let empty = C (fun _ -> None)
  let lookup (C cache) key = cache key
  let bind (C cache) key value = C (fun k -> if k = key then Some value else cache k)
  let merge (C cache1) (C cache2) = C (fun k -> match (lookup (C cache1) k) with | Some v -> Some v | None -> lookup (C cache2) k)
end

module Smm_pre2 = struct
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | IF of exp * exp * exp (* if-then-else *)
    | CALL of id * id list
    | LET of id * exp * exp
    | LETFN of id * id list * exp * exp

  type program = exp
  type value = Num of int | Bool of bool
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and entry = Addr of Loc.t | Function of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

end

module Smm_pre = struct
  exception Error of string

  type id = string
  type eid = int
  type fid = int

  type exp = eid * ebody
  and ebody = 
    | NUM of int
    | TRUE
    | FALSE
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | IF of exp * exp * exp (* if-then-else *)
    | CALL of id * id list
    | LET of id * exp * exp
    | LETFN of fid * id * id list * exp * exp

  type program = exp
  type value = Num of int | Bool of bool (* | Pair of (value * value) *)
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and trace_key = Eid of eid | FnArg of (fid * id)
  and trace = (trace_key, value) Cache.t
  and entry = Addr of Loc.t | Function of fid * id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty
  let emptyTrace = Cache.empty

  let next_eid = ref 0
  let new_eid () =
    let eid = !next_eid in
    next_eid := eid + 1;
    eid

  let next_fid = ref 0
  let new_fid () =
    let fid = !next_fid in
    next_fid := fid + 1;
    fid

  let from_pre2 (e : Smm_pre2.exp) : exp =
    next_eid := 0;
    let rec annotate_pre e =
      let ne = new_eid () in
      match e with
      | Smm_pre2.NUM n -> (ne, NUM n)
      | Smm_pre2.TRUE -> (ne, TRUE)
      | Smm_pre2.FALSE -> (ne, FALSE)
      | Smm_pre2.VAR x -> (ne, VAR x)
      | Smm_pre2.ADD (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, ADD (e1', e2'))
      | Smm_pre2.SUB (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, SUB (e1', e2'))
      | Smm_pre2.MUL (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, MUL (e1', e2'))
      | Smm_pre2.DIV (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, DIV (e1', e2'))
      | Smm_pre2.MOD (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, MOD (e1', e2'))
      | Smm_pre2.EQUAL (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, EQUAL (e1', e2'))
      | Smm_pre2.LESS (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, LESS (e1', e2'))
      | Smm_pre2.NOT e ->
        let e' = annotate_pre e in
        (ne, NOT e')
      | Smm_pre2.IF (e1, e2, e3) ->
        let (e1', e2', e3') = annotate_pre e1, annotate_pre e2, annotate_pre e3 in
        (ne, IF (e1', e2', e3'))
      | Smm_pre2.CALL (f, ids) ->
        (ne, CALL (f, ids))
      | Smm_pre2.LET (x, e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, LET (x, e1', e2'))
      | Smm_pre2.LETFN (f, params, body, e1) ->
        let (body', e1') = annotate_pre body, annotate_pre e1 in
        (ne, LETFN (new_fid (), f, params, body', e1'))
    in
    annotate_pre e

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError: not bool")

  (* let value_pair v =
    match v with Pair p -> p | _ -> raise (Error "TypeError: not pair") *)

  let entry_addr entry =
    match entry with Addr l -> l | Function _ -> raise (Error "TypeError: not a value")

  let entry_function entry =
    match entry with
    | Function (fid, params, body, cenv) -> (fid, params, body, cenv)
    | Addr _ -> raise (Error "TypeError: not a function")

  let validate_call_argument = function
    | Addr _ -> ()
    | Function _ ->
      raise (Error "TypeError: function arguments are not supported")

  let eq v1 v2 =
    match v1, v2 with
    | Num n1, Num n2 -> n1 = n2
    | Bool b1, Bool b2 -> b1 = b2
    | _ -> false

  let rec eval mem env e =
    let (eid, e') = e in
    match e' with
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | VAR x -> (Mem.load mem (entry_addr (Env.lookup env x)), mem)
    | ADD (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (value_int v1 + value_int v2), mem2)
    | SUB (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (value_int v1 - value_int v2), mem2)
    | MUL (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (value_int v1 * value_int v2), mem2)
    | DIV (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (value_int v1 / value_int v2), mem2)
    | MOD (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (value_int v1 mod value_int v2), mem2)
    | EQUAL (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Bool (eq v1 v2), mem2)
    | LESS (e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Bool (value_int v1 < value_int v2), mem2)
    | NOT (e) ->
      let (v, mem1) = eval mem env e in
      (Bool (not (value_bool v)), mem1)
    | IF (e1, e2, e3) ->
      let (v, mem1) = eval mem env e1 in
      if value_bool v then eval mem1 env e2 else eval mem1 env e3
    | CALL (f, ids) ->
      let (fid, params, body, env') = entry_function (Env.lookup env f) in
      let entries = ids |> List.map (Env.lookup env) in
      let env'' =
        match List.combine params entries with
        | bindings ->
          List.iter
            (fun (_, entry) -> validate_call_argument entry)
            bindings;
          List.fold_left (fun env'' (param, entry) -> Env.bind env'' param entry) env' bindings
        | exception Invalid_argument _ -> raise (Error "TypeError: wrong number of arguments")
      in
      eval mem env'' body
    | LET (x, e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let l, mem2 = Mem.alloc mem1 in
      let mem3 = Mem.store mem2 l v1 in
      let env' = Env.bind env x (Addr l) in
      eval mem3 env' e2
    | LETFN (fid, f, params, body, e1) ->
      let env' = Env.bind env f (Function (fid, params, body, env)) in
      eval mem env' e1

  let run pgm = fst (eval emptyMemory emptyEnv pgm)

  (* Same: value is ensured to be the same *)
  (* Unknown: the interpreter might perform equality checks to mark it as Same or Diff on runtime *)
  (* Diff: value is trusted to be different, hence re-evaluating for all steps *)
  (* Note: it is sound to mark anything as Unknown *)
  (* Question: should we make it sound to mark anything as Diff? *)
  type change = Same | Diff | Unknown
  and change_env = (id, change_entry) Env.t
  and change_trace = (eid, change_entry) Cache.t
  and change_fn = (trace * change_env * change_trace) -> change_entry
  and change_entry = Value of change | Function of (change * (fid * id list * change_fn * change_env))
  let emptyChangeEnv = Env.empty

  let cent_value entry =
    match entry with | Value v -> v | Function _ -> raise (Error "TypeError: not a value")

  let cent_function entry =
    match entry with | Function f -> f | Value _ -> raise (Error "TypeError: not a function")
  
  let cent_change entry =
    match entry with | Value c -> c | Function (c, _) -> c

  let rec keep_unique l =
    match l with
    | x :: xs ->
      let ret = keep_unique xs in
      if List.mem x ret then ret else x :: ret
    | [] -> []

  let rec free_vars exclude e =
    let (eid, e') = e in
    begin
      match e' with
      | NUM _ | TRUE | FALSE -> []
      | VAR x -> if List.mem x exclude then [] else [x]
      | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | EQUAL (e1, e2) | LESS (e1, e2) ->
        free_vars exclude e1 @ free_vars exclude e2
      | NOT e -> free_vars exclude e
      | IF (e1, e2, e3) -> free_vars exclude e1 @ free_vars exclude e2 @ free_vars exclude e3
      | LET (x, e1, e2) -> free_vars exclude e1 @ free_vars (x :: exclude) e2
      | LETFN (fid, f, params, body, e1) -> free_vars (params @ exclude) body @ free_vars (f :: exclude) e1
      | CALL (f, ids) -> List.fold_left (fun ret id -> if List.mem id exclude then ret else id :: ret) [] (f :: ids)
    end |> keep_unique
    
  let rec eval_change (e: exp): change_fn =
    let (eid, e') = e in
    (* Previous (Value) Trace, Change Environment, Change Trace *)
    fun pcc -> begin
      let (ptrace, cenv, ctrace) = pcc in
      match Cache.lookup ctrace eid with
      | Some entry -> entry
      | None ->
        begin
          match e' with
          | NUM _ | TRUE | FALSE -> Value (Same)
          | VAR x -> Env.lookup cenv x
          | ADD (e1, e2) | SUB (e1, e2) ->
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | Same, Diff | Diff, Same -> Value (Diff)
              | _, _ -> Value (Unknown)
            end
          | MUL (e1, e2) ->
            (* let (eid1, _) = e1 in *)
            (* let (eid2, _) = e2 in *)
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              (* | Same, Diff | Same, Unknown -> begin
                match Cache.lookup ptrace (Eid eid1) with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce2)
              end
              | Diff, Same | Unknown, Same -> begin
                match Cache.lookup ptrace (Eid eid2) with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce1)
              end *)
              | _, _ -> Value (Unknown)
            end
          | DIV (e1, e2) ->
            (* let (eid1, _) = e1 in *)
            let (eid2, _) = e2 in
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              (* | Same, _ -> begin
                match Cache.lookup ptrace (Eid eid1) with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce2)
              end  *)
              | Diff, Same | Unknown, Same -> begin
                match Cache.lookup ptrace (Eid eid2) with
                | Some (Num 1) -> Value (ce1)
                | _ -> Value (Unknown)
              end
              | _, _ -> Value (Unknown)
            end
          | MOD (e1, e2) ->
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | _, _ -> Value (Unknown)
            end
          | EQUAL (e1, e2) ->
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | Same, Diff | Diff, Same -> begin
                match Cache.lookup ptrace (Eid eid) with
                | Some (Bool true) -> Value (Diff)
                | _ -> Value (Unknown)
              end
              | _, _ -> Value (Unknown)
            end
          | LESS (e1, e2) ->
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | _, _ -> Value (Unknown)
            end
          | NOT e -> eval_change e pcc
          | IF (e1, e2, e3) ->
            let (eid1, _), (eid2, _), (eid3, _) = e1, e2, e3 in
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            let ce3 = eval_change e3 pcc |> cent_value in
            begin
              match Cache.lookup ptrace (Eid eid1) with
              | None -> begin
                match ce1, ce2, ce3 with
                | Same, Same, Same -> Value (Same)
                | _, _, _ -> Value (Unknown)
              end
              | Some (Bool b) -> begin
                match ce1 with
                | Same -> if b then Value (ce2) else Value (ce3)
                | Diff | Unknown -> Value (Unknown)
              end
              | _ -> Value (Unknown) (* Should not happen *)
            end
          | LET (x, e1, e2) ->
            let ce1 = eval_change e1 pcc in
            let cenv' = Env.bind cenv x ce1 in
            eval_change e2 (ptrace, cenv', ctrace)
          | LETFN (fid, f, params, body, e1) ->
            let body_fvs = free_vars params body in
            let aux change id = begin
              let c = Env.lookup cenv id |> cent_change in
              match change, c with
              | Same, Same -> Same
              | _, Diff -> Diff
              | _, _ -> Unknown
            end in
            let lit_change = List.fold_left aux Same body_fvs in
            let cenv' = Env.bind cenv f (Function (lit_change, (fid, params, eval_change body, cenv))) in
            eval_change e1 (ptrace, cenv', ctrace)
          | CALL (f, ids) ->
            let aux change id = begin
              let c = Env.lookup cenv id |> cent_change in
              match change, c with
              | Same, Same -> Same
              | _, _ -> Unknown
            end in
            Value (List.fold_left aux Same (f :: ids))
      end
    end

end

module Smm = struct
  exception Error of string

  let reject_function_argument () =
    raise (Error "TypeError: function arguments are not supported")

  open Smm_pre

  type exp = eid * change_fn * ebody
  and ebody = 
    | NUM of int
    | TRUE
    | FALSE
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | IF of exp * exp * exp (* if-then-else *)
    | CALL of id * id list
    | LET of id * exp * exp
    | LETFN of fid * id * id list * exp * exp

  type env = (id, entry) Env.t
  and entry = Addr of Loc.t | Function of fid * id list * exp * env

  let string_of_ebody_type ebody =
    match ebody with
    | NUM _ -> "NUM"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | VAR _ -> "VAR"
    | ADD _ -> "ADD"
    | SUB _ -> "SUB"
    | MUL _ -> "MUL"
    | DIV _ -> "DIV"
    | MOD _ -> "MOD"
    | EQUAL _ -> "EQUAL"
    | LESS _ -> "LESS"
    | NOT _ -> "NOT"
    | IF _ -> "IF"
    | CALL _ -> "CALL"
    | LET _ -> "LET"
    | LETFN _ -> "LETFN"

  let debug_same_hit eid ebody =
    prerr_string
      ("[Smm.eval] Same hit: eid="
       ^ string_of_int eid
       ^ " expr="
       ^ string_of_ebody_type ebody
       ^ "\n");
    flush stderr

  let debug_reuse_hit eid ebody =
    prerr_string
      ("[Smm.eval] Reuse hit: eid="
       ^ string_of_int eid
       ^ " expr="
       ^ string_of_ebody_type ebody
       ^ "\n");
    flush stderr

  let entry_addr entry =
    match entry with Addr l -> l | Function _ -> raise (Error "TypeError: not a value")

  let entry_function entry =
    match entry with
    | Function (fid, params, body, cenv) -> (fid, params, body, cenv)
    | Addr _ -> raise (Error "TypeError: not a function")

  let validate_call_argument = function
    | Addr _ -> ()
    | Function _ -> reject_function_argument ()

  let rec free_vars exclude e =
    let (eid, _, e') = e in
    begin
      match e' with
      | NUM _ | TRUE | FALSE -> []
      | VAR x -> if List.mem x exclude then [] else [x]
      | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | EQUAL (e1, e2) | LESS (e1, e2) ->
        free_vars exclude e1 @ free_vars exclude e2
      | NOT e -> free_vars exclude e
      | IF (e1, e2, e3) -> free_vars exclude e1 @ free_vars exclude e2 @ free_vars exclude e3
      | LET (x, e1, e2) -> free_vars exclude e1 @ free_vars (x :: exclude) e2
      | LETFN (fid, f, params, body, e1) -> free_vars (params @ exclude) body @ free_vars (f :: exclude) e1
      | CALL (f, ids) -> List.fold_left (fun ret id -> if List.mem id exclude then ret else id :: ret) [] (f :: ids)
    end |> keep_unique

  let rec from_pre (e: Smm_pre.exp): exp =
    let (eid, e') = e in
    match e' with
    | Smm_pre.NUM n -> (eid, Smm_pre.eval_change e, NUM n)
    | Smm_pre.TRUE -> (eid, Smm_pre.eval_change e, TRUE)
    | Smm_pre.FALSE -> (eid, Smm_pre.eval_change e, FALSE)
    | Smm_pre.VAR x -> (eid, Smm_pre.eval_change e, VAR x)
    | Smm_pre.ADD (e1, e2) -> (eid, Smm_pre.eval_change e, ADD (from_pre e1, from_pre e2))
    | Smm_pre.SUB (e1, e2) -> (eid, Smm_pre.eval_change e, SUB (from_pre e1, from_pre e2))
    | Smm_pre.MUL (e1, e2) -> (eid, Smm_pre.eval_change e, MUL (from_pre e1, from_pre e2))
    | Smm_pre.DIV (e1, e2) -> (eid, Smm_pre.eval_change e, DIV (from_pre e1, from_pre e2))
    | Smm_pre.MOD (e1, e2) -> (eid, Smm_pre.eval_change e, MOD (from_pre e1, from_pre e2))
    | Smm_pre.EQUAL (e1, e2) -> (eid, Smm_pre.eval_change e, EQUAL (from_pre e1, from_pre e2))
    | Smm_pre.LESS (e1, e2) -> (eid, Smm_pre.eval_change e, LESS (from_pre e1, from_pre e2))
    | Smm_pre.NOT e -> (eid, Smm_pre.eval_change e, NOT (from_pre e))
    | Smm_pre.IF (e1, e2, e3) -> (eid, Smm_pre.eval_change e, IF (from_pre e1, from_pre e2, from_pre e3))
    | Smm_pre.CALL (f, ids) -> (eid, Smm_pre.eval_change e, CALL (f, ids))
    | Smm_pre.LET (x, e1, e2) -> (eid, Smm_pre.eval_change e, LET (x, from_pre e1, from_pre e2))
    | Smm_pre.LETFN (fid, f, params, body, e1) -> (eid, Smm_pre.eval_change e, LETFN (fid, f, params, from_pre body, from_pre e1))
  
  (* ptrace를 무지성으로 계속 업데이트하면서 돌려도 괜찮나? 검토 필요 *)
  (*
    mem: the memory we always use
    env: the environment we always use
    ptrace: previous trace for the current evaluation domain. Function bodies use the trace from their most recent completed evaluation.
    cenv: change environment, describes the change of values compared to the "previous" trace.
    e: the expression we are evaluating

    ptrace와 cenv는 항상 change_fn 호출 시에 같이 다녀야 하고,
    cenv가 ptrace와 sync 깨지는 건 LET / CALL에서 항상 해결
    change_fn 호출 시 ctrace 사용은 한 레벨 아래 쪽만이므로
    ctrace를 사실 아래에서부터 build-up 해갈 필요는 없음? <- 아닌 듯
    ctrace에는 Same/Diff만 남겨야 유의미 (할 것으로 보임)


    value: the value of the expression
    memory: updated memory
    trace: the trace produced by the current traversal
    change_trace: the change trace we are building & using to provide additional change information on the fly
  *)
  type eval_state = {
    function_traces : (fid, trace) Hashtbl.t;
    mutable active_trace : trace option;
  }

  let rec eval_with_state state (mem: memory) (env: env) (ptrace: trace) (cenv: change_env) (ctrace: change_trace) (e: exp): (value * memory * trace * change_trace) =
    let (eid, change_fn, e') = e in
    begin
      match e' with
      | CALL (_, ids) ->
        List.iter
          (fun id -> Env.lookup env id |> validate_call_argument)
          ids
      | _ -> ()
    end;
    let result =
      let change = change_fn (ptrace, cenv, emptyTrace) |> cent_change in
      let early_return = begin
        match change with
        | Same ->
          debug_same_hit eid e';
          Cache.lookup ptrace (Eid eid)
        | Unknown -> None
        | Diff -> None
      end in
      match early_return with
      | Some ret ->
        debug_reuse_hit eid e';
        (ret, mem, Cache.bind ptrace (Eid eid) ret, emptyTrace)
      | None -> begin
      (* Literals *)
      let aux x = (x, mem, Cache.bind ptrace (Eid eid) x, emptyTrace) in
      (* We re-evaluate the change with additional information from new ctrace, *)
      (* and propagate our change information accordingly, *)
      (* only if we need to do (overriding 'Unknown's.) *)
      (* Do not override with 'Unknown' in ctrace. *)
      let aux' change_fn cenv v mem trace ctrace = begin
        let change' = change_fn (ptrace, cenv, ctrace) |> cent_change in
        let trace' = Cache.bind trace (Eid eid) v in
        match change' with
        | Same ->
          debug_same_hit eid e';
          let v = begin
            match Cache.lookup ptrace (Eid eid) with
            | Some v -> debug_reuse_hit eid e'; v
            | None -> v
          end in
          (v, mem, trace', ctrace)
        | Diff ->
          (v, mem, trace', ctrace)
        | Unknown -> begin
          match Cache.lookup ptrace (Eid eid) with
          | Some v' -> (v, mem, trace', Cache.bind ctrace eid (Value (if eq v v' then Same else Diff)))
          | None -> (v, mem, trace', ctrace)
        end
      end in
      let aux'' = aux' change_fn cenv in
      match e' with
      | NUM n -> aux (Num n)
      | TRUE -> aux (Bool true)
      | FALSE -> aux (Bool false)
      | VAR x ->
        let v = Mem.load mem (entry_addr (Env.lookup env x)) in
        (v, mem, Cache.bind ptrace (Eid eid) v, Cache.bind emptyTrace eid (Env.lookup cenv x))
      | ADD (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 + value_int v2)) mem2 trace2 ctrace2
      | SUB (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 - value_int v2)) mem2 trace2 ctrace2
      | MUL (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 * value_int v2)) mem2 trace2 ctrace2
      | DIV (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 / value_int v2)) mem2 trace2 ctrace2
      | MOD (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 mod value_int v2)) mem2 trace2 ctrace2
      | EQUAL (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Bool (eq v1 v2)) mem2 trace2 ctrace2
      | LESS (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
        aux'' (Bool (value_int v1 < value_int v2)) mem2 trace2 ctrace2
      | NOT e ->
        let (v, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e in
        aux'' (Bool (not (value_bool v))) mem1 trace1 ctrace1
      | IF (e1, e2, e3) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        if value_bool v1 then
          let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env trace1 cenv ctrace1 e2 in
          aux'' v2 mem2 trace2 ctrace2
        else
          let (v3, mem3, trace3, ctrace3) = eval_with_state state mem1 env trace1 cenv ctrace1 e3 in
          aux'' v3 mem3 trace3 ctrace3
      | CALL (f, ids) ->
        (* We do not run additional evaluations for 'call' itself, *)
        (* so early return is enough. *)
        let (fid, params, body, env') = Env.lookup env f |> entry_function in
        let (_, (_, _, _, cenv')) = Env.lookup cenv f |> cent_function in
        let fn_ptrace =
          match Hashtbl.find_opt state.function_traces fid with
          | Some trace -> trace
          | None -> emptyTrace
        in
        let arguments = List.combine ids params in
        (* Compute change for each argument *)
        (* CALL is a point of change propagation. <- is this inevitable? *)
        let aux''' (id, param) = begin
          let v_old = Cache.lookup fn_ptrace (FnArg (fid, param)) in
          let v = Mem.load mem (entry_addr (Env.lookup env id)) in
          match v_old with
          | Some v_old ->
            if eq v v_old then begin
              debug_same_hit eid e';
              Same
            end else Diff
          | None -> Unknown
        end in
        let aux'''' trace (id, param) = begin
          let v = Mem.load mem (entry_addr (Env.lookup env id)) in
          Cache.bind trace (FnArg (fid, param)) v
        end in
        let changes = List.map aux''' arguments in
        let changes' = List.combine params changes in
        let fresh_trace = List.fold_left aux'''' emptyTrace arguments in
        let cenv'' = List.fold_left (fun cenv (id, change) -> Env.bind cenv id (Value change)) cenv' changes' in
        let entries = ids |> List.map (Env.lookup env) in
        let env'' =
          begin
            match List.combine params entries with
            | bindings ->
              List.fold_left (fun env' (param, entry) -> Env.bind env' param entry) env' bindings
            | exception Invalid_argument _ -> raise (Error "TypeError: wrong number of arguments")
            end
        in
        let caller_trace = state.active_trace in
        let ((v, mem', _, _), completed_trace) =
          Fun.protect
            ~finally:(fun () -> state.active_trace <- caller_trace)
            (fun () ->
              state.active_trace <- Some fresh_trace;
              let result = eval_with_state state mem env'' fn_ptrace cenv'' emptyTrace body in
              let completed_trace =
                match state.active_trace with
                | Some trace -> trace
                | None -> assert false
              in
              (result, completed_trace))
        in
        Hashtbl.replace state.function_traces fid completed_trace;
        aux'' v mem' ptrace ctrace
      | LET (x, e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace cenv ctrace e1 in
        (* LET is also a point of change propagation.*)
        let (eid1, change_fn1, _) = e1 in 
        let change' = change_fn1 (ptrace, cenv, ctrace1) |> cent_change in
        let change'' = begin
          match change' with
          | Same ->
            debug_same_hit eid e';
            Same
          | Diff -> Diff
          | Unknown ->
            let v1_old = Cache.lookup ptrace (Eid eid1) in
            begin
              match v1_old with
              | Some v1_old -> if eq v1 v1_old then Same else Diff
              | None -> Unknown
            end
        end in
        let cenv' = Env.bind cenv x (Value change'') in
        let ctrace1' = begin
          match change'' with
          | Same | Diff -> Cache.bind ctrace1 eid1 (Value change'')
          | _ -> ctrace1
        end in
        let l, mem2 = Mem.alloc mem1 in
        let mem3 = Mem.store mem2 l v1 in
        let env' = Env.bind env x (Addr l) in
        let change' = change_fn (ptrace, cenv', ctrace1') |> cent_change in
        let v' = begin
          match change' with
          | Same ->
            debug_same_hit eid e';
            Cache.lookup ptrace (Eid eid)
          | Diff | Unknown -> None
        end in
        begin
          match v' with
          | Some v' ->
            debug_reuse_hit eid e';
            (v', mem1, Cache.bind trace1 (Eid eid) v', ctrace1')
          | None -> eval_with_state state mem3 env' trace1 cenv' ctrace1' e2 |> fun (v, mem, trace, ctrace) -> (v, mem, Cache.bind trace (Eid eid) v, ctrace)
        end
      | LETFN (fid, f, params, body, e1) ->
        (* We do not run additional evaluations for 'letfn' itself, *)
        (* so early return is enough. *)
        (* 왜 cenv maintain하는 코드가 둘 다에서 있는 것 같지? *)
        (* A dynamically created closure must not reuse a trace collected by
           an older activation of the same static function definition. *)
        Hashtbl.remove state.function_traces fid;
        let env' = Env.bind env f (Function (fid, params, body, env)) in
        let body_fvs = free_vars params body in
        let aux change id = begin
          let c = Env.lookup cenv id |> cent_change in
          match change, c with
          | Same, Same ->
            debug_same_hit eid e';
            Same
          | _, Diff -> Diff
          | _, _ -> Unknown
        end in
        let lit_change = List.fold_left aux Same body_fvs in
        let (eidb, cfnb, ebodyb) = body in
        let cenv' = Env.bind cenv f (Function (lit_change, (fid, params, cfnb, cenv))) in
        eval_with_state state mem env' ptrace cenv' ctrace e1 |> fun (v, mem, trace, ctrace) -> (v, mem, Cache.bind trace (Eid eid) v, ctrace)
      end
    in
    let (v, _, _, _) = result in
    begin
      match state.active_trace with
      | Some trace -> state.active_trace <- Some (Cache.bind trace (Eid eid) v)
      | None -> ()
    end;
    result

  let eval (e: exp): (value * memory * trace * change_trace) =
    let state = {
      function_traces = Hashtbl.create 16;
      active_trace = None;
    } in
    eval_with_state state emptyMemory Env.empty emptyTrace emptyChangeEnv Cache.empty e



end
