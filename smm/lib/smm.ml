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
end

module SmmPre = struct
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

module Smm = struct
  exception Error of string

  type id = string
  type eid = int

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
    | LETFN of id * id list * exp * exp

  type program = exp
  type value = Num of int | Bool of bool (* | Pair of (value * value) *)
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and trace = (eid, value) Cache.t
  and entry = Addr of Loc.t | Function of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let next_eid = ref 0
  let new_eid () =
    let eid = !next_eid in
    next_eid := eid + 1;
    eid

  let from_pre (e : SmmPre.exp) : exp =
    next_eid := 0;
    let rec annotate_pre e =
      let ne = new_eid () in
      match e with
      | SmmPre.NUM n -> (ne, NUM n)
      | SmmPre.TRUE -> (ne, TRUE)
      | SmmPre.FALSE -> (ne, FALSE)
      | SmmPre.VAR x -> (ne, VAR x)
      | SmmPre.ADD (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, ADD (e1', e2'))
      | SmmPre.SUB (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, SUB (e1', e2'))
      | SmmPre.MUL (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, MUL (e1', e2'))
      | SmmPre.DIV (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, DIV (e1', e2'))
      | SmmPre.MOD (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, MOD (e1', e2'))
      | SmmPre.EQUAL (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, EQUAL (e1', e2'))
      | SmmPre.LESS (e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, LESS (e1', e2'))
      | SmmPre.NOT e ->
        let e' = annotate_pre e in
        (ne, NOT e')
      | SmmPre.IF (e1, e2, e3) ->
        let (e1', e2', e3') = annotate_pre e1, annotate_pre e2, annotate_pre e3 in
        (ne, IF (e1', e2', e3'))
      | SmmPre.CALL (f, ids) ->
        (ne, CALL (f, ids))
      | SmmPre.LET (x, e1, e2) ->
        let (e1', e2') = annotate_pre e1, annotate_pre e2 in
        (ne, LET (x, e1', e2'))
      | SmmPre.LETFN (f, params, body, e1) ->
        let (body', e1') = annotate_pre body, annotate_pre e1 in
        (ne, LETFN (f, params, body', e1'))
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
    | Function (params, body, cenv) -> (params, body, cenv)
    | Addr _ -> raise (Error "TypeError: not a function")

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
      let (params, body, cenv) = entry_function (Env.lookup env f) in
      let entries = ids |> List.map (Env.lookup env) in
      let env' =
        match List.combine params entries with
        | bindings ->
          List.fold_left (fun env' (param, entry) -> Env.bind env' param entry) cenv bindings
        | exception Invalid_argument _ -> raise (Error "TypeError: wrong number of arguments")
      in
      eval mem env' body
    | LET (x, e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let l, mem2 = Mem.alloc mem1 in
      let mem3 = Mem.store mem2 l v1 in
      let env' = Env.bind env x (Addr l) in
      eval mem3 env' e2
    | LETFN (f, params, body, e1) ->
      let env' = Env.bind env f (Function (params, body, env)) in
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
  and change_entry = Value of change | Function of (change * (id list * change_fn * change_env))
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
      | LET (x, e1, e2) -> free_vars (x :: exclude) e1 @ free_vars exclude e2
      | LETFN (f, params, body, e1) -> free_vars (params @ exclude) body @ free_vars exclude e1
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
            let (eid1, _) = e1 in
            let (eid2, _) = e2 in
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | Same, Diff | Same, Unknown -> begin
                match Cache.lookup ptrace eid1 with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce2)
              end
              | Diff, Same | Unknown, Same -> begin
                match Cache.lookup ptrace eid2 with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce1)
              end
              | _, _ -> Value (Unknown)
            end
          | DIV (e1, e2) ->
            let (eid1, _) = e1 in
            let (eid2, _) = e2 in
            let ce1 = eval_change e1 pcc |> cent_value in
            let ce2 = eval_change e2 pcc |> cent_value in
            begin
              match ce1, ce2 with
              | Same, Same -> Value (Same)
              | Same, _ -> begin
                match Cache.lookup ptrace eid1 with
                | Some (Num 0) -> Value (Same)
                | _ -> Value (ce2)
              end 
              | Diff, Same | Unknown, Same -> begin
                match Cache.lookup ptrace eid2 with
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
                match Cache.lookup ptrace eid with
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
              match Cache.lookup ptrace eid1 with
              | None -> begin
                match ce1, ce2, ce3 with
                | Same, Same, Same -> Value (Same)
                | _, _, _ -> Value (Unknown)
              end
              | Some (Bool b) -> begin
                match ce1 with
                | Same -> if b then Value (ce2) else Value (ce3)
                | Diff -> if b then Value (ce3) else Value (ce2)
                | Unknown -> Value (Unknown)
              end
              | _ -> Value (Unknown) (* Should not happen *)
            end
          | LET (x, e1, e2) ->
            let ce1 = eval_change e1 pcc in
            let cenv' = Env.bind cenv x ce1 in
            eval_change e2 (ptrace, cenv', ctrace)
          | LETFN (f, params, body, e1) ->
            let body_fvs = free_vars params body in
            let aux change id = begin
              let c = Env.lookup cenv id |> cent_change in
              match change, c with
              | Same, Same -> Same
              | _, Diff -> Diff
              | _, _ -> Unknown
            end in
            let lit_change = List.fold_left aux Same body_fvs in
            let cenv' = Env.bind cenv f (Function (lit_change, (params, eval_change body, cenv))) in
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
