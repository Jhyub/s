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
  type 'a storage =
    | Empty
    | Array of 'a content array ref

  type 'a t = M of Loc.t * 'a storage

  let empty = M (Loc.base, Empty)
  let initial_capacity = 16

  let location_index loc = Loc.diff loc Loc.base

  let is_allocated boundary loc =
    let index = location_index loc in
    index >= 0 && index < location_index boundary

  let find_cell boundary storage loc =
    if not (is_allocated boundary loc) then raise Not_allocated;
    match storage with
    | Empty -> raise Not_allocated
    | Array cells ->
      let index = location_index loc in
      let cells = !cells in
      if index >= Array.length cells then raise Not_allocated;
      (cells, index)

  let load (M (boundary, storage)) loc =
    let cells, index = find_cell boundary storage loc in
    match cells.(index) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage) as memory) loc content =
    let cells, index = find_cell boundary storage loc in
    cells.(index) <- V content;
    memory

  let ensure_capacity cells index =
    let current = !cells in
    if index < Array.length current then current
    else begin
      let capacity =
        max (index + 1) (max initial_capacity (2 * Array.length current))
      in
      let grown = Array.make capacity U in
      Array.blit current 0 grown 0 (Array.length current);
      cells := grown;
      grown
    end

  let alloc (M (boundary, storage)) =
    let storage =
      match storage with
      | Empty -> Array (ref (Array.make initial_capacity U))
      | Array _ -> storage
    in
    match storage with
    | Empty -> assert false
    | Array cells ->
      let index = location_index boundary in
      let cells = ensure_capacity cells index in
      cells.(index) <- U;
      (boundary, M (Loc.increase boundary 1, storage))
end

module Env = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun _ -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

module Cache = struct
  type 'a storage = {
    mutable cells : 'a option array;
    mutable touched : int array;
    mutable touched_count : int;
  }

  type 'a t =
    | Empty
    | Array of 'a storage

  let empty = Empty
  let initial_capacity = 16

  let create_with_capacities cell_capacity touched_capacity =
    Array {
      cells = Array.make cell_capacity None;
      touched = Array.make touched_capacity 0;
      touched_count = 0;
    }

  let create () =
    create_with_capacities initial_capacity initial_capacity

  let validate_key key =
    if key < 0 then invalid_arg "Cache: negative key"

  let ensure_cell_capacity storage key =
    let current = storage.cells in
    if key < Array.length current then current
    else begin
      let capacity =
        max (key + 1) (max initial_capacity (2 * Array.length current))
      in
      let grown = Array.make capacity None in
      Array.blit current 0 grown 0 (Array.length current);
      storage.cells <- grown;
      grown
    end

  let ensure_touched_capacity storage =
    if storage.touched_count < Array.length storage.touched then ()
    else begin
      let current = storage.touched in
      let capacity = max initial_capacity (2 * Array.length current) in
      let grown = Array.make capacity 0 in
      Array.blit current 0 grown 0 (Array.length current);
      storage.touched <- grown
    end

  let clear = function
    | Empty -> ()
    | Array storage ->
      for index = 0 to storage.touched_count - 1 do
        storage.cells.(storage.touched.(index)) <- None
      done;
      storage.touched_count <- 0

  let lookup cache key =
    validate_key key;
    match cache with
    | Empty -> None
    | Array storage ->
      let cells = storage.cells in
      if key < Array.length cells then cells.(key) else None

  let bind cache key value =
    validate_key key;
    let cache =
      match cache with
      | Empty -> create ()
      | Array _ -> cache
    in
    match cache with
    | Empty -> assert false
    | Array storage ->
      let cells = ensure_cell_capacity storage key in
      begin
        match cells.(key) with
        | Some _ -> ()
        | None ->
          ensure_touched_capacity storage;
          storage.touched.(storage.touched_count) <- key;
          storage.touched_count <- storage.touched_count + 1
      end;
      cells.(key) <- Some value;
      cache

  let iter_present f = function
    | Empty -> ()
    | Array storage ->
      for index = 0 to storage.touched_count - 1 do
        let key = storage.touched.(index) in
        match storage.cells.(key) with
        | Some value -> f key value
        | None -> assert false
      done

  let merge cache1 cache2 =
    let length = function
      | Empty -> 0
      | Array storage -> Array.length storage.cells
    in
    let count = function
      | Empty -> 0
      | Array storage -> storage.touched_count
    in
    let cell_capacity =
      max initial_capacity (max (length cache1) (length cache2))
    in
    let touched_capacity =
      max initial_capacity (count cache1 + count cache2)
    in
    let merged = create_with_capacities cell_capacity touched_capacity in
    iter_present (fun key value -> bind merged key value |> ignore) cache2;
    iter_present (fun key value -> bind merged key value |> ignore) cache1;
    merged
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
  type parameter = eid * id

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
    | LETFN of fid * id * parameter list * exp * exp

  type program = exp
  type value = Num of int | Bool of bool (* | Pair of (value * value) *)
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and trace = value Cache.t
  and entry = Addr of Loc.t | Function of fid * parameter list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty
  let emptyTrace = Cache.empty

  let root_eid = 0
  let root_fid = 0

  let next_fid = ref (root_fid + 1)
  let new_fid () =
    let fid = !next_fid in
    next_fid := fid + 1;
    fid

  let from_pre2 (e : Smm_pre2.exp) : exp =
    next_fid := root_fid + 1;
    let rec annotate_domain params e =
      let next_eid = ref root_eid in
      let new_eid () =
        let eid = !next_eid in
        next_eid := eid + 1;
        eid
      in
      let rec annotate_pre e =
        let ne = new_eid () in
        match e with
        | Smm_pre2.NUM n -> (ne, NUM n)
        | Smm_pre2.TRUE -> (ne, TRUE)
        | Smm_pre2.FALSE -> (ne, FALSE)
        | Smm_pre2.VAR x -> (ne, VAR x)
        | Smm_pre2.ADD (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, ADD (e1', e2'))
        | Smm_pre2.SUB (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, SUB (e1', e2'))
        | Smm_pre2.MUL (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, MUL (e1', e2'))
        | Smm_pre2.DIV (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, DIV (e1', e2'))
        | Smm_pre2.MOD (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, MOD (e1', e2'))
        | Smm_pre2.EQUAL (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, EQUAL (e1', e2'))
        | Smm_pre2.LESS (e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, LESS (e1', e2'))
        | Smm_pre2.NOT e ->
          let e' = annotate_pre e in
          (ne, NOT e')
        | Smm_pre2.IF (e1, e2, e3) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          let e3' = annotate_pre e3 in
          (ne, IF (e1', e2', e3'))
        | Smm_pre2.CALL (f, ids) ->
          (ne, CALL (f, ids))
        | Smm_pre2.LET (x, e1, e2) ->
          let e1' = annotate_pre e1 in
          let e2' = annotate_pre e2 in
          (ne, LET (x, e1', e2'))
        | Smm_pre2.LETFN (f, params, body, e1) ->
          let fid = new_fid () in
          let (params', body') = annotate_domain params body in
          let e1' = annotate_pre e1 in
          (ne, LETFN (fid, f, params', body', e1'))
      in
      let e' = annotate_pre e in
      (* Parameter values share this domain's trace but do not interrupt the
         dense preorder range of its expressions. *)
      let params' =
        List.map (fun param -> (new_eid (), param)) params
      in
      (params', e')
    in
    annotate_domain [] e |> snd

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
          List.fold_left
            (fun env'' ((_, param), entry) -> Env.bind env'' param entry)
            env'
            bindings
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
  type change_env = (id, change) Env.t
  type change_trace = change Cache.t
  type change_fn = (trace * change_env * change_trace) -> change

  let emptyChangeEnv = Env.empty

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
      | LETFN (fid, f, params, body, e1) ->
        let param_names = List.map snd params in
        free_vars (param_names @ exclude) body @ free_vars (f :: exclude) e1
      | CALL (f, ids) -> List.fold_left (fun ret id -> if List.mem id exclude then ret else id :: ret) [] (f :: ids)
    end |> keep_unique
    
  type change_fn_table = change_fn array

  let compile_change_fns
      (e : exp)
      : (parameter list * change_fn_table) array =
    let change_fn_tables = Dynarray.create () in
    let uninitialized_change_fn _ =
      failwith "compile_change_fns: uninitialized change-function slot"
    in
    let rec compile_domain fid params (e : exp) =
      Dynarray.add_last change_fn_tables (params, [||]);
      let change_fns = Dynarray.create () in
      let _domain_change = compile change_fns e in
      let change_fns = Dynarray.to_array change_fns in
      Dynarray.set change_fn_tables fid (params, change_fns)
    and compile change_fns ((eid, e') : exp) =
      (* Eids are assigned in preorder, so reserve this node's slot before
         recursively compiling its children. *)
      Dynarray.add_last change_fns uninitialized_change_fn;
      let compute =
        match e' with
        | NUM _ | TRUE | FALSE ->
          fun _ -> Same
        | VAR x ->
          fun (_, cenv, _) -> Env.lookup cenv x
        | ADD (e1, e2) | SUB (e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun pcc ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | Same, Diff | Diff, Same -> Diff
              | _, _ -> Unknown
            end
        | MUL (e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun pcc ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | _, _ -> Unknown
            end
        | DIV (e1, e2) ->
          let (eid2, _) = e2 in
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun ((ptrace, _, _) as pcc) ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | Diff, Same | Unknown, Same ->
                begin
                  match Cache.lookup ptrace eid2 with
                  | Some (Num 1) -> ce1
                  | _ -> Unknown
                end
              | _, _ -> Unknown
            end
        | MOD (e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun pcc ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | _, _ -> Unknown
            end
        | EQUAL (e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun ((ptrace, _, _) as pcc) ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | Same, Diff | Diff, Same ->
                begin
                  match Cache.lookup ptrace eid with
                  | Some (Bool true) -> Diff
                  | _ -> Unknown
                end
              | _, _ -> Unknown
            end
        | LESS (e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun pcc ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            begin
              match ce1, ce2 with
              | Same, Same -> Same
              | _, _ -> Unknown
            end
        | NOT e ->
          let change = compile change_fns e in
          fun pcc -> change pcc
        | IF (e1, e2, e3) ->
          let (eid1, _) = e1 in
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          let change3 = compile change_fns e3 in
          fun ((ptrace, _, _) as pcc) ->
            let ce1 = change1 pcc in
            let ce2 = change2 pcc in
            let ce3 = change3 pcc in
            begin
              match Cache.lookup ptrace eid1 with
              | None ->
                begin
                  match ce1, ce2, ce3 with
                  | Same, Same, Same -> Same
                  | _, _, _ -> Unknown
                end
              | Some (Bool b) ->
                begin
                  match ce1 with
                  | Same -> if b then ce2 else ce3
                  | Diff | Unknown -> Unknown
                end
              | _ -> Unknown
            end
        | LET (x, e1, e2) ->
          let change1 = compile change_fns e1 in
          let change2 = compile change_fns e2 in
          fun (ptrace, cenv, ctrace) ->
            let ce1 = change1 (ptrace, cenv, ctrace) in
            let cenv' = Env.bind cenv x ce1 in
            change2 (ptrace, cenv', ctrace)
        | LETFN (fid, f, params, body, e1) ->
          let body_fvs = free_vars (List.map snd params) body in
          compile_domain fid params body;
          let change1 = compile change_fns e1 in
          fun (ptrace, cenv, ctrace) ->
            let combine change id =
              let c = Env.lookup cenv id in
              match change, c with
              | Same, Same -> Same
              | _, Diff -> Diff
              | _, _ -> Unknown
            in
            let lit_change = List.fold_left combine Same body_fvs in
            let cenv' = Env.bind cenv f lit_change in
            change1 (ptrace, cenv', ctrace)
        | CALL (f, ids) ->
          fun (_, cenv, _) ->
            let combine change id =
              let c = Env.lookup cenv id in
              match change, c with
              | Same, Same -> Same
              | _, _ -> Unknown
            in
            List.fold_left combine Same (f :: ids)
      in
      (* Previous value trace, change environment, and current change trace. *)
      let change_fn ((_, _, ctrace) as pcc) =
        match Cache.lookup ctrace eid with
        | Some change -> change
        | None ->
          let change = compute pcc in
          begin
            match change with
            | Same | Diff -> Cache.bind ctrace eid change |> ignore
            | Unknown -> ()
          end;
          change
      in
      Dynarray.set change_fns eid change_fn;
      change_fn
    in
    compile_domain root_fid [] e;
    Dynarray.to_array change_fn_tables

  let eval_change (e : exp) : change_fn =
    let (_, root_change_fns) =
      (compile_change_fns e).(root_fid)
    in
    root_change_fns.(root_eid)

end

module Smm = struct
  exception Error of string

  open Smm_pre

  (* eid list: where will the eval function jump, *)
  (* to expect possible Same from given change_fn? *)
  (* Initial compare points might have empty eid lists, *)
  (* but this might be overwritten in save point population progress *)
  (* ^ does this cause costly change_fn calls? *)
  type etype = Normal | SavPnt of (change_fn * eid list) | CmpPnt of (change_fn * eid list)

  let savepoint_data = function
    | Normal -> None
    | SavPnt x | CmpPnt x -> Some x

  type exp = eid * etype * ebody
  and ebody =
    | NUM of int
    | TRUE
    | FALSE
    | VAR of id
    | ADD of eid * eid
    | SUB of eid * eid
    | MUL of eid * eid
    | DIV of eid * eid
    | MOD of eid * eid
    | EQUAL of eid * eid
    | LESS of eid * eid
    | NOT of eid
    | IF of eid * eid * eid (* if-then-else *)
    | CALL of id * id list
    | LET of id * eid * eid
    | LETFN of fid * id * parameter list * eid * eid

  (* fid -> (params, eid -> exp) *)
  (* fid #0 is the root code (empty params) *)
  (* eid #0 is the root expression of each function / root code *)
  type program = (parameter list * (exp array)) array
  type value = Num of int | Bool of bool (* | Pair of (value * value) *)
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and trace = value Cache.t
  and entry = Addr of Loc.t | Function of fid * parameter list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty
  let emptyTrace = Cache.empty

  let from_pre (e : Smm_pre.program) : program =
    let domains = Dynarray.create () in
    let expression_eid ((eid, _) : Smm_pre.exp) = eid in
    let rec flatten_domain fid params e =
      (* Fids are assigned in preorder, so reserve this domain before
         flattening any nested function domains. *)
      Dynarray.add_last domains (params, [||]);
      let expressions = Dynarray.create () in
      flatten expressions e;
      let expressions = Dynarray.to_array expressions in
      Dynarray.set domains fid (params, expressions)
    and flatten expressions ((eid, e') : Smm_pre.exp) =
      let append ebody =
        Dynarray.add_last expressions (eid, Normal, ebody)
      in
      match e' with
      | Smm_pre.NUM n -> append (NUM n)
      | Smm_pre.TRUE -> append TRUE
      | Smm_pre.FALSE -> append FALSE
      | Smm_pre.VAR x -> append (VAR x)
      | Smm_pre.ADD (e1, e2) ->
        append (ADD (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.SUB (e1, e2) ->
        append (SUB (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.MUL (e1, e2) ->
        append (MUL (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.DIV (e1, e2) ->
        append (DIV (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.MOD (e1, e2) ->
        append (MOD (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.EQUAL (e1, e2) ->
        append (EQUAL (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.LESS (e1, e2) ->
        append (LESS (expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.NOT e ->
        append (NOT (expression_eid e));
        flatten expressions e
      | Smm_pre.IF (e1, e2, e3) ->
        append
          (IF
             ( expression_eid e1,
               expression_eid e2,
               expression_eid e3 ));
        flatten expressions e1;
        flatten expressions e2;
        flatten expressions e3
      | Smm_pre.CALL (f, ids) -> append (CALL (f, ids))
      | Smm_pre.LET (x, e1, e2) ->
        append (LET (x, expression_eid e1, expression_eid e2));
        flatten expressions e1;
        flatten expressions e2
      | Smm_pre.LETFN (fid, f, params, body, e1) ->
        append
          (LETFN
             ( fid,
               f,
               params,
               expression_eid body,
               expression_eid e1 ));
        flatten_domain fid params body;
        flatten expressions e1
    in
    flatten_domain root_fid [] e;
    Dynarray.to_array domains

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError: not bool")

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

end


(*
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
    | LETFN of fid * id * parameter list * exp * exp

  type env = (id, entry) Env.t
  and entry = Addr of Loc.t | Function of fid * parameter list * exp * env

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

  let debug_same_hit enabled eid ebody =
    if enabled then begin
      prerr_string
        ("[Smm.eval] Same hit: eid="
         ^ string_of_int eid
         ^ " expr="
         ^ string_of_ebody_type ebody
         ^ "\n");
      flush stderr
    end

  let debug_reuse_hit enabled eid ebody =
    if enabled then begin
      prerr_string
        ("[Smm.eval] Reuse hit: eid="
         ^ string_of_int eid
         ^ " expr="
         ^ string_of_ebody_type ebody
         ^ "\n");
      flush stderr
    end

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
      | LETFN (fid, f, params, body, e1) ->
        let param_names = List.map snd params in
        free_vars (param_names @ exclude) body @ free_vars (f :: exclude) e1
      | CALL (f, ids) -> List.fold_left (fun ret id -> if List.mem id exclude then ret else id :: ret) [] (f :: ids)
    end |> keep_unique

  let from_pre (e : Smm_pre.exp) : exp =
    let change_fns = Smm_pre.compile_change_fns e in
    let rec lower ((eid, e') : Smm_pre.exp) =
      let change_fn = Hashtbl.find change_fns eid in
      match e' with
      | Smm_pre.NUM n -> (eid, change_fn, NUM n)
      | Smm_pre.TRUE -> (eid, change_fn, TRUE)
      | Smm_pre.FALSE -> (eid, change_fn, FALSE)
      | Smm_pre.VAR x -> (eid, change_fn, VAR x)
      | Smm_pre.ADD (e1, e2) -> (eid, change_fn, ADD (lower e1, lower e2))
      | Smm_pre.SUB (e1, e2) -> (eid, change_fn, SUB (lower e1, lower e2))
      | Smm_pre.MUL (e1, e2) -> (eid, change_fn, MUL (lower e1, lower e2))
      | Smm_pre.DIV (e1, e2) -> (eid, change_fn, DIV (lower e1, lower e2))
      | Smm_pre.MOD (e1, e2) -> (eid, change_fn, MOD (lower e1, lower e2))
      | Smm_pre.EQUAL (e1, e2) -> (eid, change_fn, EQUAL (lower e1, lower e2))
      | Smm_pre.LESS (e1, e2) -> (eid, change_fn, LESS (lower e1, lower e2))
      | Smm_pre.NOT e -> (eid, change_fn, NOT (lower e))
      | Smm_pre.IF (e1, e2, e3) ->
        (eid, change_fn, IF (lower e1, lower e2, lower e3))
      | Smm_pre.CALL (f, ids) -> (eid, change_fn, CALL (f, ids))
      | Smm_pre.LET (x, e1, e2) ->
        (eid, change_fn, LET (x, lower e1, lower e2))
      | Smm_pre.LETFN (fid, f, params, body, e1) ->
        (eid, change_fn, LETFN (fid, f, params, lower body, lower e1))
    in
    lower e
  
  (* Keep the completed previous trace stable while accumulating this traversal
     in a separate mutable trace. *)
  (*
    mem: the memory we always use
    env: the environment we always use
    ptrace: read-only previous trace for the current evaluation domain. Function bodies use the trace from their most recent completed evaluation.
    trace: mutable trace produced by the current traversal.
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
  type function_trace_state = {
    value_traces : trace array;
    change_trace : change_trace;
    mutable previous_trace : int;
    mutable has_previous_trace : bool;
  }

  type eval_state = {
    function_traces : (fid, function_trace_state) Hashtbl.t;
    mutable active_trace : trace option;
    debug : bool;
  }

  let create_function_trace_state () =
    {
      value_traces = [| Cache.create (); Cache.create () |];
      change_trace = Cache.create ();
      previous_trace = 0;
      has_previous_trace = false;
    }

  let function_previous_trace state fid =
    match Hashtbl.find_opt state.function_traces fid with
    | Some traces when traces.has_previous_trace ->
      Some traces.value_traces.(traces.previous_trace)
    | Some _ | None -> None

  let reset_function_trace state fid =
    match Hashtbl.find_opt state.function_traces fid with
    | None -> ()
    | Some traces ->
      Array.iter Cache.clear traces.value_traces;
      Cache.clear traces.change_trace;
      traces.previous_trace <- 0;
      traces.has_previous_trace <- false

  let rec eval_with_state state (mem: memory) (env: env) (ptrace: trace) (trace: trace) (cenv: change_env) (ctrace: change_trace) (e: exp): (value * memory * trace * change_trace) =
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
      let change = change_fn (ptrace, cenv, ctrace) |> cent_change in
      let early_return = begin
        match change with
        | Same ->
          debug_same_hit state.debug eid e';
          Cache.lookup ptrace eid
        | Unknown -> None
        | Diff -> None
      end in
      match early_return with
      | Some ret ->
        debug_reuse_hit state.debug eid e';
        (ret, mem, Cache.bind trace eid ret, ctrace)
      | None -> begin
      (* Literals *)
      let aux x = (x, mem, Cache.bind trace eid x, ctrace) in
      (* We re-evaluate the change with additional information from new ctrace, *)
      (* and propagate our change information accordingly, *)
      (* only if we need to do (overriding 'Unknown's.) *)
      (* Do not override with 'Unknown' in ctrace. *)
      let aux' change_fn cenv v mem trace ctrace = begin
        let change' = change_fn (ptrace, cenv, ctrace) |> cent_change in
        let trace' = Cache.bind trace eid v in
        match change' with
        | Same ->
          debug_same_hit state.debug eid e';
          let v = begin
            match Cache.lookup ptrace eid with
            | Some v -> debug_reuse_hit state.debug eid e'; v
            | None -> v
          end in
          (v, mem, trace', ctrace)
        | Diff ->
          (v, mem, trace', ctrace)
        | Unknown -> begin
          match Cache.lookup ptrace eid with
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
        (v, mem, Cache.bind trace eid v, ctrace)
      | ADD (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 + value_int v2)) mem2 trace2 ctrace2
      | SUB (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 - value_int v2)) mem2 trace2 ctrace2
      | MUL (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 * value_int v2)) mem2 trace2 ctrace2
      | DIV (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 / value_int v2)) mem2 trace2 ctrace2
      | MOD (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Num (value_int v1 mod value_int v2)) mem2 trace2 ctrace2
      | EQUAL (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Bool (eq v1 v2)) mem2 trace2 ctrace2
      | LESS (e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
        aux'' (Bool (value_int v1 < value_int v2)) mem2 trace2 ctrace2
      | NOT e ->
        let (v, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e in
        aux'' (Bool (not (value_bool v))) mem1 trace1 ctrace1
      | IF (e1, e2, e3) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        if value_bool v1 then
          let (v2, mem2, trace2, ctrace2) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e2 in
          aux'' v2 mem2 trace2 ctrace2
        else
          let (v3, mem3, trace3, ctrace3) = eval_with_state state mem1 env ptrace trace1 cenv ctrace1 e3 in
          aux'' v3 mem3 trace3 ctrace3
      | CALL (f, ids) ->
        (* We do not run additional evaluations for 'call' itself, *)
        (* so early return is enough. *)
        let (fid, params, body, env') = Env.lookup env f |> entry_function in
        let (_, (_, _, _, cenv')) = Env.lookup cenv f |> cent_function in
        let function_trace =
          match Hashtbl.find_opt state.function_traces fid with
          | Some traces -> traces
          | None ->
            let traces = create_function_trace_state () in
            Hashtbl.add state.function_traces fid traces;
            traces
        in
        let fn_ptrace =
          if function_trace.has_previous_trace then
            function_trace.value_traces.(function_trace.previous_trace)
          else emptyTrace
        in
        let arguments = List.combine ids params in
        (* Compute change for each argument *)
        (* CALL is a point of change propagation. <- is this inevitable? *)
        let aux''' (id, (param_eid, _)) = begin
          let v_old = Cache.lookup fn_ptrace param_eid in
          let v = Mem.load mem (entry_addr (Env.lookup env id)) in
          match v_old with
          | Some v_old ->
            if eq v v_old then begin
              debug_same_hit state.debug eid e';
              Same
            end else Diff
          | None -> Unknown
        end in
        let aux'''' trace (id, (param_eid, _)) = begin
          let v = Mem.load mem (entry_addr (Env.lookup env id)) in
          Cache.bind trace param_eid v
        end in
        let changes = List.map aux''' arguments in
        let changes' = List.combine params changes in
        let cenv'' =
          List.fold_left
            (fun cenv ((_, param), change) ->
              Env.bind cenv param (Value change))
            cenv'
            changes'
        in
        let entries = ids |> List.map (Env.lookup env) in
        let env'' =
          begin
            match List.combine params entries with
            | bindings ->
              List.fold_left
                (fun env' ((_, param), entry) -> Env.bind env' param entry)
                env'
                bindings
            | exception Invalid_argument _ -> raise (Error "TypeError: wrong number of arguments")
            end
        in
        let current_trace_index =
          if function_trace.has_previous_trace then
            1 - function_trace.previous_trace
          else 0
        in
        let fresh_trace = function_trace.value_traces.(current_trace_index) in
        Cache.clear fresh_trace;
        List.fold_left aux'''' fresh_trace arguments |> ignore;
        Cache.clear function_trace.change_trace;
        let caller_trace = state.active_trace in
        let ((v, mem', _, _), completed_trace) =
          Fun.protect
            ~finally:(fun () -> state.active_trace <- caller_trace)
            (fun () ->
              state.active_trace <- Some fresh_trace;
              let result =
                eval_with_state
                  state mem env'' fn_ptrace fresh_trace cenv''
                  function_trace.change_trace body
              in
              let completed_trace =
                match state.active_trace with
                | Some trace -> trace
                | None -> assert false
              in
              (result, completed_trace))
        in
        if completed_trace != fresh_trace then assert false;
        let result = aux'' v mem' trace ctrace in
        function_trace.previous_trace <- current_trace_index;
        function_trace.has_previous_trace <- true;
        result
      | LET (x, e1, e2) ->
        let (v1, mem1, trace1, ctrace1) = eval_with_state state mem env ptrace trace cenv ctrace e1 in
        (* LET is also a point of change propagation.*)
        let (eid1, change_fn1, _) = e1 in 
        let change' = change_fn1 (ptrace, cenv, ctrace1) |> cent_change in
        let change'' = begin
          match change' with
          | Same ->
            debug_same_hit state.debug eid e';
            Same
          | Diff -> Diff
          | Unknown ->
            let v1_old = Cache.lookup ptrace eid1 in
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
            debug_same_hit state.debug eid e';
            Cache.lookup ptrace eid
          | Diff | Unknown -> None
        end in
        begin
          match v' with
          | Some v' ->
            debug_reuse_hit state.debug eid e';
            (v', mem1, Cache.bind trace1 eid v', ctrace1')
          | None -> eval_with_state state mem3 env' ptrace trace1 cenv' ctrace1' e2 |> fun (v, mem, trace, ctrace) -> (v, mem, Cache.bind trace eid v, ctrace)
        end
      | LETFN (fid, f, params, body, e1) ->
        (* We do not run additional evaluations for 'letfn' itself, *)
        (* so early return is enough. *)
        (* 왜 cenv maintain하는 코드가 둘 다에서 있는 것 같지? *)
        (* A dynamically created closure must not reuse a trace collected by
           an older activation of the same static function definition. *)
        reset_function_trace state fid;
        let env' = Env.bind env f (Function (fid, params, body, env)) in
        let body_fvs = free_vars (List.map snd params) body in
        let aux change id = begin
          let c = Env.lookup cenv id |> cent_change in
          match change, c with
          | Same, Same ->
            debug_same_hit state.debug eid e';
            Same
          | _, Diff -> Diff
          | _, _ -> Unknown
        end in
        let lit_change = List.fold_left aux Same body_fvs in
        let (eidb, cfnb, ebodyb) = body in
        let cenv' = Env.bind cenv f (Function (lit_change, (fid, params, cfnb, cenv))) in
        eval_with_state state mem env' ptrace trace cenv' ctrace e1 |> fun (v, mem, trace, ctrace) -> (v, mem, Cache.bind trace eid v, ctrace)
      end
    in
    let (v, _, _, _) = result in
    begin
      match state.active_trace with
      | Some trace -> state.active_trace <- Some (Cache.bind trace eid v)
      | None -> ()
    end;
    result

  let eval ?(debug = true) (e: exp): (value * memory * trace * change_trace) =
    let state = {
      function_traces = Hashtbl.create 16;
      active_trace = None;
      debug;
    } in
    eval_with_state
      state emptyMemory Env.empty emptyTrace (Cache.create ())
      emptyChangeEnv (Cache.create ()) e



end
*)
