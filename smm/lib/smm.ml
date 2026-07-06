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

module Smm = struct
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
    (* | SEQ of exp * exp (* sequence *) *)
    (* | PAIR of exp * exp *)
    (* | PFST of exp *)
    (* | PSND of exp *)
    | IF of exp * exp * exp (* if-then-else *)
    | CALL of id * id list
    | LET of id * exp * exp
    | LETFN of id * id list * exp * exp

  type program = exp
  type value = Num of int | Bool of bool (* | Pair of (value * value) *)
  and memory = value Mem.t
  and env = (id, entry) Env.t
  and entry = Addr of Loc.t | Function of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

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
    match e with
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
  (* Question: do we make it sound to mark anything as Diff? *)
  type change = Same | Diff | Unknown
  and change_env = (id, change_entry) Env.t
  and change_fn = change_env -> change
  and change_entry = Value of change | Function of (id list * exp * change_env)
  let emptyChangeEnv = Env.empty

  let change_entry_value entry =
    match entry with | Value v -> v | Function _ -> raise (Error "TypeError: not a value")

  let change_entry_function entry =
    match entry with | Function f -> f | Value _ -> raise (Error "TypeError: not a function")

  let rec free_vars bound_vars e =
    begin
      match e with
      | NUM _ | TRUE | FALSE -> []
      | VAR x -> [x]
      | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | EQUAL (e1, e2) | LESS (e1, e2) ->
        let e1' = free_vars bound_vars e1 in
        let e2' = free_vars bound_vars e2 in
        e1' @ e2'
      | NOT e -> free_vars bound_vars e
      | IF (e1, e2, e3) -> free_vars bound_vars e1 @ free_vars bound_vars e2 @ free_vars bound_vars e3
      | LET (x, e1, e2) -> free_vars bound_vars e1 @ free_vars (x :: bound_vars) e2
      | LETFN (f, params, body, e1) -> free_vars (params @ bound_vars) body @ free_vars (f :: bound_vars) e1
      | CALL (f, ids) -> f :: ids
    end |> List.filter (fun x -> not (List.mem x bound_vars))

  let unique_ids ids =
    let rec aux seen acc ids =
      match ids with
      | [] -> List.rev acc
      | id :: rest ->
        if List.mem id seen then aux seen acc rest
        else aux (id :: seen) (id :: acc) rest
    in
    aux [] [] ids

  let free_variable_list pgm = free_vars [] pgm |> unique_ids

  let bind_value (mem, env) id value =
    let loc, mem' = Mem.alloc mem in
    let mem'' = Mem.store mem' loc value in
    (mem'', Env.bind env id (Addr loc))

  let run_with_values pgm bindings =
    let mem, env =
      List.fold_left
        (fun state (id, value) -> bind_value state id value)
        (emptyMemory, emptyEnv)
        bindings
    in
    fst (eval mem env pgm)

  let change_of_values before after = if eq before after then Same else Diff

  let change_env_from_values ids before_values after_values =
    let rec aux cenv ids before_values after_values =
      match ids, before_values, after_values with
      | [], [], [] -> cenv
      | id :: ids', before :: before_values', after :: after_values' ->
        let entry = Value (change_of_values before after) in
        aux (Env.bind cenv id entry) ids' before_values' after_values'
      | _ -> raise (Error "TypeError: wrong number of arguments")
    in
    aux emptyChangeEnv ids before_values after_values

  let final_change change_fn ids before_values after_values =
    let cenv = change_env_from_values ids before_values after_values in
    change_fn cenv |> change_entry_value

  let rec eval_change e =
    match e with
    | NUM _ | TRUE | FALSE -> fun _ -> Value (Same)
    | VAR x -> fun cenv -> Env.lookup cenv x
    | ADD (e1, e2) | SUB (e1, e2) -> fun cenv ->
      let e1' = eval_change e1 cenv |> change_entry_value in
      let e2' = eval_change e2 cenv |> change_entry_value in
      begin
        match e1', e2' with
        | Same, Same -> Value (Same)
        | Same, Diff | Diff, Same -> Value (Diff)
        | _, _ -> Value (Unknown)
      end
    | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) -> fun cenv ->
      let e1' = eval_change e1 cenv |> change_entry_value in
      let e2' = eval_change e2 cenv |> change_entry_value in
      begin
        match e1', e2' with
        | Same, Same -> Value (Same)
        (* | Same, Diff | Diff, Same -> Diff *) (* We might have cases like 0 * x = 0 *)
        | _, _ -> Value (Unknown)
      end
    | EQUAL (e1, e2) -> fun cenv ->
      let e1' = eval_change e1 cenv |> change_entry_value in
      let e2' = eval_change e2 cenv |> change_entry_value in
      begin
        match e1', e2' with
        | Same, Same -> Value (Same)
        | Same, Diff | Diff, Same -> Value (Diff)
        | _, _ -> Value (Unknown)
      end
    | LESS (e1, e2) -> fun cenv ->
      let e1' = eval_change e1 cenv |> change_entry_value in
      let e2' = eval_change e2 cenv |> change_entry_value in
      begin
        match e1', e2' with
        | Same, Same -> Value (Same)
        | _, _ -> Value (Unknown)
      end
    | NOT e -> fun cenv -> eval_change e cenv
    | IF (e1, e2, e3) -> fun cenv ->
      let e1' = eval_change e1 cenv |> change_entry_value in
      let e2' = eval_change e2 cenv |> change_entry_value in
      let e3' = eval_change e3 cenv |> change_entry_value in
      begin
        match e1', e2', e3' with
        | Same, Same, Same -> Value (Same)
        | _, _, _ -> Value (Unknown)
      end
    | LET (x, e1, e2) -> fun cenv ->
      let e1' = eval_change e1 cenv in
      let cenv' = Env.bind cenv x e1' in
      eval_change e2 cenv'
    (* Are these good enough? Can we make a 'function' itself same or not same to skip each calls for CALL? *)
    | LETFN (f, params, body, e1) -> fun cenv ->
      let cenv' = Env.bind cenv f (Function (params, body, cenv)) in
      eval_change e1 cenv'
    | CALL (f, ids) -> fun cenv ->
      let (params, body, cenv') = change_entry_function (Env.lookup cenv f) in
      let entries = ids |> List.map (Env.lookup cenv) in
      let cenv'' =
        match List.combine params entries with
        | bindings ->
          List.fold_left (fun cenv'' (param, entry) -> Env.bind cenv'' param entry) cenv' bindings
        | exception Invalid_argument _ -> raise (Error "TypeError: wrong number of arguments")
      in
      eval_change body cenv''

  (*
  type equality = Equal | IfBoth of (equality * equality) | IfEither of (equality * equality) | IfVar of id | Unknown
  and eq_val = Plain of equality | Closure of (equality * (id list * eq_val))
  and eq_env = (id, eq_val) Env.t

  let emptyEqEnv = Env.empty

  (* Merging: think of an if-then-else, so we might have different paths *)
  (* But we behave conservatively so we say equality if both of them are equal *)
  let rec merge_eq_val e1 e2 =
    match e1, e2 with
    | Plain e1', Plain e2' | Closure (e1', _), Closure (e2', _) -> Plain (IfBoth (e1', e2')) (* TODO: We are erasing function information, how to handle? *)
    | Plain ep, Closure (ec, ec') | Closure (ec, ec'), Plain ep -> (* It 'might' be a closure, so it is useful to keep closure information *)
      Closure (IfBoth (ep, ec), ec')

  let rec free_vars ids e =
    begin
      match e with
      | NUM _ | TRUE | FALSE -> []
      | VAR x -> [x]
      | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | EQUAL (e1, e2) | LESS (e1, e2) ->
        let e1' = free_vars ids e1 in
        let e2' = free_vars ids e2 in
        e1' @ e2'
      | NOT e -> free_vars ids e
      | IF (e1, e2, e3) -> free_vars ids e1 @ free_vars ids e2 @ free_vars ids e3
      | LET (x, e1, e2) -> free_vars ids e1 @ free_vars (x :: ids) e2
      | FN (ids, body) -> free_vars ids body
      | CALL (e, ids') -> free_vars ids e @ ids'
    end |> List.filter (fun x -> not (List.mem x ids))

  let rec equality_from_id_list ids =
    match ids with
    | id::ids' -> IfBoth (IfVar id, equality_from_id_list ids')
    | [] -> Equal

  let rec satisfy_equality eq id =
    match eq with
    | Equal | Unknown -> eq
    | IfVar id' -> if id = id' then Equal else eq
    | IfBoth (eq1, eq2) ->
      let eq1' = satisfy_equality eq1 id in
      let eq2' = satisfy_equality eq2 id in
      IfBoth (eq1', eq2')
    | IfEither (eq1, eq2) ->
      let eq1' = satisfy_equality eq1 id in
      let eq2' = satisfy_equality eq2 id in
      IfEither (eq1', eq2')

  let rec satifsfy_eq_val eq_val id = (* This seems like a hack to me *)
    match eq_val with
    | Plain eq -> Plain (satisfy_equality eq id)
    | Closure (eq1, (ids, eq2)) ->
      let eq1' = satisfy_equality eq1 id in
      let eq2' = satifsfy_eq_val eq2 id in
      Closure (eq1', (ids, eq2'))

  let rec replace_equality eq id id' =
    match eq with
    | Equal | Unknown -> eq
    | IfVar id'' -> if id = id'' then IfVar id' else eq
    | IfBoth (eq1, eq2) -> IfBoth (replace_equality eq1 id id', replace_equality eq2 id id')
    | IfEither (eq1, eq2) -> IfEither (replace_equality eq1 id id', replace_equality eq2 id id')

  let rec replace_eq_val eq_val id id' =
    match eq_val with
    | Plain eq -> Plain (replace_equality eq id id')
    | Closure (eq1, (ids, eq2)) -> Closure (replace_equality eq1 id id', (ids, replace_eq_val eq2 id id'))

  let rec eval_eq_val eq_env e =
    match e with
    | NUM _ | TRUE | FALSE -> Plain (Equal)
    | FN (ids, body) ->
      let fvs = free_vars ids body in
      let self = equality_from_id_list fvs in
      let eq_env' = List.fold_left (fun eq_env' id -> Env.bind eq_env' id (Plain (IfVar id))) eq_env ids in (* TODO: this is information loss when a closure is passed in as an argument *)
      let closure = eval_eq_val eq_env' body in
      let closure' = List.fold_left (fun closure' id -> satifsfy_eq_val closure' id) closure fvs in
      Closure(self, (ids, closure'))
    | VAR x -> Env.lookup eq_env x 
    | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) | MOD (e1, e2) | EQUAL (e1, e2) | LESS (e1, e2) ->
      let e1' = eval_eq_val eq_env e1 in
      let e2' = eval_eq_val eq_env e2 in
      merge_eq_val e1' e2'
    | NOT e -> eval_eq_val eq_env e
    | IF (e1, e2, e3) -> (* Temporarily check all subexpressions' equality *)
      let e1' = eval_eq_val eq_env e1 in
      let e2' = eval_eq_val eq_env e2 in
      let e3' = eval_eq_val eq_env e3 in
      e1' |> merge_eq_val e2' |> merge_eq_val e3'
    | LET (x, e1, e2) ->
      let e1' = eval_eq_val eq_env e1 in
      let eq_env' = Env.bind eq_env x e1' in
      eval_eq_val eq_env' e2
    | CALL (e, ids) ->
      let e' = eval_eq_val eq_env e in
      let replace equality ids' ids = List.combine ids' ids
        |> List.fold_left (fun equality (id', id) -> replace_equality equality id id') equality in
      match e' with
      | Closure (self, (ids', Closure (inner, innerc))) -> Closure (IfBoth (self, replace inner ids' ids), innerc)
      | Closure (self, (ids', Plain eq_val)) -> Plain (IfBoth (self, replace eq_val ids' ids))
      | Plain _ -> Plain (Unknown)
  *)

end
