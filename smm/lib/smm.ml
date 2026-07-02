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
    | FN of id list * exp
    | CALL of exp * id list
    | LET of id * exp * exp

  type program = exp
  type value = Num of int | Bool of bool (* | Pair of (value * value) *) | Function of (int * (id list) * exp * env)
  and memory = value Mem.t
  and env = (id, Loc.t) Env.t

  let functionId = ref 0
  let newFunctionId () =
    functionId := !functionId + 1;
    !functionId

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")
  
  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError: not bool")

  (* let value_pair v =
    match v with Pair p -> p | _ -> raise (Error "TypeError: not pair") *)
  
  let value_function v =
    match v with Function f -> f | _ -> raise (Error "TypeError: not function")

  let eq v1 v2 =
    match v1, v2 with
    | Num n1, Num n2 -> n1 = n2
    | Bool b1, Bool b2 -> b1 = b2
    | Function (fid1, _, _, _), Function (fid2, _, _, _) -> fid1 = fid2
    | _ -> false
  
  let rec eval mem env e =
    match e with
    | NUM n -> (Num n, mem)
    | FN (ids, body) -> (Function (newFunctionId (), ids, body, env), mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | VAR x -> (Mem.load mem (Env.lookup env x), mem)
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
    | CALL (e, ids) ->
      let (f, mem1) = eval mem env e in
      let locs = ids |> List.map (Env.lookup env) in
      call_fn mem1 env f locs
    | LET (x, e1, e2) ->
      let (v1, mem1) = eval mem env e1 in
      let l, mem2 = Mem.alloc mem1 in
      let mem3 = Mem.store mem2 l v1 in
      let env' = Env.bind env x l in
      eval mem3 env' e2
  and call_fn mem env f locs =
    let (_, id's, body, env') = value_function f in
    let env'' = List.combine id's locs |> List.fold_left (fun env'' (id', loc) -> Env.bind env'' id' loc) env' in
    eval mem env'' body

  (* Memory, environment, function (interpreted program), and locations of arguments *)
  type run_ctx = memory * env * value * (Loc.t list)

  let build_run_ctx (mem, env, pgm) =
    let v, mem' = eval mem env pgm in
    let (_, ids, _, _) = value_function v in
    let mem'', locs = List.fold_left (fun (mem', locs) id ->
      let l, mem'' = Mem.alloc mem' in
      (mem'', l :: locs)
    ) (mem', []) ids in
    (mem'', env, v, locs)

  let run (ctx, args) =
    let (mem, env, f, locs) = ctx in
    let mem' = List.combine locs args |> List.fold_left (fun mem' (loc, arg) -> Mem.store mem' loc arg) mem in
    call_fn mem' env f locs

end
