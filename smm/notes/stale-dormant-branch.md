# Stale dormant-branch entries under lazy IF

~~*Status: confirmed wrong-value bug, unfixed. Verified by differential testing
against `Smm_pre.run` on 2026-07-10 (working tree with lazy IF and the DIV
zero-refinement removed).*~~
*Status: fixed.*

## Symptom

```
let t := true in
let f := false in
let a := 5 in
let b := 9 in
let fn h(c, x) => (if c then x + 1 else 0) in
h(t, a) + h(f, b) + h(t, b)
```

Reference evaluator: **16** (6 + 0 + 10). Incremental evaluator: **12**
(6 + 0 + 6). The third call reuses the cached value of `x + 1` from the
*first* call (`x = 5`) instead of computing it for `x = 9`.

## Mechanism

Reuse is justified by an invariant: a `Same` verdict means "unchanged relative
to the previous evaluation", so serving `ptrace`'s cached entry is only sound
if that entry was also produced by the previous evaluation. Strict IF
guaranteed this globally — every node re-bound its eid entry on every pass, so
no entry was ever more than one evaluation old. Lazy IF breaks the guarantee
for the branch that is *not* taken: its `Eid` entries freeze, while the
`FnArg` entries they will later be compared against keep advancing (they are
re-bound on every call).

Timeline for the repro, tracking the then-branch node `x + 1`:

| Call        | cond `c` | branch taken | `FnArg(h, x)` after call | `Eid(x+1)` after call |
|-------------|----------|--------------|--------------------------|------------------------|
| `h(t, a)`   | true     | then         | 5                        | 6 (fresh)              |
| `h(f, b)`   | false    | else         | **9**                    | **6 (stale — dormant)**|
| `h(t, b)`   | true     | then         | 9                        | 6 served — wrong       |

In call 3 the analysis compares the current `x = 9` against `FnArg(h, x) = 9`
from call 2 and concludes `Same`; the early-return path then serves
`Eid(x+1) = 6`, which was computed under `x = 5` in call 1. The comparison
baseline (call 2) and the cached entry (call 1) come from different runs —
that is the whole bug.

## Why this is specific to branch-local eids

Entries reached through a `CALL` do not have this problem. If a function `g`
is invoked only from the dormant branch, then *both* its `FnArg` entries and
its body `Eid` entries freeze together, so they stay mutually consistent: when
the branch is re-taken and the new argument equals the frozen `FnArg` value,
the frozen body entries really were computed under that argument. The
inconsistency arises only for entries keyed by eids lexically inside the
dormant branch, because their baselines (`FnArg` entries of the enclosing
function, sibling eids outside the branch) are refreshed by evaluations the
branch does not participate in.

That containment is exactly what makes the fix below sufficient.

## Fix options

### 1. Range invalidation of the re-taken branch (recommended)

`from_pre2` assigns expression eids in **preorder**, independently for the
root and for each function body. Within one fid's evaluation domain, every
subtree's expression eids form a contiguous range `[lo, hi]`. A nested
`LETFN` body belongs to its own fid and does not advance the enclosing
domain's counter; parameter slots are allocated after the body's expression
range.

At an IF node, the condition is evaluated on every pass, so its `ptrace` entry
is always exactly one run old. Compare it with the current condition value:

- **Previous cond = current cond** — the branch about to be evaluated was also
  taken last run; its entries are one run old and trustworthy. No action.
- **Previous cond ≠ current cond** (or absent) — the branch about to be
  evaluated was dormant last run; every expression entry in its range is at
  least two runs old. Evaluate it with a `ptrace` that masks that range in the
  active fid's trace. The function-encoded `Cache` makes this an O(1) wrapper,
  e.g.:

  ```ocaml
  let mask_range lo hi (C f) =
    C (function
       | Eid i when lo <= i && i <= hi -> None
       | k -> f k)
  ```

  Parameter slots lie after the domain's expression range, and nested
  function bodies use separate traces, so neither is masked by a branch
  range.

The branch ranges are static within their fid. With preorder numbering,
`e2`'s range is `[eid e2, eid e3 - 1]`; `e3`'s range is
`[eid e3, max_eid e3]`, where `max_eid` is the largest same-domain eid in the
subtree. A traversal computing it must skip nested `LETFN` bodies, or it can
be recorded directly from the active domain counter during `from_pre2`.

Cost model: masking only triggers when the condition actually flips, and it
disables reuse only inside the re-taken branch for that one pass — the branch
repopulates its entries as it evaluates, so the pass after that reuses
normally again.

### 2. Speculative strict evaluation with error swallowing

Keep evaluating both branches (restoring entry freshness everywhere) but catch
and discard exceptions from the untaken branch. Staleness then survives only
in the corner where the untaken branch crashed and left no fresh entry.
Downsides: it re-introduces wasted work proportional to the dormant branch,
and if the language's memory operations ever appear in a branch, speculatively
executing the untaken branch is observably wrong — the same reason the IF was
made lazy in the first place. Kept for completeness; option 1 is preferred.

## Related

The same "errors are part of the semantics" decision that motivated lazy IF
also invalidates the remaining value-sensitive **MUL-by-zero** refinements in
`eval_change` (both operand orders): `0 * (100 / x)` and `(100 / x) * 0` with
`x` going `5 → 0` return `0` incrementally where the reference raises
`Division_by_zero`. The DIV `Same, _` zero refinement has already been removed
(verified fixed); the MUL cases are the analogous remaining holes. The
surviving DIV refinement (previous divisor `= 1` and divisor `Same`) is sound:
it never produces `Same`, so it cannot suppress an evaluation that would have
raised.
