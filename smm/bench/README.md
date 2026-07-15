# Smm evaluator benchmarks

This suite compares `Smm_pre.run` with the change-aware `Smm.eval`. It generates
large S-- source programs instead of using loops: every generated call is a leaf
of a balanced addition tree, so every returned value contributes to the final
result and left-to-right call order is preserved.

## Run the suite

Run the default 4,096-call, 23-workload suite as optimized native executables:

```sh
dune exec --profile release bench/run_benchmarks.exe
```

The command prints a comparison table, writes generated programs and their
manifest to `_build/bench/generated`, and writes the summary to
`_build/bench/results.csv`.

For a quick smoke run or a custom experiment:

```sh
dune exec --profile release bench/run_benchmarks.exe -- \
  --calls 128 --samples 3 --warmups 1 --seed 20260710 \
  --output-dir _build/bench/generated \
  --csv _build/bench/results.csv
```

Generate the source programs without running them:

```sh
dune exec --profile release bench/generate.exe -- \
  --calls 4096 --seed 20260710 --output-dir _build/bench/generated
```

`--calls` is the number of contributing outer `f(...)` calls in each program.
Nested-call workloads execute additional helper calls internally.

## Workloads

Six workloads use the required `f(x) = x / 10 * 5 + 2` pipeline with linear,
random-permutation, constant, repeated-eight, alternating-extremes, and
equal-quotient bucket-shuffled input orders. Ten more cover ordered and random
branches, captured constants, stable and changing second arguments, nested
helper calls, and heavier arithmetic with local intermediate bindings.

Seven workloads isolate the cost of reusing a more expensive function body.
Their staged kernel starts with the call argument and defines each subsequent
value with `v_i = (v_(i-1) * 17 + i) % 1_000_003`; stage 1 therefore reads the
original input as `v_0`. The generated S-- spells every stage as a nested `let`
binding. Four helper-call variants run 16, 24, 32, or 64 stages with constant
input `100`. A 64-stage inline control puts the identical kernel directly in
`f`, while a 32-stage linear-miss control uses unique ascending inputs. The
64-stage burst control has eight contiguous constant-input runs, each containing
512 calls at the default 4,096-call size. These controls make cache crossover
points visible without treating machine-dependent timing as a correctness
condition.

Generation is deterministic for a given call count and seed, and all random
variants share the same permutation within a suite. Each generated manifest
records the expected aggregate result, and both evaluators must match it before
a worker reports measurements.

## Measurement boundaries

- Parsing, `Smm_pre.from_pre2`, and `Smm.from_pre` complete before measurement.
- Timing brackets exactly one top-level `Smm_pre.run` or
  `Smm.eval ~debug:false` call. Major GC, validation, and formatting occur
  outside that bracket.
- The reported runtime is the median wall time of seven samples by default;
  minimum and maximum samples are included as context. Each call starts with
  fresh evaluator state.
- Allocated bytes are the median `Gc.allocated_bytes` delta across the same
  samples. This is allocation traffic, not retained or peak memory.
- Peak RSS comes from a separate fresh worker that prepares the AST, performs a
  major GC, and evaluates exactly once. Whole-worker peak RSS therefore includes
  the prepared AST. `peak growth` is any new high-water mark established by the
  eval after setup.
- RSS uses Linux `/proc/self/status`. On platforms without it, RSS columns are
  reported as `NA`; timing and allocation measurements still work.

Speedup and memory ratios are reported as `Smm_pre / Smm`, so values above 1.0
favor the change-aware evaluator.
