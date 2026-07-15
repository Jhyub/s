  $ ./run_benchmarks.exe --calls 8 --samples 1 --warmups 0 --output-dir generated --csv results.csv > table.txt 2> progress.txt

  $ sed -n '1p' results.csv
  benchmark,source,calls,seed,expected,samples,warmups,smm_pre_median_seconds,smm_pre_min_seconds,smm_pre_max_seconds,smm_pre_median_allocated_bytes,smm_pre_rss_before_bytes,smm_pre_peak_rss_bytes,smm_pre_peak_growth_bytes,smm_median_seconds,smm_min_seconds,smm_max_seconds,smm_median_allocated_bytes,smm_rss_before_bytes,smm_peak_rss_bytes,smm_peak_growth_bytes,speedup_smm_pre_over_smm,allocated_ratio_smm_pre_over_smm,peak_rss_ratio_smm_pre_over_smm,peak_growth_ratio_smm_pre_over_smm,ocaml_version,timestamp_utc

  $ wc -l < results.csv
  24

  $ find generated -name '*.s--' | wc -l
  23

  $ find generated -name 'complex_*.s--' | wc -l
  7

  $ grep -c '^arithmetic_linear,' results.csv
  1

  $ grep -c '^nested_random,' results.csv
  1

  $ grep -c 'CSV: results.csv' table.txt
  1

  $ grep -c 'Generated 23 workloads in generated' progress.txt
  1

  $ grep -c '^complex_64_inline_constant,' results.csv
  1

  $ grep -c '^\[23/23\] complex_64_bursty_eight_runs: Smm$' progress.txt
  1

  $ awk '/\[Smm.eval\]/{ count++ } END { print count + 0 }' progress.txt
  0
