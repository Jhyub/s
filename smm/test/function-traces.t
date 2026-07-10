  $ ../bin/main.exe stale-then.s-- 2>/dev/null
  16

  $ ../bin/main.exe stale-else.s-- 2>/dev/null
  16

  $ ../bin/main.exe unchanged.s-- 2>/dev/null
  18

  $ ../bin/main.exe unchanged.s-- 2>&1 >/dev/null | grep -c 'Reuse hit:.*expr=IF'
  2

  $ ../bin/main.exe nested.s-- 2>/dev/null
  28

  $ ../bin/main.exe nested.s-- 2>&1 >/dev/null | grep -c 'Reuse hit:.*expr=VAR'
  1

  $ ../bin/main.exe nested.s-- 2>&1 >/dev/null | grep -c 'Reuse hit:.*expr=CALL'
  1

  $ ../bin/main.exe closure-activation.s-- 2>/dev/null
  3

  $ ../bin/main.exe dormant-letfn-activation.s-- 2>/dev/null
  3

  $ ../bin/main.exe closure-activation-exception.s-- 2>&1 >/dev/null | grep -F 'Error: division by zero'
  Error: division by zero

  $ ../bin/main.exe same-closure-activation.s-- 2>/dev/null
  4

  $ ../bin/main.exe same-closure-activation.s-- 2>&1 >/dev/null | grep -c 'Reuse hit:.*expr=ADD'
  1
