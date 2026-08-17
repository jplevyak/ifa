#!/bin/bash
# Which corpus programs COMPILE successfully and then fail at RUNTIME?
# Every sweep in this session checked the compiler's exit code only; the
# harness likewise stops at the first failing stage. A binary that builds
# and then segfaults is invisible to both.
OUT="$1"; : > "$OUT"
cd /home/jplevyak/projects/pyc/shedskin_examples || exit 1
for d in */; do
  p="${d%/}"; f="$p/$p.py"; [ -f "$f" ] || continue
  rm -f "$p/$p"
  timeout 300 /home/jplevyak/projects/pyc/pyc -D /home/jplevyak/projects/pyc "$f" >/dev/null 2>&1
  crc=$?
  if [ $crc -ne 0 ] || [ ! -x "$p/$p" ]; then
    echo "$p compile_rc=$crc run_rc=- " >> "$OUT"; continue
  fi
  (cd "$p" && timeout 60 ./"$p" >/dev/null 2>&1)
  rrc=$?
  echo "$p compile_rc=$crc run_rc=$rrc" >> "$OUT"
done
echo DONE >> "$OUT"
