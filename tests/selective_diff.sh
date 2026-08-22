#!/bin/bash
# ifa/issues/111 M2 — differential harness for selective invalidation.
#
# Compiles each named program twice, IFA_SELECTIVE=0 then =1, and
# asserts the two runs agree on everything that matters:
#
#   final_pass, violations, ess, css   (FA's converged state, via PYC_DBG_OSC)
#   the emitted C, byte for byte       (what actually ships)
#   the compiler's exit code
#
# Built BEFORE M3 changes any behaviour, on purpose. Selective
# invalidation's failure mode is silent precision drift -- a slightly
# wider type, one fewer contour -- not a crash, so the equivalence check
# has to exist before there is anything to check.
#
# Each program is compiled THREE times: sel=0, sel=0 again, sel=1. The
# repeated sel=0 run is a determinism control, and it is not optional --
# pyc is nondeterministic run-to-run on at least one corpus program
# (msp_ss emits the same 40211 lines with temps renumbered and one
# getter relocated between functions; see ifa/issues/112). On such a
# program the emitted-C comparison is not evidence about the flag, so it
# is reported UNSTABLE and only the FA state is compared. Without this
# control every UNSTABLE program would read as a divergence at M3 and
# the harness would be worse than useless -- it would point at the
# wrong change.
#
# Usage:  ifa/tests/selective_diff.sh [-q] <program>...
#         ifa/tests/selective_diff.sh --corpus
# Exit:   0 all agree, 1 any divergence.

set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
PYC="$ROOT/pyc"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

QUIET=0
[ "${1:-}" = "-q" ] && { QUIET=1; shift; }

if [ "${1:-}" = "--corpus" ]; then
  set -- $(for d in "$ROOT"/shedskin_examples/*/; do
             n=$(basename "$d"); [ -f "$d$n.py" ] && echo "$n"; done)
fi

fail=0 checked=0 skipped=0 unstable=0
for name in "$@"; do
  d="$ROOT/shedskin_examples/$name"
  src="$d/$name.py"
  [ -f "$src" ] || { echo "SKIP    $name (no $name.py)"; skipped=$((skipped+1)); continue; }
  cd "$d" || continue

  # run 0 and 0b are both sel=0: 0b is the determinism control.
  for run in 0 0b 1; do
    sel=${run%b}
    IFA_SELECTIVE=$sel PYC_DBG_OSC=1 timeout 600 "$PYC" -D "$ROOT" "$name.py" \
      > "$TMP/log$run" 2>&1
    echo $? > "$TMP/rc$run"
    # Last OSC line is the converged state; strip the selective= field,
    # which is expected to differ and is only there to prove the flag
    # actually reached FA.
    grep '^OSC' "$TMP/log$run" | tail -1 | sed 's/ selective=[0-9]*//' > "$TMP/osc$run"
    cp "$name.py.c" "$TMP/c$run" 2>/dev/null || : > "$TMP/c$run"
  done

  # Determinism control: do two IDENTICAL invocations agree?
  stable_c=1; stable_fa=1
  cmp -s "$TMP/c0"   "$TMP/c0b"   || stable_c=0
  cmp -s "$TMP/osc0" "$TMP/osc0b" || stable_fa=0

  # ...and if they did agree but sel=1 did not, ESCALATE before calling
  # it a divergence. ifa/issues/112's nondeterminism is INTERMITTENT: two
  # sel=0 runs of msp_ss can coincidentally match, which made a single
  # control report it as a divergence caused by the flag. Only pay for
  # the extra runs when there is actually something to explain.
  if [ $stable_c = 1 ] && ! cmp -s "$TMP/c0" "$TMP/c1"; then
    for extra in x y; do
      IFA_SELECTIVE=0 timeout 600 "$PYC" -D "$ROOT" "$name.py" >/dev/null 2>&1
      cp "$name.py.c" "$TMP/c$extra" 2>/dev/null || : > "$TMP/c$extra"
      cmp -s "$TMP/c0" "$TMP/c$extra" || stable_c=0
    done
  fi

  # A program that does not compile under BOTH settings is not evidence
  # either way -- report it rather than counting it as agreement.
  rc0=$(cat "$TMP/rc0"); rc1=$(cat "$TMP/rc1")
  if [ "$rc0" != 0 ] && [ "$rc1" != 0 ] && [ "$rc0" = "$rc1" ]; then
    [ $QUIET = 1 ] || echo "SKIP    $name (does not compile either way, rc=$rc0)"
    skipped=$((skipped+1)); continue
  fi

  why=""
  [ "$rc0" = "$rc1" ]            || why="$why rc($rc0/$rc1)"
  # FA state is compared only when it is itself reproducible.
  if [ $stable_fa = 1 ]; then
    cmp -s "$TMP/osc0" "$TMP/osc1" || why="$why fa-state"
  fi
  # Emitted C is compared only when this program emits reproducibly.
  if [ $stable_c = 1 ]; then
    cmp -s "$TMP/c0" "$TMP/c1"     || why="$why emitted-C"
  fi

  checked=$((checked+1))
  if [ -n "$why" ]; then
    fail=$((fail+1))
    echo "DIVERGE $name --$why"
    echo "         sel=0: $(cat "$TMP/osc0")"
    echo "         sel=1: $(cat "$TMP/osc1")"
    cmp -s "$TMP/c0" "$TMP/c1" || \
      echo "         C differs: $(diff "$TMP/c0" "$TMP/c1" | grep -c '^[<>]') changed lines"
  elif [ $stable_c = 0 ] || [ $stable_fa = 0 ]; then
    unstable=$((unstable+1))
    u=""
    [ $stable_fa = 0 ] && u="$u fa-state"
    [ $stable_c  = 0 ] && u="$u emitted-C"
    echo "UNSTABLE $name --$u varies across IDENTICAL runs (ifa/issues/112); \
flag comparison inconclusive for those"
  else
    [ $QUIET = 1 ] || echo "AGREE   $name  $(cat "$TMP/osc0" | sed 's/^OSC //')"
  fi
done

echo
echo "---- selective_diff ----"
echo "  compared:  $checked"
echo "  diverged:  $fail"
echo "  skipped:   $skipped"
echo "  unstable:  $unstable  (nondeterministic compiler, not the flag -- ifa/issues/112)"
[ $fail = 0 ] && echo "  RESULT: agree" || echo "  RESULT: DIVERGENCE"
exit $([ $fail = 0 ] && echo 0 || echo 1)
