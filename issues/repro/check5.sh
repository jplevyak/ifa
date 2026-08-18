#!/bin/bash
# ifa/issues/105 reduction oracle, v3 -- the candidate must be a WORKING
# PYTHON PROGRAM that pyc miscompiles.
#
# v1 checked only for the target error: it produced a 93-line result that
# CPython rejects outright (IndentationError), because pyc's parser
# accepts an empty `if:` body (issues/106).
#
# v2 added ast.parse: it produced a 94-line result that parses but calls
# a dozen undefined names (Edge, Rule, lexical, ...) and, because the
# reduction gutted `__main__`, EXECUTES NOTHING -- 0 bytes of stdout
# against the original's 3456. pyc tolerates undefined names silently, so
# the "reproducer" was pyc inferring over garbage rather than plcfrs's
# actual mechanism.
#
# v4 additionally requires CPython to EXIT 0: v3 accepted a candidate
# that printed 7 bytes and then died of NameError, leaving 10 undefined
# names. rc==0 forces a genuinely working program.
# produces real output -- and only then that pyc still fails.
f="$1"
d=$(dirname "$f")
b=$(basename "$f")

python3 -c "import ast,sys; ast.parse(open(sys.argv[1]).read())" "$f" 2>/dev/null || exit 1

# v5: no UNDEFINED NAMES. CPython resolves names at runtime, so v4 exited
# 0 while still referencing 10 deleted definitions from never-executed
# paths -- which pyc analyses anyway, silently, inferring garbage there.
python3 "$(dirname "$0")/nameck.py" "$f" 2>/dev/null || exit 1

# must run and produce output (guards against reducing away all execution)
o=$(cd "$d" && timeout 60 python3 "$b" 2>/dev/null); rc=$?
[ $rc -eq 0 ] || exit 1
[ -n "$o" ] || exit 1

out=$(cd "$d" && timeout 400 /home/jplevyak/projects/pyc/pyc -D /home/jplevyak/projects/pyc "$b" 2>&1 >/dev/null)
echo "$out" | grep -q "mixes 8- and 1-byte members"
