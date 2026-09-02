#!/bin/bash
# ifa/issues/124 reduction oracle. Same guards as check5.sh -- the
# candidate must PARSE, define every name it uses, and actually RUN under
# CPython producing output -- because pyc analyses code that never
# executes, so a reduction that guts execution "reproduces" pyc inferring
# over garbage rather than the real mechanism (check5.sh's v2/v4 lesson).
#
# The predicate is a COMPILE-TIME signal, not a crash: pyc must still
# report a list constructor that HAS elements and no element type for
# them. That is ifa/123's `path = [node]`, and reducing against the
# diagnostic is far cheaper than reducing against a segfault.
f="$1"
d=$(dirname "$f")
b=$(basename "$f")

python3 -c "import ast,sys; ast.parse(open(sys.argv[1]).read())" "$f" 2>/dev/null || exit 1
python3 "$(dirname "$0")/nameck.py" "$f" 2>/dev/null || exit 1
# v2: and no ORPHANED ATTRIBUTES. nameck stops the reducer deleting a
# NAME it still uses; without the attribute analogue it does the same
# thing one level down -- delete `Square.set_neighbours` and
# `square.neighbours` refers to nothing, on a path the reducer has also
# made unreachable, so CPython never complains and pyc infers over
# garbage. Measured: without this, the first pass on go introduced 7.
# DDMIN_ORIG makes this exact: an attribute the ORIGINAL defined, the
# candidate still reads, and the candidate no longer defines, is drift by
# construction. Without it attrck falls back to a stdlib allowlist, which
# is not enough -- `Square.find` collides with `str.find`, and a 132-line
# reduction passed while `neighbour.find()` referred to a deleted method.
python3 "$(dirname "$0")/attrck.py" "$f" ${DDMIN_ORIG:+"$DDMIN_ORIG"} 2>/dev/null || exit 1

o=$(cd "$d" && timeout 60 python3 "$b" 2>/dev/null); rc=$?
[ $rc -eq 0 ] || exit 1
[ -n "$o" ] || exit 1

out=$(cd "$d" && PYC_DBG_IMPRECISE=1 timeout 300 /home/jplevyak/projects/pyc/pyc \
        -D /home/jplevyak/projects/pyc "$b" 2>&1 >/dev/null)
echo "$out" | grep -q "list's element type untyped"
