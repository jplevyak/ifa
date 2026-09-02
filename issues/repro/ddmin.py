#!/usr/bin/env python3
"""ifa/issues/105: line-granularity delta reduction of plcfrs against the
invariant "still fails with `mixes 8- and 1-byte members`".

Greedy chunk removal: try to drop contiguous blocks, largest first,
keeping any drop that preserves the failure. Indentation-aware only in
the crude sense that a syntactically broken candidate simply fails the
invariant (pyc reports a parse error, not the field-size error) and is
rejected, so correctness is enforced by the oracle rather than by
understanding Python structure.
"""
import subprocess, sys, os, shutil

R = os.path.dirname(os.path.abspath(__file__))
# Parameterised so the same reducer serves other issues: DDMIN_CHECK is
# the oracle, DDMIN_CAND a scratch candidate path (it must sit beside any
# data files the program needs), DDMIN_OUT the result. Defaults are 105's.
CHECK = os.environ.get("DDMIN_CHECK", os.path.join(R, "check5.sh"))
CAND = os.environ.get("DDMIN_CAND", os.path.join(R, "cand.py"))


def fails(lines):
    with open(CAND, "w") as f:
        f.write("".join(lines))
    return subprocess.call(["bash", CHECK, CAND],
                           stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL) == 0


def reduce(lines):
    n = len(lines) // 2
    while n >= 1:
        i = 0
        while i < len(lines):
            trial = lines[:i] + lines[i + n:]
            if trial and fails(trial):
                lines = trial
                print(f"  -{n:4d} lines -> {len(lines)}", flush=True)
            else:
                i += n
        n //= 2
    return lines


if __name__ == "__main__":
    src = sys.argv[1]
    lines = open(src).readlines()
    print(f"start: {len(lines)} lines", flush=True)
    assert fails(lines), "invariant does not hold on the input"
    out = reduce(lines)
    dst = os.environ.get("DDMIN_OUT", os.path.join(R, "reduced5.py"))
    with open(dst, "w") as f:
        f.write("".join(out))
    print(f"done: {len(out)} lines -> {dst}", flush=True)
