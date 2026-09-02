#!/usr/bin/env python3
"""ifa/issues/124: reject candidates that ORPHANED an attribute -- the
attribute analogue of nameck.py.

Why this exists. nameck.py stops a reduction deleting a *name* it still
uses (check5.sh's v5 lesson). It does nothing about ATTRIBUTES, and a
reduction walks into the same hole one level down: delete
`Square.set_neighbours` and `square.neighbours` refers to nothing, on a
path the reducer has also made unreachable, so CPython never complains
and pyc analyses the dead code anyway. The "reproducer" then reproduces
pyc inferring over garbage rather than the program's real mechanism.
Measured on go: without any attribute check the first pass introduced 7.

USAGE: attrck.py CANDIDATE [ORIGINAL]

With ORIGINAL -- the authoritative mode, and what the oracle uses. An
attribute is orphaned iff the ORIGINAL defined it (assigned it, or named
a def/class with it), the CANDIDATE still reads it, and the CANDIDATE no
longer defines it. No allowlist, no guessing: the reducer's only job is
to delete, so anything it deleted out from under a surviving use is
drift, by construction.

Without ORIGINAL -- a weaker standalone check against a stdlib
allowlist. Kept for ad-hoc use, but it is NOT sufficient: `Square.find`
collides with `str.find`, so the allowlist exempted a genuine orphan and
a 132-line reduction passed while `neighbour.find()` referred to a
method the reducer had deleted. That is exactly why the ORIGINAL mode
exists -- prefer it.

Exit 0 iff nothing is orphaned.
"""
import ast, builtins, sys

STDLIB = set(dir(builtins)) | {
    "append", "extend", "insert", "pop", "remove", "index", "count", "sort",
    "reverse", "clear", "copy", "keys", "values", "items", "get", "setdefault",
    "update", "add", "discard", "union", "intersection", "difference",
    "split", "rsplit", "join", "strip", "lstrip", "rstrip", "lower", "upper",
    "startswith", "endswith", "replace", "find", "format", "encode", "decode",
    "isdigit", "isalpha", "isspace", "read", "readline", "readlines", "write",
    "writelines", "flush", "close", "seek", "tell",
    "random", "randrange", "randint", "shuffle", "choice", "seed", "uniform",
    "time", "clock", "sqrt", "log", "exp", "sin", "cos", "pi", "floor", "ceil",
    "argv", "stdin", "stdout", "stderr", "exit", "maxsize", "path",
    "setrecursionlimit", "deepcopy", "match", "search", "sub", "group", "groups",
}


def parse(path):
    return ast.parse(open(path).read())


def defined(tree):
    d = set()
    for n in ast.walk(tree):
        if isinstance(n, ast.Attribute) and isinstance(n.ctx, (ast.Store, ast.Del)):
            d.add(n.attr)
        elif isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            d.add(n.name)
    return d


def read(tree, imported):
    u = set()
    for n in ast.walk(tree):
        if not (isinstance(n, ast.Attribute) and isinstance(n.ctx, ast.Load)):
            continue
        if isinstance(n.value, ast.Name) and n.value.id in imported:
            continue  # `mod.thing`: not ours to resolve
        u.add(n.attr)
    return u


def imports(tree):
    i = set()
    for n in ast.walk(tree):
        if isinstance(n, ast.Import):
            for a in n.names:
                i.add((a.asname or a.name).split(".")[0])
        elif isinstance(n, ast.ImportFrom):
            for a in n.names:
                i.add(a.asname or a.name)
    return i


def main(argv):
    cand = parse(argv[1])
    cand_def, cand_read = defined(cand), read(cand, imports(cand))

    if len(argv) > 2:
        orphaned = (cand_read & defined(parse(argv[2]))) - cand_def
    else:
        orphaned = cand_read - cand_def - STDLIB

    if orphaned:
        print("orphaned attributes: " + " ".join(sorted(orphaned)), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
