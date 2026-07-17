# 046 — Optional-None list-of-self field crashes inline_prim_chain (Type_SUM assert)

**Status:** FIXED 2026-07-17 (fix sketch point 1): inline.cc's
chain inliner now pre-checks substitutions
(`prim_chain_substitution_safe`) and leaves the call un-inlined when
both an actual's and a formal's types are Type_SUM, instead of
asserting mid-mutation. The minimal repro below compiles AND runs
correctly (the deepcopy work in issues/029 gave the nil arms typed
methods). Point 2 (field narrowing) remains open as a precision
item.

Original report follows.

**Status (original):** open. Minimal repro below; found 2026-07-15 while
diagnosing genetic2 (pyc issues/025 R1 item 5), which has since
moved past this exact crash (post recursive-ES splitting it fails
later, in codegen) — but the micro still reproduces the assert
standalone, so the underlying hole is live.

## Symptom

```
pyc: optimize/inline.cc:407: auto inline_prim_chain(Fun *, PNode *,
Fun *, Vec<PNode *> *)::(anonymous class)::operator()(PNode *,
PNode *, int) const: Assertion `v->type->type_kind != Type_SUM' failed.
```

Compiler abort (SIGABRT), no diagnostics before it.

## Minimal repro (10 lines)

```python
class Node:
    def __init__(self, value, children=None):
        self.value = value
        self.children = children

def make():
    return Node(1, [Node(2, None), Node(3, None)])

n = make()
print(n.children[0].value)
```

The shape: a class field holding `Optional[list-of-same-class]` —
`children` is `None` on leaves and a list of `Node` on interior
nodes, so the field's type is the sum `{nil, list}` and the list's
element type recursively involves the class itself.

## Analysis pointers

`inline_prim_chain` (optimize/inline.cc) asserts every involved
Var's type is not a `Type_SUM` — this shape delivers exactly that:
the `children` field read feeding a prim chain (`index_object` /
`period`) has the unresolved `{nil, list}` sum because nothing
narrows it before use here. Related families:

- The "optional-None fields" inference bucket (pyc issues/025
  tail-dig: loop / softrender / pygmy / lz2, and genetic2's
  TreeNode.args) — same source shape, different downstream
  manifestations.
- ifa/issues/030 (tagged dispatch) / issue 011 (exceptions) discuss
  what a *sound* runtime story for a genuinely-mixed value would be.
- The `x is None` / `isinstance` narrowing machinery (fa.cc issue
  025 blocks) can kill the None member per-branch when the source
  GUARDS the access; this repro doesn't guard, and CPython would
  crash at runtime on the None path too — but the compiler must
  fail with a diagnostic, not an assert.

## Fix sketch

Two independent pieces:
1. **Don't crash:** inline_prim_chain should skip (not assert on)
   sum-typed chain members, leaving the send un-inlined so the
   normal violation reporting fires ("expression has mixed basic
   types" / unresolved member), matching how other passes degrade.
2. **Precision (separate, optional):** narrow `{nil, T}` fields at
   guarded uses (`if x.children:` / `is not None`) so the sum never
   reaches codegen in well-guarded programs — that's the existing
   issue-025 narrowing extended to field reads, or 043's per-CS
   contour work.

## Verification

- Micro above compiles to a clean diagnostic (or, with narrowing,
  runs) instead of asserting; both backends.
- genetic2 progresses (its later blockers are separate).
- Suites stay green.
