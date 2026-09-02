# ifa/issues/124 -- MINIMAL REPRO, reduced from shedskin_examples/go
# (635 lines -> 10) against the `--refuse-imprecise` diagnostic.
#
#   PYC_DBG_IMPRECISE=1 pyc -D <root> 124-comprehension-index-untypes-list.py
#   => "inference left a list's element type untyped (_CG_void)"  on `[self]`
#
# `path = [self]` is a one-element literal holding exactly one class, and
# its element type still comes out untyped, so codegen has to guess a
# layout for it. In go that guess picks the wrong class and corrupts a
# field -- ifa/issues/123.
#
# Three variants pin the trigger. Only the third reports:
#
#   path.append(self.kids[0])              constant index         NO
#   moves() returns [0, 1, 2, 3]           function-returned list NO
#   moves() returns [p for p in range(4)]  COMPREHENSION          YES
#
# So it is not `path = [self]`, not the class, and not the
# dynamically-added attribute (`go`'s `unexplored`) -- those were all
# tried and none of them reproduces on its own. What is required is that
# the INDEX flows from a comprehension built in another function.
#
# CPython prints 2. pyc also prints 2 here: this is imprecision, not yet
# a miscompile -- go needs the additional layout divergence to crash.


def moves():
    return [p for p in range(4)]


class N:
    def __init__(self):
        self.kids = [None for x in range(4)]

    def play(self):
        path = [self]
        path.append(self.kids[moves()[0]])
        return len(path)


print(N().play())
