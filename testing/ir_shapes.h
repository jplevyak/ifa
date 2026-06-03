// SPDX-License-Identifier: BSD-3-Clause
//
// Layer 3 of the IR builder: whole-program shape generators used
// as synthetic test fixtures. See
// ifa/testing/phases/09d_generator_design.md §2.
//
// Each shape function populates the global `if1` with a complete
// program (closures, types, sym___main__-rooted entry) ready to
// hand off to the phase's `run(if1)` step. Phase printers don't
// distinguish synthetic from `.ir`-loaded fixtures.

#ifndef IFA_TESTING_IR_SHAPES_H
#define IFA_TESTING_IR_SHAPES_H

#include "ifadefs.h"

#include <unordered_map>
#include <string>

namespace IRShape {

using ParamMap = std::unordered_map<std::string, int>;
using ShapeFn = void (*)(const ParamMap &);

// Look up a registered shape by name; returns null if unknown.
ShapeFn lookup(cchar *name);

// Read a param from the map with a default if missing.
int param(const ParamMap &m, cchar *name, int fallback = 0);

// ---------------------------------------------------------------------------
// Registered shapes
// ---------------------------------------------------------------------------
//
// Each shape takes a ParamMap (parsed from the .synth fixture) and
// populates if1. Set if1->top to the user-entry closure; the test
// harness's fa_setup_environment splices it into __main__.

// noop_main: a single closure that does nothing but reply. Used to
// validate the runner end-to-end without exercising any FA paths.
// No parameters.
void noop_main(const ParamMap &);

// polymorphic_formal: a closure `f(a, b)` returning its first arg.
// Main calls f with constants of `n_types` distinct types
// (cycling through int32, float64, int64), `n_per_type` calls
// each type. Stresses the FA type-stage splitter — f's formals
// see a type confluence; extend_analysis must split f's ES.
//
// Params: n_types (1-3), n_per_type (>=1). With (2, 1) this
// matches the shape of 02_splitter.ir.
void polymorphic_formal(const ParamMap &);

// same_type_dispatch: a record type T with one field. Builds
// `n_allocs` allocations of T (same type, distinct CSes), each
// storing a *different-typed* value in the field. A reader fn
// `peek(t)` returns t.field; main calls peek on each allocation.
//
// Per 09c, this targets the mark-type stage: the formal of peek
// has same type T (no ES-level type confluence to split), but the
// field's CS-contour AVar has mixed types across CSes. Whether
// mark-type actually fires (vs the type stage absorbing the
// confluence first) is what the golden documents.
//
// Params: n_allocs (>=2, <=3). With 2 allocs this is the same
// shape as 13_setter_split.ir (which currently triggers type, not
// setter/mark-type).
void same_type_dispatch(const ParamMap &);

// nested_iterator: V (outer vector) holds inner V's. Re-attempted
// with method dispatch through __getitem__/__setitem__ rather than
// raw primitives — per PRIMITIVES.md §13.12, the method-dispatch
// path gives the splitter per-CS specialization opportunity that
// primitive-direct doesn't, which avoids the clone-phase
// "mismatched field sizes" failure the raw-primitive version hit.
void nested_iterator(const ParamMap &);

// iterator_copy: copy elements between two vectors via two
// iterators. Builds on the vector_iterator breakthrough — tries
// to extend to setter-of-setter by chaining: read from src
// iterator → write to dst via set_index_object. Polymorphism
// flows src CSes → element AVar → dst element AVar.
//
// Targets setter-of-setter (the cascade form of setter).
void iterator_copy(const ParamMap &);

// iterator_missing_field: iterator yields elements that may or may
// not have a particular field. Builds the vector_iterator pattern
// but with V's elements being records of disjoint types (A has
// field fa, B has field fb). Reader accesses .fa on each yielded
// element — fails for B-element vectors.
//
// Targets the violation stage by routing the type-violation through
// the iterator-mediated dispatch chain.
void iterator_missing_field(const ParamMap &);

// vector_iterator: mirrors pyc's list-runtime iterator pattern.
// A vector type V, an iterator record It with `vec` + `pos`
// fields, an iterator `next` method that reads self.vec and
// indexes into it. Main allocates two V's with different-typed
// elements; for each, builds an It bound to that V and calls
// next(it). The polymorphism lives in V's per-CS element AVar
// AND propagates through It's vec field — exactly the split_css
// trigger pattern pyc programs hit.
//
// This is the "real" split_css attempt — see issue 007 for the
// background and previous absorbed iterations.
void vector_iterator(const ParamMap &);

// vector_element_polymorphism: targets split_css — the only
// post-type splitter path pyc programs reach in practice (via
// list runtime). A vector-marked type V; n_allocs instances
// of V, each having different-typed values written to its
// element slot via set_index_object. Reader function reads
// the element back via index_object.
//
// Per 09c §"Stage 3c", the trigger is AVars with cs_map
// populated where setters' containers overlap with the cs_map's
// CSes. Vector element AVars are CS-mapped.
//
// Params: n_allocs (>=2, <=3).
void vector_element_polymorphism(const ParamMap &);

// missing_field_dispatch: two record types with disjoint fields.
// A polymorphic reader reads a field that exists in one type
// but not the other → type violation on the missing-field path.
//
// Per 09c's sketch for the violation stage: when the call site is
// polymorphic and the dispatch fails for some receiver types,
// splitting the caller specializes each context to a single type
// and resolves the violation.
//
// Whether the violation stage actually fires (vs the type stage
// splitting the reader's formal first) is what the golden documents.
//
// Params: none (always builds two types A, B with one field each).
void missing_field_dispatch(const ParamMap &);

// setter_chain: cascading setter writes where field A's value
// becomes field B's setter input. Two RECORD types R1 and R2 in
// a chain `r1.a → v → r2.b`. Done twice with distinct types
// (int and float) so the chain carries polymorphic values.
//
// Per 09c, this targets setter-of-setter. Whether it actually
// fires (vs the type stage absorbing the polymorphism first) is
// what the golden documents.
//
// Params: n_types (1-3). With 2 types, mirrors the 09c sketch.
void setter_chain(const ParamMap &);

// stored_fn_dispatch: a deeper variant of same_type_dispatch
// targeting mark-type via a stored-function-pointer pattern.
//
// Builds: record T with one field `fn`. Builds N small closures
// f0(), f1(), ... each returning a different-typed constant.
// Allocates N T instances, each storing a different fi in its
// `fn` field. A dispatcher closure `call_via(t)` does
// `t.fn()` — calling whatever closure is stored in t's field.
// Main calls call_via on each allocation.
//
// Hypothesis: the call dispatch through stored function pointers
// creates a mark-distinguishable resolution that mark-type can
// split. If it still falls to the type stage, that's a finding
// (the mark mechanism may not engage on this pattern either).
//
// Params: n_allocs (>=2, <=3).
void stored_fn_dispatch(const ParamMap &);

}  // namespace IRShape

#endif  // IFA_TESTING_IR_SHAPES_H
