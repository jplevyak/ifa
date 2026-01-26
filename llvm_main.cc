/* -*-Mode: c++;-*- */
#define EXTERN
#define EXTERN_INIT(_x) = _x
#include "ifadefs.h"
#include "num.h" // Ensures num globals defined
#include "num.h" // Ensures num globals defined

// Stubs for missing globals/functions from other files
#include "log.h"
// Header defines alog via EXTERN

#include "ifalog.h"
// Header defines log_dir, log_tag via EXTERN

// Config
#include "config.h"
// Header defines config_filenames via EXTERN

// C Codegen stubs
// ifa.cc is in LLVM_BACKEND_SRCS.
void c_codegen_write_c(FA*, Fun*, const char*) {}
int c_codegen_compile(const char*) { return 0; }

#include "ir_serialize.h"
#include "llvm.h"
#include "fa.h"
#include "dead.h"
#include "dom.h"

int main(int argc, char **argv) {

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <ir_file>\n", argv[0]);
        return 1;
    }

    // Initialize minimal environment
    if1 = new IF1(); 
    // prim_init happens in IF1 constructor
    
    // Set builtin types manually if needed? 
    // deserialize handles symbols, but builtins?
    // Symbols for builtins like "int" are serialized.
    // Pointers in if1 (like sym_int) might be null?
    // We might need to map them back if LLVM uses global sym pointers (e.g. sym_string).
    // Serialized Syms have names. We can look them up?
    // But `sym_string` is a global variable.
    // Deserializer allocates new Syms. Global pointer `sym_string` is still null.
    // If backend uses `sym_string` directly -> crash.
    // We should fix global pointers after deserialization.
    
    fprintf(stderr, "DEBUG: Creating PDB and FA\n");
    PDB *pdb = new PDB(if1);
    FA *fa = new FA(pdb);
    pdb->fa = fa;

    fprintf(stderr, "DEBUG: Deserializing IR from %s\n", argv[1]);
    deserialize_ir(argv[1], fa);
    fprintf(stderr, "DEBUG: Deserialization complete, %d functions\n", fa->funs.n);

    // Mark live code - this sets live flags on PNodes and Vars
    fprintf(stderr, "DEBUG: Building CFG dominators\n");
    forv_Fun(f, fa->funs) {
        fprintf(stderr, "DEBUG: build_cfg_dominators for %s (id %d), entry=%p\n",
                f->sym->name ? f->sym->name : "<unnamed>", f->sym->id, f->entry);
        if (f->entry) build_cfg_dominators(f);
    }
    fprintf(stderr, "DEBUG: Marking live code and functions\n");
    fprintf(stderr, "DEBUG: if1->top = %p (%s, id %d), top->fun = %p\n",
            if1->top, if1->top ? if1->top->name : "<null>",
            if1->top ? if1->top->id : -1,
            if1->top ? if1->top->fun : NULL);
    mark_live_code(fa);
    mark_live_funs(fa);
    fprintf(stderr, "DEBUG: Live marking complete\n");
    fprintf(stderr, "DEBUG: After marking, top->fun->live = %d\n",
            if1->top && if1->top->fun ? if1->top->fun->live : -1);

    // Fixup global symbols if possible
    // Iterate all syms, if name matches "string", set sym_string?
    for (int i = 0; i < if1->allsyms.n; i++) {
        Sym *s = if1->allsyms[i];
        if (!s->name) continue;
        if (strcmp(s->name, "string") == 0) sym_string = s;
        else if (strcmp(s->name, "void") == 0) sym_void = s;
        // Add others as needed by key builtins in builtin.h
        // This is hacky but necessary if logic relies on pointer equality.
        // llvm.cpp uses sym_void, sym_string, sym_int, etc.
    }
    
    // Main Fun
    Fun *main_fun = nullptr;
    if (if1->top) {
        // Find Fun wrapper in FA
        forv_Fun(f, fa->funs) {
            if (f->sym == if1->top) {
                main_fun = f;
                break;
            }
        }
        // If not found (maybe deserialize didn't create Fun wrapper for top?), create it
        if (!main_fun) {
             main_fun = new Fun(if1->top);
             fa->pdb->add(main_fun);
        }
    }
    
    if (main_fun) {
        fprintf(stderr, "DEBUG: Found main_fun %p (sym id %d)\n", main_fun, main_fun->sym->id);
        llvm_codegen_write_ir(fa, main_fun, argv[1]);
        // Also compile to .o
        llvm_codegen_compile(argv[1]);
    } else {
        fprintf(stderr, "No main function found in IR\n");
        return 1;
    }

    return 0;
}
