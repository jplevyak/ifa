/* -*-Mode: c++;-*- */
#include "ir_serialize.h"
#include <vector>
#include <map>
#include <string>
#include <stdlib.h>
#include <string.h>
#include "fa.h"
#include "prim.h"
#include "pattern.h"
#include "fun.h"
#include "var.h"

// Debug logging - set IR_DESERIALIZE_DEBUG to 1 to enable
#ifndef IR_DESERIALIZE_DEBUG
#define IR_DESERIALIZE_DEBUG 0
#endif

#if IR_DESERIALIZE_DEBUG
#define DEBUG_LOG(...) fprintf(stderr, "DEBUG: " __VA_ARGS__)
#else
#define DEBUG_LOG(...) do {} while(0)
#endif

// Forward decls
extern IF1 *if1;

/**
 * Reads a length-prefixed string from the file.
 * Allocates memory that should be freed by the caller or attached to a Sym.
 * Returns nullptr for length 0.
 */
static char* read_string(FILE *fp) {
    unsigned long len;
    if (fscanf(fp, " %lu", &len) != 1) return nullptr;
    if (len == 0) return nullptr;
    char *buf = (char*)malloc(len + 1);
    char space;
    fread(&space, 1, 1, fp); // skip space
    fread(buf, 1, len, fp);
    buf[len] = '\0';
    return buf; // User must free or attach to Sym
}

void serialize_ir(const char *filename, IF1 *if1, FA *fa) {
    fail("serialize_ir called in backend!");
}

/**
 * Deserializes IR from a file and populates IF1 and FA structures.
 * Reads symbols, labels, code nodes, and function definitions.
 * Handles forward references for function calls.
 *
 * @param filename Path to the IR file to deserialize
 * @param fa Flow analysis context to populate (can be NULL)
 */
void deserialize_ir(const char *filename, FA *fa) {
    FILE *fp = fopen(filename, "r");
    if (!fp) fail("Unable to open IR file %s", filename);
    
    char buf[256];
    if (fscanf(fp, "%255s", buf) != 1 || strcmp(buf, "IFA_IR_V1") != 0) {
        fail("Invalid IR file header");
    }

    int n_syms = 0;
    if (fscanf(fp, " SYMS %d", &n_syms) != 1) {
        fail("Failed to read SYMS header");
    }

    // Allocate Syms (IDs are 0 to n_syms-1)
    std::vector<Sym*> syms(n_syms);

    
    // Pass 1: Create Syms to allow circular refs
    for (int i = 0; i < n_syms; i++) {
        Sym *s = new Sym(); // Basic new, no GC
        s->id = i; // Will be overwritten but correct
        syms[i] = s;
        if1->allsyms.add(s);
    }
    
    // Pass 2: Fill Syms
    for (int i = 0; i < n_syms; i++) {
        Sym *s = syms[i];
        int id, kind, type_id, flags;
        fscanf(fp, " SYM %d %d", &id, &kind);
        if (id != i) { fprintf(stderr, "Sym ID mismatch: expected %d, got %d\n", i, id); exit(1); }
        s->id = id;
        s->type_kind = (Type_kind)kind;
        
        char *name = read_string(fp);
        if (name) s->name = name; // Leaks?
        
        fscanf(fp, " %d", &type_id);
        if (type_id >= 0 && type_id < n_syms) s->type = syms[type_id];
        
        fscanf(fp, " %u", &flags);
        if (flags & 1) s->is_local = 1;
        if (flags & 2) s->is_constant = 1;
        if (flags & 4) s->is_external = 1;
        if (flags & 8) s->is_fun = 1;
        
        // Constant value
        if (s->is_constant) {
            int num_kind;
            fscanf(fp, " %d", &num_kind);
            s->num_kind = num_kind;
            if (num_kind == IF1_NUM_KIND_NONE) {
                 char *cstr = read_string(fp);
                 s->constant = cstr;
            } else {
                 if (num_kind == IF1_NUM_KIND_INT) {
                     long long val; fscanf(fp, " %lld", &val);
                     s->imm.v_int64 = val;
                 } else if (num_kind == IF1_NUM_KIND_FLOAT) {
                     double val; fscanf(fp, " %lf", &val);
                     s->imm.v_float64 = val;
                 } else {
                     int dummy; fscanf(fp, " %d", &dummy);
                 }
            }
        } else {
            int dummy; fscanf(fp, " %d", &dummy);
        }
        
        // Has
        int n_has;
        fscanf(fp, " %d", &n_has);
        for(int j=0; j<n_has; j++) {
            int hid; fscanf(fp, " %d", &hid);
            if(hid >=0 && hid < n_syms) s->has.add(syms[hid]);
        }
        
        int ret_id; fscanf(fp, " %d", &ret_id);
        if(ret_id >= 0 && ret_id < n_syms) s->ret = syms[ret_id];
        
        int elem_id; fscanf(fp, " %d", &elem_id);
        if(elem_id >= 0 && elem_id < n_syms) s->element = syms[elem_id];
        
        int ni; fscanf(fp, " %d", &ni); s->num_index = ni;
    }
    
    // LABELS
    int n_labels = 0;
    if (fscanf(fp, " LABELS %d", &n_labels) != 1) {
        fail("Failed to read LABELS header");
    }
    std::vector<Label*> labels(n_labels);

    DEBUG_LOG("n_labels = %d\n", n_labels);
    for (int i = 0; i < n_labels; i++) {
         int id = -1; 
         int res = fscanf(fp, " LABEL %d", &id);
         if (res != 1) fprintf(stderr, "Failed LABEL scan at i=%d\n", i);
         Label *l = new Label();
         l->id = id;
         labels[i] = l; // id should match index ideally
         if1->alllabels.add(l);
    }
    
    // CODES
    int n_codes = -1;
    if (fscanf(fp, " CODES %d", &n_codes) != 1) {
        fail("Failed to read CODES header");
    }
    // Code IDs were 1-based in serialization map.
    // Vector size n_codes + 1.
    std::vector<Code*> codes(n_codes + 1);
    
    // Pass 1: Create Codes
    long codes_start_pos = ftell(fp);
    for (int i = 0; i < n_codes; i++) {
        int id, kind;
        int res = fscanf(fp, " CODE %d %d", &id, &kind);
        if (res != 2) { fprintf(stderr, "Scan failed at i=%d\n", i); break; }
        if (id < 0 || id > n_codes) { fprintf(stderr, "Bad ID %d > %d\n", id, n_codes); exit(1); }
        Code *c = new Code((Code_kind)kind);
        codes[id] = c;
        
        // Skip rest of line
        int n;
        fscanf(fp, " %d", &n); // rvals
        for(int j=0; j<n; j++) { int tmp; fscanf(fp, " %d", &tmp); }
        fscanf(fp, " %d", &n); // lvals
        for(int j=0; j<n; j++) { int tmp; fscanf(fp, " %d", &tmp); }
        int l1, l2;
        fscanf(fp, " %d %d", &l1, &l2); // labels
        fscanf(fp, " %d", &n); // sub
        for(int j=0; j<n; j++) { int tmp; fscanf(fp, " %d", &tmp); }
        int pidx, line;
        fscanf(fp, " %d %d", &pidx, &line);
    }
    
    // Fill Codes
    fseek(fp, codes_start_pos, SEEK_SET);
    for (int i = 0; i < n_codes; i++) {
        int id, kind;
        fscanf(fp, " CODE %d %d", &id, &kind);
        Code *c = codes[id]; // Already created
        
        int n;
        fscanf(fp, " %d", &n); // rvals
        for(int j=0; j<n; j++) { 
            int sid; fscanf(fp, " %d", &sid);
            if(sid >=0 && sid < n_syms) c->rvals.add(syms[sid]);
        }
        
        fscanf(fp, " %d", &n); // lvals
        for(int j=0; j<n; j++) { 
            int sid; fscanf(fp, " %d", &sid);
            if(sid >=0 && sid < n_syms) c->lvals.add(syms[sid]);
        }
        
        int l1, l2;
        fscanf(fp, " %d %d", &l1, &l2);
        if(l1 >= 0 && l1 < n_labels) { c->label[0] = labels[l1]; labels[l1]->code = c; } // Fixup label->code?
        if(l2 >= 0 && l2 < n_labels) { c->label[1] = labels[l2]; }
        
        fscanf(fp, " %d", &n); // sub
        for(int j=0; j<n; j++) {
             int cid; fscanf(fp, " %d", &cid);
             if(cid > 0 && cid <= n_codes) c->sub.add(codes[cid]);
        }
        
        int pidx;
        fscanf(fp, " %d", &pidx);
        if (pidx > 0) {
            // Map index to Prim*
            // This relies on Primitives::prims vector being populated in same order.
             if (if1->primitives && if1->primitives->prims.n > pidx) {
                 // Index in prims might not match Enum index directly everywhere?
                 // But Prim constructor assigns index.
                 // We should search for it? Or assume sorted?
                 // Let's search.
                 // bool found = false;
                 forv_Prim(p, if1->primitives->prims) {
                     if (p->index == pidx) { c->prim = p; break; }
                 }
                 // If not found, user default or null
             }
        }
        
        int line;
        fscanf(fp, " %d", &line);
        // c->line = line; // Code::line() usually delegates to AST. 
        // We don't have AST. 
        // Can we set line another way? 
        // Code struct doesn't have line field, it calls method.
        // But llvm.cpp needs line.
        // We'd need to mock AST or add line field to Code if we want debug info.
        // For now, ignore.
    }
    
    // Map to unify functions by name (handle alias IDs)
    std::map<std::string, Fun*> fun_by_name;

    // FUNS
    int n_funs;
    fscanf(fp, " FUNS %d", &n_funs);
    for (int i = 0; i < n_funs; i++) {
        int sid, cid;
        fscanf(fp, " FUN %d %d", &sid, &cid);
        if(sid >= 0 && sid < n_syms) {
            Sym *f = syms[sid];
            if(cid > 0 && cid <= n_codes) f->code = codes[cid];
            if1->allclosures.add(f);
            
            // Add to FA?
            if (fa) {
                Fun *fun = f->fun;
                        fun = new Fun(f);
                        DEBUG_LOG("Deserialized NEW Fun: %p '%s' (id %d)\n", fun, f->name, sid);
                        fa->pdb->add(fun);
                        fa->funs.add(fun); // Add to fa->funs so iterators find it
                
                // Only build CFG if not already done (or just always do it if it's the main definition?)
                // Forward refs created bare wrappers. We need to fill content.
                if (!fun->entry) {
                     fun->build_cfg();  // Rebuild PNodes and CFG from Code
                     if (fun->entry) fun->live = 1; // Mark live if CFG built
                }
                
                int n_args;
                fscanf(fp, " %d", &n_args);
                for (int j = 0; j < n_args; j++) {
                     int sym_id; fscanf(fp, " %d", &sym_id);
                     if (sym_id >= 0 && sym_id < n_syms) {
                         Sym *arg_sym = syms[sym_id];
                         // Synthesize MPosition
                         MPosition *pos = new MPosition();
                         pos->push(j + 1); // 1-based index usually? Or 0? pattern.cc uses 1-based usually
                         // But llvm.cpp writes `arg_idx < ifa_fun->positional_arg_positions.n`
                         // Let's use 1-based to be safe with MPosition conventions if any, 
                         // but llvm.cpp iteration is just over the vector elements.
                         // The actual pos internal value doesn't matter much unless used as key.
                         
                         fun->positional_arg_positions.add(pos);
                         if (arg_sym && arg_sym->var) {
                             fun->args.put(pos, arg_sym->var);
                         } else if (arg_sym) {
                             // Create var if missing (unlikely if deserialize creates vars for all syms)
                             Var *v = new Var(arg_sym);
                             fun->args.put(pos, v);
                         }
                     }
                }
                
                int n_rets;
                fscanf(fp, " %d", &n_rets);
                for (int j = 0; j < n_rets; j++) {
                     int sym_id; fscanf(fp, " %d", &sym_id);
                     if (sym_id >= 0 && sym_id < n_syms) {
                         Sym *ret_sym = syms[sym_id];
                         if (ret_sym && ret_sym->var) {
                             fun->rets.add(ret_sym->var);
                         } else if (ret_sym) {
                             // Create var if missing
                             // Note: Var(Sym*) sets sym->var = this.
                             // But we might have created it already for args?
                             // Also update Var type? Var constructor does it.
                             Var *v = new Var(ret_sym);
                             fun->rets.add(v);
                         }
                     }
                }
                
                // Read Calls Map
                // Note: Forward references are handled by creating placeholder Fun objects
                // and linking them via fun_by_name map. The main loop will fill in their content.
                int n_calls;
                fscanf(fp, " %d", &n_calls);
                for(int k=0; k<n_calls; k++) {
                    int cid, tid;
                    fscanf(fp, " %d %d", &cid, &tid);
                    if (cid > 0 && cid <= n_codes && tid >= 0 && tid < n_syms) {
                        Code *c = codes[cid];
                        PNode *pn = c ? c->pn : NULL;  // Back pointer set by build_cfg
                        Sym *tsym = syms[tid];

                         Fun *target_fun = NULL;
                         if (tsym->fun) {
                             target_fun = tsym->fun;
                             DEBUG_LOG("Calls map %p found EXISTING ref to %s %p\n", fun, tsym->name, target_fun);
                         } else {
                             if (tsym->name && fun_by_name.count(tsym->name)) {
                                 target_fun = fun_by_name[tsym->name];
                                 tsym->fun = target_fun;
                                 DEBUG_LOG("Calls map %p found LINKED ref to '%s' %p\n", fun, tsym->name, target_fun);
                             } else {
                                 // Forward reference. Create shell.
                                 target_fun = new Fun(tsym);
                                 if (tsym->name) {
                                     fun_by_name[tsym->name] = target_fun;
                                     DEBUG_LOG("Added forward ref '%s' to map\n", tsym->name);
                                 }
                                 DEBUG_LOG("Calls map %p created NEW FORWARD ref to '%s' %p (id %d)\n", fun, tsym->name ? tsym->name : "(null)", target_fun, tsym->id);
                                 fa->pdb->add(target_fun);
                                 fa->funs.add(target_fun);
                                 // Do NOT call build_cfg yet. Main loop will fill it.
                             }
                         }
                         
                         if (pn && target_fun) {
                             Vec<Fun*> *fv = new Vec<Fun*>();
                             fv->add(target_fun);
                             fun->calls.put(pn, fv);
                         }
                    }
                }

                int is_varargs, is_external;
                fscanf(fp, " %d %d", &is_varargs, &is_external);
                fun->is_varargs = is_varargs;
                fun->is_external = is_external;
                char *cgs = read_string(fp);
                if (cgs) fun->cg_string = cgs;
            } else {
                 // Skip args if fa not present (should generally not happen in backend)
                 int n_args; fscanf(fp, " %d", &n_args);
                 for (int j = 0; j < n_args; j++) { int tmp; fscanf(fp, " %d", &tmp); }

                 int n_rets; fscanf(fp, " %d", &n_rets);
                 for (int j = 0; j < n_rets; j++) { int tmp; fscanf(fp, " %d", &tmp); }

                 int n_calls; fscanf(fp, " %d", &n_calls);
                 for (int j = 0; j < n_calls; j++) { int tmp1, tmp2; fscanf(fp, " %d %d", &tmp1, &tmp2); }

                 int tmp1, tmp2;
                 fscanf(fp, " %d %d", &tmp1, &tmp2); // is_varargs, is_external
                 read_string(fp); // skip cg_string
            }
        }
    }
    
    // ENTRY
    int entry_id;
    fscanf(fp, " ENTRY %d", &entry_id);
    if(entry_id >= 0 && entry_id < n_syms) if1->top = syms[entry_id];
    
    // Create Vars for all Syms (needed by llvm.cpp)
    for (int i = 0; i < if1->allsyms.n; i++) {
        Sym *s = if1->allsyms[i];
        if (s && !s->var) {
            new Var(s); // Constructor sets s->var = this
        }
    }

    fclose(fp);
}
