/* -*-Mode: c++;-*- */
#include "ir_serialize.h"
#include <map>
#include <vector>
#include <string>
#include <functional>
#include <string.h>  // For strlen
#include "pattern.h"
#include "fun.h"
#include "var.h"
#include "sym.h"
#include "fa.h"

// Debug logging - set IR_SERIALIZE_DEBUG to 1 to enable
#ifndef IR_SERIALIZE_DEBUG
#define IR_SERIALIZE_DEBUG 0
#endif

#if IR_SERIALIZE_DEBUG
#define DEBUG_LOG(...) fprintf(stderr, "DEBUG: " __VA_ARGS__)
#else
#define DEBUG_LOG(...) do {} while(0)
#endif

/**
 * Writes a length-prefixed string to the file.
 * Format: "<length> <string_content>"
 * Null strings are written as "0"
 */
static void write_string(FILE *fp, const char *s) {
    if (!s) { fprintf(fp, " 0"); return; }
    fprintf(fp, " %lu %s", strlen(s), s);
}

static std::map<Code*, int> code_map;
static int code_id_counter = 1;

static int get_code_id(Code *c) {
    if (!c) return 0;
    if (code_map.find(c) == code_map.end()) {
        code_map[c] = code_id_counter++;
    }
    return code_map[c];
}

/**
 * Recursively collects all live Code nodes and assigns them IDs.
 * Only processes live code nodes, skipping dead ones.
 */
static void collect_code(Code *c) {
    if (!c || code_map.count(c)) return;
    if (!c->live) return;

    get_code_id(c);

    for (int i = 0; i < c->sub.n; i++) collect_code(c->sub[i]);
}

static int get_new_id(Sym *s, std::map<int, int> &map) {
    if (!s) return 0;
    auto it = map.find(s->id);
    if (it != map.end()) return it->second;
    // If not found, it means it's dead or filtered out. Return -1 (null)
    return -1; 
}

static void fixup_pnode_prim(FA *fa);

void serialize_ir(const char *filename, IF1 *if1, FA *fa) {
    fixup_pnode_prim(fa);
    FILE *fp = fopen(filename, "w");
    if (!fp) {
        fail("Unable to open IR file %s", filename);
        return;
    }
    
    fprintf(fp, "IFA_IR_V1\n");

    // Map old Sym ID -> New Sym ID (0-based contiguous)
    std::map<int, int> sym_id_map;
    int new_sym_id_counter = 0;
    
    // Always include system/builtin symbols if they are not marked live?
    // Usually they are marked live if used.
    // Let's trust s->live.
    // Recursive helper to add live symbols and their dependencies
    std::function<void(Sym*)> add_live_sym;
    add_live_sym = [&](Sym *s) {
        if (!s || sym_id_map.count(s->id)) return;
        sym_id_map[s->id] = new_sym_id_counter++;
        
        // Dependencies
        if (s->type) add_live_sym(s->type);
        if (s->ret) add_live_sym(s->ret);
        if (s->element) add_live_sym(s->element);
        
        // For Type Definitions, 'has' list is structural
        if (s->type_kind != Type_VARIABLE && s->type_kind != Type_UNKNOWN) {
             for(int j=0; j<s->has.n; j++) add_live_sym(s->has[j]);
        }
    };

    for (int i = 0; i < if1->allsyms.n; i++) {
        Sym *s = if1->allsyms[i];
        if (s->live) {
            add_live_sym(s);
        }
    }

    // 1. Collect all LIVE Code nodes to assign IDs
    code_map.clear();
    code_id_counter = 1;
    for (int i = 0; i < if1->allclosures.n; i++) {
        Sym *s = if1->allclosures[i];
        if (s->live && s->code) {
             collect_code(s->code);
        }
    }

    // 2. Symbols
    // Format: SYM <id> <kind> <name> <type_id> <flags...>
    fprintf(fp, "SYMS %d\n", new_sym_id_counter);
    
    std::vector<Sym*> sorted_syms(new_sym_id_counter);
    for (int i = 0; i < if1->allsyms.n; i++) {
        Sym *s = if1->allsyms[i];
        if (sym_id_map.count(s->id)) {
            sorted_syms[sym_id_map[s->id]] = s;
        }
    }

    for (Sym *s : sorted_syms) {
        if (!s) { // Should not happen if logic is correct
             fprintf(stderr, "CRITICAL ERROR: Symbol gap in sorted_syms\n");
             continue;
        }
        
        // Use mapped ID
        int id = sym_id_map[s->id];
        
        fprintf(fp, "SYM %d %d", id, s->type_kind);
        write_string(fp, s->name);
        fprintf(fp, " %d", get_new_id(s->type, sym_id_map));
        
        // Flags
        unsigned flags = 0;
        if (s->is_local) flags |= 1;
        bool treat_constant = s->is_constant || s->num_kind != IF1_NUM_KIND_NONE;
        if (treat_constant) flags |= 2;
        if (s->is_external) flags |= 4;
        if (s->is_fun) flags |= 8;
        fprintf(fp, " %u", flags);

        // Constant values (Basic)
        if (treat_constant) {
            fprintf(fp, " %d", s->num_kind); // Kind of numeric
             // Dump immediate or string const
             if (s->num_kind == IF1_NUM_KIND_NONE && s->constant) {
                 // String constant
                 write_string(fp, (const char*)s->constant);
             } else {
                 if (s->num_kind == IF1_NUM_KIND_INT) fprintf(fp, " %lld", (long long)s->imm.v_int64);
                 else if (s->num_kind == IF1_NUM_KIND_FLOAT) fprintf(fp, " %.17g", s->imm.v_float64); // Full precision
                 else if (s->num_kind == IF1_NUM_KIND_UINT) fprintf(fp, " %llu", (unsigned long long)s->imm.v_uint64);
                 else fprintf(fp, " 0"); // Placeholder
             }

        } else {
            fprintf(fp, " 0"); // No constant data
        }
        
        // Sym Extras
        // Has list - Filter dead children?
        // cg.cc implies components of live syms are live.
        // But let's filter safely just in case.
        // Actually, if a child is dead, we can't reference it by ID (it won't exist).
        std::vector<int> live_has;
        for(int j=0; j<s->has.n; j++) {
            if (s->has[j]->live) live_has.push_back(get_new_id(s->has[j], sym_id_map));
        }

        fprintf(fp, " %d", (int)live_has.size());
        for(int hid : live_has) fprintf(fp, " %d", hid);
        
        // Ret type
        fprintf(fp, " %d", get_new_id(s->ret, sym_id_map));

        // Element type (for arrays/refs)
        fprintf(fp, " %d", get_new_id(s->element, sym_id_map));
        
        // Num index (for primitives)
        fprintf(fp, " %d", s->num_index);

        fprintf(fp, "\n");
    }

    // 3. Labels
    // Filter dead labels? cg.cc has l->live logic.
    std::vector<Label*> live_labels;
    for (int i = 0; i < if1->alllabels.n; i++) {
        if (if1->alllabels[i]->live) live_labels.push_back(if1->alllabels[i]);
    }

    fprintf(fp, "LABELS %d\n", (int)live_labels.size());
    // Output labels with contiguous IDs (0..N-1) for indexing in deserializer
    for (size_t i = 0; i < live_labels.size(); i++) {
        fprintf(fp, "LABEL %d\n", (int)i);
    }
    // We need map for labels too for Code references
    std::map<Label*, int> label_id_map;
    for(size_t i=0; i<live_labels.size(); i++) label_id_map[live_labels[i]] = i;

    
    // 4. Code (PNodes)
    std::vector<Code*> sorted_codes(code_id_counter); // 1-based
    for(auto const& [ptr, id] : code_map) {
        if(id < (int)sorted_codes.size()) sorted_codes[id] = ptr;
    }

    fprintf(fp, "CODES %d\n", code_id_counter - 1);
    for(int i=1; i < code_id_counter; i++) {
        Code *c = sorted_codes[i];
        if(!c) continue; 
        
        fprintf(fp, "CODE %d %d", i, (int)c->kind);
        
        // Rvals - filter dead? 
        // Code usually uses live vars. If s is dead, should we skip?
        // Code using dead rval is effectively dead or broken.
        // But we already filtered dead Code nodes.
        // So surviving Code nodes should only point to live Syms?
        // Verify.
        fprintf(fp, " %d", c->rvals.n);
        for(int j=0; j<c->rvals.n; j++) fprintf(fp, " %d", get_new_id(c->rvals[j], sym_id_map));
        
        // Lvals
        fprintf(fp, " %d", c->lvals.n);
        for(int j=0; j<c->lvals.n; j++) fprintf(fp, " %d", get_new_id(c->lvals[j], sym_id_map));
        
        // Labels (map to new index)
        int l1_idx = (c->label[0] && label_id_map.count(c->label[0])) ? label_id_map[c->label[0]] : -1;
        int l2_idx = (c->label[1] && label_id_map.count(c->label[1])) ? label_id_map[c->label[1]] : -1;

        fprintf(fp, " %d %d", l1_idx, l2_idx);
        
        // Children (IDs should be valid as we collected only live code)
        fprintf(fp, " %d", c->sub.n);
        for(int j=0; j<c->sub.n; j++) fprintf(fp, " %d", get_code_id(c->sub[j])); 
        
        // Primitives
        fprintf(fp, " %d", c->prim ? c->prim->index : 0);
        
        // Source info
        fprintf(fp, " %d", c->line());
        
        fprintf(fp, "\n");
    }

    // 5. Functions (Closures)
    std::vector<Sym*> live_funs;
    
    // Use FA for liveness if available (matches cg.cc logic)
    if (fa && fa->funs.n > 0) {
        DEBUG_LOG("ir_serialize fa->funs.n = %d\n", fa->funs.n);
        forv_Fun(f, fa->funs) {
            DEBUG_LOG("Inspecting Fun %p, sym=%p\n", f, f ? f->sym : nullptr);

            if (f && f->sym) {
                 DEBUG_LOG("Adding live fun %s\n", f->sym->name);
                 live_funs.push_back(f->sym);
            }
        }
    } else {
        // Fallback to allclosures if FA not available
        for(int i=0; i<if1->allclosures.n; i++) {
            if (if1->allclosures[i]->live) live_funs.push_back(if1->allclosures[i]);
        }
    }
    
    fprintf(fp, "FUNS %ld\n", live_funs.size());
    for (Sym *s : live_funs) {
        DEBUG_LOG("Serializing Fun %s\n", s->name ? s->name : "<anon>");
        fprintf(fp, "FUN %d %d", get_new_id(s, sym_id_map), get_code_id(s->code));
        
        Fun *f = NULL;
        if (fa && fa->funs.n > 0) {
             forv_Fun(fx, fa->funs) {
                 if (fx->sym == s) { f = fx; break; }
             }
        }
        
        if (f) {
            // Args
            std::vector<Var*> live_args;
            forv_MPosition(p, f->positional_arg_positions) {
                Var *v = f->args.get(p);
                // Check v->live ? cg.cc checks v->live.
                if (v && v->live) live_args.push_back(v);
            }

            fprintf(fp, " %ld", live_args.size());
            for(Var *v : live_args) {
                 fprintf(fp, " %d", get_new_id(v->sym, sym_id_map));
            }
            // Rets
            std::vector<Var*> live_rets;
            forv_Var(v, f->rets) {
                if(v && v->live) live_rets.push_back(v);
            }

            fprintf(fp, " %ld", live_rets.size());
            for(Var *v : live_rets) {
                 fprintf(fp, " %d", get_new_id(v->sym, sym_id_map));
            }
            
            // Calls Map
            // f->calls maps PNode* -> Vec<Fun*>*
            // We need to map PNode back to CodeID for serialization
            // Only serialize calls for live targets
            
            int call_count = 0;
            // First count valid entries
             for(int k=0; k<f->calls.n; k++) {
                 if (f->calls.v[k].key) { // Check key for existence
                     PNode *pn = (PNode*)f->calls.v[k].key;
                     if (pn && pn->code && code_map.count(pn->code)) {
                          if (pn->prim) continue;
                          Vec<Fun*> *targets = f->calls.v[k].value;
                          if (targets && targets->n == 1) {
                              call_count++;
                          }
                     }
                 }
            }

            fprintf(fp, " %d", call_count);
            for(int k=0; k<f->calls.n; k++) {
                 if (f->calls.v[k].key) {
                     PNode *pn = (PNode*)f->calls.v[k].key;
                     if (pn && pn->code && code_map.count(pn->code)) {
                          if (pn->prim) continue;
                          Vec<Fun*> *targets = f->calls.v[k].value;
                          if (targets && targets->n == 1) {
                              Fun *target = targets->v[0];
                              // Need target sym ID
                              int tid = get_new_id(target->sym, sym_id_map);
                              int cid = code_map[pn->code];

                              if (tid == -1) {
                                  DEBUG_LOG("Serialize skipping call to %s (id %d) because sym not in map\n", target->sym->name, target->sym->id);
                              } else {
                                  fprintf(fp, " %d %d", cid, tid);
                              }
                          } else {
                               DEBUG_LOG("Serialize skipping call due to targets->n=%d\n", targets ? targets->n : 0);
                          }
                     }
                 }
            }
            // Is VarArgs?
             fprintf(fp, " %d", f->is_varargs);
             // Is External?
             fprintf(fp, " %d", f->is_external);
             // CG String
             write_string(fp, f->cg_string);

        } else {
             // Default if no Fun object found
             fprintf(fp, " 0 0 0 0"); 
        }
        fprintf(fp, "\n");
    }
    
    // 6. Entry Point
    fprintf(fp, "ENTRY %d\n", get_new_id(if1->top, sym_id_map));

    fclose(fp);
    fprintf(stderr, "Serialized IR to %s\n", filename);
}

/**
 * Fixes divergence between PNode->prim and Code->prim before serialization.
 * Updates Code nodes in-place to ensure serialization captures the correct primitive.
 */
static void fixup_pnode_prim(FA *fa) {
    if (!fa) return;
    forv_Fun(f, fa->funs) {
        if (!f) continue;

        forv_PNode(p, f->fa_all_PNodes) {
            if (p && p->code) {
                if (p->prim && p->prim != p->code->prim) {
                     // Divergence detected - update Code in-place
                     // (cloning would break parent->child links in Code graph)
                     p->code->prim = p->prim;
                     DEBUG_LOG("Fixed PNode prim divergence for prim %d\n", p->prim->index);
                }
            }
        }
    }
}

void deserialize_ir(const char *filename, FA *fa) {
    fail("deserialize_ir called in frontend!");
}
