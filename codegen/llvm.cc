#include "llvm_internal.h"
#include "builtin.h"
#include "cg.h"
#include "codegen_common.h"
#include "fail.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <stdarg.h>
#include <sstream>

// DEBUG_LOG moved to llvm_internal.h; runtime-gated on ifa_debug.

// ============================================================================
// Global LLVM State (definitions)
// ============================================================================

std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::DIBuilder> DBuilder;
llvm::DICompileUnit *CU = nullptr;
llvm::DIFile *UnitFile = nullptr;
Vec<Fun *> *all_funs_global = NULL;

// Forward declarations of file-scope caches reset by
// llvm_codegen_initialize. Definitions live near their use sites
// later in this file. See AUDIT §4 / CODEGEN_PLAN §3.1.
static std::map<std::string, llvm::Constant *> string_constants_map;
static std::map<Fun *, std::vector<PNode *>> reverse_call_graph;

static void llvm_codegen_initialize(FA *fa) {
  // Initialize LLVM components
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  // llvm::InitializeAllAsmParsers(); // Not strictly needed for IR generation to .ll
  llvm::InitializeAllAsmPrinters();  // Needed for object file emission if done via PassManager

  // Tear down any previous-call state in reverse-dependency order.
  // The naive `TheContext = make_unique<...>()` assigns the new Context
  // first, which destroys the old Context — and then `TheModule = ...`
  // tries to destroy the old Module, which dereferences the just-freed
  // Context. Explicit reset() in reverse order keeps every destructor
  // running while its dependencies are still alive. CU and UnitFile
  // are raw pointers owned by DBuilder; null them so a re-entry that
  // doesn't go through createCompileUnit again can't dereference
  // dangling memory.
  CU = nullptr;
  UnitFile = nullptr;
  DBuilder.reset();
  Builder.reset();
  TheModule.reset();
  TheContext.reset();

  // File-scope caches whose entries are pointers into the just-destroyed
  // TheContext / TheModule. Clearing them here makes lookups in the new
  // run start cold and removes the use-after-free risk that would otherwise
  // bite on the second invocation. See AUDIT §4 / CODEGEN_PLAN §3.1.
  string_constants_map.clear();
  extern std::map<Label *, llvm::BasicBlock *> label_to_bb_map;  // llvm_codegen.cc
  label_to_bb_map.clear();
  reverse_call_graph.clear();

  TheContext = std::make_unique<llvm::LLVMContext>();
  // Use a more descriptive module ID, perhaps from FA or filename
  cchar *mod_id = fa && fa->pdb && fa->pdb->if1 && fa->pdb->if1->filename ? fa->pdb->if1->filename : "ifa_output";
  TheModule = std::make_unique<llvm::Module>(mod_id, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  DBuilder = std::make_unique<llvm::DIBuilder>(*TheModule);

  // Set target triple for the module
  std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(llvm::Triple(TargetTriple));

  // Enable PIC for position-independent code generation
  TheModule->setPICLevel(llvm::PICLevel::BigPIC);
  TheModule->setPIELevel(llvm::PIELevel::Default);
  // Optionally, set data layout
  // std::string Error;
  // auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
  // if (Target) {
  //   auto TM = Target->createTargetMachine(TargetTriple, "generic", "", llvm::TargetOptions(),
  //   llvm::Optional<llvm::Reloc::Model>()); TheModule->setDataLayout(TM->createDataLayout());
  // } else {
  //   fail("Could not lookup target: %s", Error.c_str());
  // }
}

// is_closure_var(Var*) — moved to codegen_common.{h,cc}.

static std::string getTypeName(llvm::Type *Ty) {
  if (!Ty) return "null";
  std::string str;
  llvm::raw_string_ostream rso(str);
  Ty->print(rso);
  return rso.str();
}

// Helper to convert IF1 numeric types to LLVM IntegerType or FloatingPointType
static llvm::Type *mapNumericType(Sym *sym) {
  if (!TheContext) {
    fail("LLVM Context not initialized in mapNumericType");
    return nullptr;
  }
  switch (sym->num_kind) {
    case IF1_NUM_KIND_UINT:
    case IF1_NUM_KIND_INT:
      switch (sym->num_index) {
        case IF1_INT_TYPE_1:
          return llvm::Type::getInt1Ty(*TheContext);  // bool
        case IF1_INT_TYPE_8:
          return llvm::Type::getInt8Ty(*TheContext);
        case IF1_INT_TYPE_16:
          return llvm::Type::getInt16Ty(*TheContext);
        case IF1_INT_TYPE_32:
          return llvm::Type::getInt32Ty(*TheContext);
        case IF1_INT_TYPE_64:
          return llvm::Type::getInt64Ty(*TheContext);
        default:
          fail("Unknown integer type index: %d", sym->num_index);
          return nullptr;
      }
    case IF1_NUM_KIND_FLOAT:
      switch (sym->num_index) {
        case IF1_FLOAT_TYPE_32:
          return llvm::Type::getFloatTy(*TheContext);
        case IF1_FLOAT_TYPE_64:
          return llvm::Type::getDoubleTy(*TheContext);
        case IF1_FLOAT_TYPE_128:
          return llvm::Type::getFP128Ty(*TheContext);
        default:
          fail("Unknown float type index: %d", sym->num_index);
          return nullptr;
      }
    default:
      fail("Unknown numeric kind: %d", sym->num_kind);
      return nullptr;
  }
}

// Forward Declarations

// Reverse Call Graph for Constant Recovery (definition above, near
// the other reset-on-init globals).

// Forward declaration for get_target_fun

// Discover all reachable functions by walking the call graph from main
// This ensures all functions are known before liveness analysis
static void discover_all_reachable_functions(FA *fa, Fun *main_fun, Vec<Fun *> &all_funs) {
  if (!fa || !main_fun) return;

  DEBUG_LOG("discover_all_reachable_functions starting from %s\n",
          main_fun->sym->name ? main_fun->sym->name : "(null)");

  std::set<Fun *> visited;
  std::vector<Fun *> worklist;

  // Start with main
  if (main_fun->live && main_fun->entry) {
    worklist.push_back(main_fun);
    visited.insert(main_fun);
  }

  // Add all functions already in fa->funs (only if live)
  int fun_idx = 0;
  for (Fun *f : fa->funs) {
    if (f) {
      DEBUG_LOG("  fa->funs[%d]: %s (id %d) live=%d\n", fun_idx, f->sym->name ? f->sym->name : "(null)",
              f->sym->id, f->live);
      if (f->live) {
        if (visited.find(f) == visited.end()) {
          worklist.push_back(f);
          visited.insert(f);
        }
      }
    }
    fun_idx++;
  }

  // Walk the call graph
  while (!worklist.empty()) {
    Fun *current = worklist.back();
    worklist.pop_back();

    // Add to all_funs
    all_funs.set_add(current);

    DEBUG_LOG("Discovered function %s (id %d), calls.n=%d\n",
            current->sym->name ? current->sym->name : "(null)", current->sym->id, current->calls.n);

    // Walk through all call sites in this function
    for (int k = 0; k < current->calls.n; k++) {
      if (current->calls.v[k].key) {
        Vec<Fun *> *targets = current->calls.v[k].value;

        if (targets) {
          for (Fun *target_fun : *targets) {
            if (target_fun && visited.find(target_fun) == visited.end()) {
              DEBUG_LOG("  Found call to %s (id %d)\n",
                      target_fun->sym->name ? target_fun->sym->name : "(null)", target_fun->sym->id);
              worklist.push_back(target_fun);
              visited.insert(target_fun);
            }
          }
        }
      }
    }
  }

  DEBUG_LOG("discover_all_reachable_functions found %d functions\n", all_funs.n);
}

static void build_reverse_call_graph(FA *fa) {
  reverse_call_graph.clear();
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    for (int k = 0; k < f->calls.n; k++) {
      if (f->calls.v[k].key) {
        PNode *pn = (PNode *)f->calls.v[k].key;
        Vec<Fun *> *targets = f->calls.v[k].value;
        if (targets && targets->n == 1) {
          Fun *target = targets->v[0];
          reverse_call_graph[target].push_back(pn);
        }
      }
    }
  }
}

static llvm::Value *recover_constant_arg(Var *var, Fun *ifa_fun) {
  if (!var || !ifa_fun || !ifa_fun->sym) return nullptr;

  // Find index in 'has' list
  int arg_idx = -1;
  for (int i = 0; i < ifa_fun->sym->has.n; i++) {
    if (ifa_fun->sym->has[i] == var->sym) {
      arg_idx = i;
      break;
    }
  }
  if (arg_idx == -1) return nullptr;  // Not an argument

  // Check callers
  auto it = reverse_call_graph.find(ifa_fun);
  if (it == reverse_call_graph.end() || it->second.empty()) return nullptr;

  Sym *consistent_sym = nullptr;
  for (PNode *caller : it->second) {
    // Map arg_idx to caller rvals
    // Assuming rvals[i] maps to has[i]
    // Code_SEND rvals[0] is the Target/Self.
    // has[0] is the first formal parameter (Self).
    // has[1] is the second formal parameter (Arg 1).
    // So indices match directly.

    int call_rval_idx = arg_idx;
    if (call_rval_idx >= caller->rvals.n) return nullptr;  // Call has too few args

    Var *passed_var = caller->rvals[call_rval_idx];
    if (!passed_var || !passed_var->sym) return nullptr;

    // Resolve to constant Sym
    Sym *s = passed_var->sym;
    bool is_const = s->is_constant || s->num_kind != IF1_NUM_KIND_NONE;
    if (!is_const) {
      // Not constant?
      DEBUG_LOG("Recovery: caller passes non-constant %s\n", s->name);
      return nullptr;
    }

    if (!consistent_sym)
      consistent_sym = s;
    else if (consistent_sym != s) {
      // Mismatch between callers
      return nullptr;
    }
  }

  if (consistent_sym) {
    DEBUG_LOG("Recovered constant %s for arg %s\n", consistent_sym->name ? consistent_sym->name : "?",
            var->sym->name);
    // Create a heap-allocated Var to wrap the Sym (using GC)
    // Stack allocation would create dangling pointers
    Var *tmp = new (UseGC) Var(consistent_sym);
    tmp->type = consistent_sym->type;  // Set the type
    return getLLVMConstant(tmp);
  }
  return nullptr;
}

// ============================================================================
// getLLVMType helpers (phase-4 decomposition)
//
// getLLVMType used to be ~230 lines doing the cross-product of all
// type_kinds. Now it's a thin driver over per-kind helpers. Each
// helper is responsible for one type family.
// ============================================================================

// Builtin singletons (void, string) and numeric kinds. Returns nullptr
// if `sym` doesn't fall into one of these — caller dispatches by
// type_kind.
static llvm::Type *mapBuiltinOrNumeric(Sym *unaliased_sym) {
  if (unaliased_sym == sym_void || unaliased_sym == sym_void_type) {
    return llvm::Type::getVoidTy(*TheContext);
  }
  if (unaliased_sym == sym_string) {
    // String represented as opaque pointer (i8*).
    return llvm::PointerType::getUnqual(*TheContext);
  }
  if (unaliased_sym->num_kind != IF1_NUM_KIND_NONE) {
    return mapNumericType(unaliased_sym);
  }
  return nullptr;
}

// Type_RECORD vector variant (is_vector flag set): IF1 vectors are
// dynamically sized; we represent them as opaque pointers and let the
// runtime helpers (`_CG_prim_*`) carry the actual layout.
static llvm::Type *mapVectorType(Sym *unaliased_sym) {
  if (!unaliased_sym->element || !unaliased_sym->element->type) {
    fail("Vector type %s has no element type",
         unaliased_sym->name ? unaliased_sym->name : "(anon)");
    return nullptr;
  }
  if (!getLLVMType(unaliased_sym->element->type)) {
    fail("Could not get LLVM type for element of vector type %s",
         unaliased_sym->name ? unaliased_sym->name : "(anon)");
    return nullptr;
  }
  return llvm::PointerType::getUnqual(*TheContext);
}

// Type_RECORD struct variant: build an `llvm::StructType` with one
// LLVM Type per IF1 field. Sets sym->llvm_type to the opaque type
// before recursing into fields, so cyclic references break cleanly.
// Void/null field types are substituted with i8 to keep struct
// layouts well-formed (see AUDIT §1 #1).
static llvm::Type *mapStructType(Sym *sym, Sym *unaliased_sym) {
  llvm::StructType *struct_type = llvm::StructType::create(
      *TheContext,
      unaliased_sym->name ? unaliased_sym->name : ("struct.anon" + std::to_string(unaliased_sym->id)));
  sym->llvm_type = struct_type;
  unaliased_sym->llvm_type = struct_type;

  std::vector<llvm::Type *> field_types;
  for (int i = 0; i < unaliased_sym->has.n; ++i) {
    Sym *field_sym = unaliased_sym->has[i];
    llvm::Type *field_llvm_type = nullptr;
    if (field_sym && field_sym->type) field_llvm_type = getLLVMType(field_sym->type);
    if (!field_llvm_type || field_llvm_type->isVoidTy()) {
      // i8 placeholder for void/null fields keeps StructLayout safe
      // and field-indexing positional. See AUDIT §1 #1 / phase-3.0.
      DEBUG_LOG("mapStructType: field %s (index %d) of %s is void/null; substituting i8\n",
                field_sym && field_sym->name ? field_sym->name : "(anon)", i,
                unaliased_sym->name ? unaliased_sym->name : "(anon)");
      field_llvm_type = llvm::Type::getInt8Ty(*TheContext);
    }
    field_types.push_back(field_llvm_type);
  }
  if (struct_type->isOpaque()) struct_type->setBody(field_types);
  return struct_type;
}

// Type_FUN: function values are opaque pointers. The signature is
// re-derived from the IF1 Sym at call sites that need it (via the
// `llvm::Function*` cached on `Fun::llvm`, which carries its own
// `FunctionType`). Under opaque pointers (LLVM 15+) every function
// pointer is `ptr` regardless of signature — so we never need to
// reify the signature here.
//
// We still walk ret + args opportunistically to warm the type cache
// (so the structural information is available for debug info and
// for cases that do need a concrete struct field type), but the
// walk is best-effort: lazily-populated `ret` / arg type slots are
// no longer an error, they just skip the cache priming.
static llvm::Type *mapFunctionType(Sym *unaliased_sym) {
  if (Sym *ret_type_sym = unaliased_sym->ret) {
    if (ret_type_sym->type) (void)getLLVMType(ret_type_sym->type);
  }
  for (int i = 0; i < unaliased_sym->has.n; ++i) {
    Sym *arg_sym = unaliased_sym->has[i];
    if (arg_sym && arg_sym->type) (void)getLLVMType(arg_sym->type);
  }
  return llvm::PointerType::getUnqual(*TheContext);
}

// Type_SUM: collapses the common `T | nil` shape to T's LLVM type
// (when T is a pointer); falls back to opaque pointer for any other
// shape. Full tagged-union representation is future work.
static llvm::Type *mapSumType(Sym *unaliased_sym) {
  if (unaliased_sym->has.n == 2) {
    Sym *t1 = unaliased_sym->has[0]->type;
    Sym *t2 = unaliased_sym->has[1]->type;
    Sym *non_nil = (t1 == sym_nil_type) ? t2 : ((t2 == sym_nil_type) ? t1 : nullptr);
    if (non_nil) {
      llvm::Type *underlying = getLLVMType(non_nil);
      if (underlying && underlying->isPointerTy()) return underlying;
    }
  }
  DEBUG_LOG("SUM type %s mapped to opaque ptr (placeholder)\n",
            unaliased_sym->name ? unaliased_sym->name : "(anon)");
  return llvm::PointerType::getUnqual(*TheContext);
}

// Dispatch by type_kind. Sub-helpers may set sym->llvm_type early
// (e.g. mapStructType) to handle cycles.
static llvm::Type *mapByTypeKind(Sym *sym, Sym *unaliased_sym) {
  switch (unaliased_sym->type_kind) {
    case Type_RECORD:
      return unaliased_sym->is_vector ? mapVectorType(unaliased_sym) : mapStructType(sym, unaliased_sym);
    case Type_FUN:
      return mapFunctionType(unaliased_sym);
    case Type_REF:
      if (!unaliased_sym->element || !unaliased_sym->element->type) {
        fail("REF type %s has no element type",
             unaliased_sym->name ? unaliased_sym->name : "(anon)");
        return nullptr;
      }
      return llvm::PointerType::getUnqual(*TheContext);
    case Type_SUM:
      return mapSumType(unaliased_sym);
    case Type_PRIMITIVE:
      // Non-numeric primitives (e.g. symbol) → opaque ptr.
      return llvm::PointerType::getUnqual(*TheContext);
    default:
      fail("Unhandled type_kind %d for Sym %s", unaliased_sym->type_kind,
           unaliased_sym->name ? unaliased_sym->name : "(anon)");
      return nullptr;
  }
}

/**
 * Maps IF1 Sym types to LLVM types.
 * Handles numeric types, structs, functions, vectors, and sum types.
 * Caches the result in sym->llvm_type to avoid recomputation.
 *
 * @param sym The IF1 symbol type to map
 * @return The corresponding LLVM type, or nullptr on failure
 */
llvm::Type *getLLVMType(Sym *sym) {
  DEBUG_LOG("getLLVMType %p\n", (void *)sym);
  if (!sym) {
    DEBUG_LOG("getLLVMType(nil), returning VoidTy as fallback\n");
    return llvm::Type::getVoidTy(*TheContext);
  }
  DEBUG_LOG("getLLVMType %s kind %d is_fun %d\n", sym->name ? sym->name : "unnamed", sym->type_kind,
          sym->is_fun);

  // Function symbols shouldn't usually reach here — getLLVMType is for type
  // symbols. The C backend's parallel path resolves function-typed Syms via
  // `s->fun->cg_structural_string` (see codegen_common::assign_type_cg_strings_pass2);
  // the LLVM analog is opaque-pointer-as-function-value (see mapFunctionType).
  //
  // Resolution order:
  //   1. If sym->type is a usable type Sym, recurse through it (gives us a
  //      concrete struct for typed closures).
  //   2. If sym->fun is set (this Sym names a known Fun), return an opaque
  //      pointer. The actual signature lives on Fun::llvm's FunctionType and
  //      is consulted at the call site via CreateCall(target->llvm, args).
  //   3. Otherwise fail with full context — a function Sym with neither a
  //      type recovery path nor a Fun is genuinely unresolvable.
  if (sym->type_kind == 0 && sym->is_fun) {
    if (sym->type && sym->type != sym) {
      DEBUG_LOG("getLLVMType: function symbol '%s' passed; recovering via sym->type (name=%s, kind=%d)\n",
                sym->name ? sym->name : "unnamed",
                sym->type->name ? sym->type->name : "unnamed", sym->type->type_kind);
      return getLLVMType(sym->type);
    }
    if (sym->fun) {
      DEBUG_LOG("getLLVMType: function symbol '%s' with no sym->type, returning opaque ptr (Fun=%p)\n",
                sym->name ? sym->name : "unnamed", (void *)sym->fun);
      llvm::Type *fn_ptr = llvm::PointerType::getUnqual(*TheContext);
      sym->llvm_type = fn_ptr;
      return fn_ptr;
    }
    fail("getLLVMType called with function symbol '%s' and cannot recover (sym->type=%p)",
         sym->name ? sym->name : "unnamed", (void *)sym->type);
    return nullptr;
  }

  if (sym->llvm_type) {
    return sym->llvm_type;
  }
  if (!TheContext) {
    fail("LLVM Context not initialized in getLLVMType for sym: %s", sym->name ? sym->name : "unnamed");
    return nullptr;
  }

  Sym *unaliased_sym = unalias_type(sym);
  if (unaliased_sym->llvm_type) {  // Check again after unaliasing
    sym->llvm_type = unaliased_sym->llvm_type;
    return sym->llvm_type;
  }

  llvm::Type *type = mapBuiltinOrNumeric(unaliased_sym);
  if (!type) {
    type = mapByTypeKind(sym, unaliased_sym);
  }

  if (type) {
    sym->llvm_type = type;
    if (unaliased_sym != sym) {
      unaliased_sym->llvm_type = type;  // Cache on unaliased too
    }
  } else {
    fail("Failed to map Sym %s (kind %d) to LLVM type", sym->name ? sym->name : "unnamed", sym->type_kind);
  }
  return type;
}

// Map a Sym to the LLVM type for a *variable* of that type. Heap-
// allocated aggregates (Type_RECORD, Type_FUN closures, Type_REF)
// live on the GC heap and are addressed through a pointer — the
// variable slot just holds the pointer, never the value. Mirrors the
// C backend's `_CG_psN` typedef: variables are declared with the
// pointer typedef, never the raw struct.
//
// Use for: global slots, local allocas, function args, function
// return types — anywhere "what LLVM type does a variable of this IF1
// type have" is asked. Keep using `getLLVMType` for the underlying
// struct type — CreateStructGEP / sizeof / struct construction
// expect the struct type, not the pointer.
//
// POD-record override (Sym::is_value_type, IR.md §3.4): when the
// frontend explicitly marks a record type as "pass-by-value" — V's
// `is_structure` records, or a future pyc `@struct` decorator —
// fall back to the underlying value type instead of a pointer. The
// current pyc frontend never sets this on a user Type_RECORD (it's
// only lifted onto numeric primitives via the implements chain), so
// the override is a no-op today but documents the seam for the
// future enhancement tracked in
// ifa/issues/015-pyc-pod-records-no-frontend-hook.md.
llvm::Type *getLLVMVarType(Sym *type) {
  llvm::Type *t = getLLVMType(type);
  if (!t) return nullptr;
  if (type) {
    Sym *unaliased = unalias_type(type);
    if (unaliased && !unaliased->is_value_type &&
        (unaliased->type_kind == Type_RECORD ||
         unaliased->type_kind == Type_FUN ||
         unaliased->type_kind == Type_REF)) {
      return llvm::PointerType::getUnqual(*TheContext);
    }
  }
  return t;
}

// Basic implementation of getLLVMDIType for Debug Info
llvm::DIType *getLLVMDIType(Sym *sym, llvm::DIFile *di_file) {
  if (!sym || !DBuilder) return nullptr;
  if (sym->llvm_type_di_cache) return sym->llvm_type_di_cache;

  // TODO: Implement proper type mapping for Debug Info
  // This is a minimal implementation to satisfy the linker and provide basic type info.

  llvm::DIType *di_type = nullptr;
  uint64_t size_in_bits = 0;  // Default
  unsigned encoding = llvm::dwarf::DW_ATE_unsigned;

  if (sym->num_kind == IF1_NUM_KIND_INT) {
    size_in_bits = 32;  // Placeholder
    encoding = llvm::dwarf::DW_ATE_signed;
    di_type = DBuilder->createBasicType(sym->name ? sym->name : "int", size_in_bits, encoding);
  } else if (sym->num_kind == IF1_NUM_KIND_FLOAT) {
    size_in_bits = 64;  // Placeholder
    encoding = llvm::dwarf::DW_ATE_float;
    di_type = DBuilder->createBasicType(sym->name ? sym->name : "float", size_in_bits, encoding);
  } else {
    // Fallback for other types -> treat as void* or similar
    di_type = DBuilder->createUnspecifiedType(sym->name ? sym->name : "unknown_type");
  }

  sym->llvm_type_di_cache = di_type;
  return di_type;
}

// Forward Declaration

// Helper to unescape a string literal (remove quotes and process escape sequences)
static std::string unescapeStringLiteral(const char *s) {
  std::string result;
  size_t len = strlen(s);

  // Skip leading quote if present
  size_t i = 0;
  if (len > 0 && s[0] == '"') {
    i = 1;
    len--;  // Also skip trailing quote
  }

  // Process characters until the trailing quote
  while (i < len && s[i] != '"') {
    if (s[i] == '\\' && i + 1 < len) {
      // Handle escape sequences
      i++;
      switch (s[i]) {
        case 'n':
          result += '\n';
          break;
        case 't':
          result += '\t';
          break;
        case 'r':
          result += '\r';
          break;
        case 'b':
          result += '\b';
          break;
        case 'f':
          result += '\f';
          break;
        case 'v':
          result += '\v';
          break;
        case 'a':
          result += '\a';
          break;
        case '\\':
          result += '\\';
          break;
        case '\"':
          result += '\"';
          break;
        case '\'':
          result += '\'';
          break;
        case '0':
          result += '\0';
          break;
        // Could add octal and hex sequences here if needed
        default:
          // Unknown escape, just include the character
          result += s[i];
          break;
      }
    } else {
      result += s[i];
    }
    i++;
  }

  return result;
}

// Helper to get or create string constants as global variables.
// `string_constants_map` is defined at file scope above and cleared
// by `llvm_codegen_initialize` — every entry references the prior
// run's TheContext / TheModule, so caching across calls would be
// a use-after-free. See AUDIT §4 / CODEGEN_PLAN §3.1.
static llvm::Constant *getOrCreateLLVMStringConstant(const std::string &str) {
  if (string_constants_map.count(str)) {
    return string_constants_map[str];
  }

  llvm::Constant *str_const_array = llvm::ConstantDataArray::getString(*TheContext, str, true);  // Add null terminator
  // Create a global variable for this string
  llvm::GlobalVariable *gv =
      new llvm::GlobalVariable(*TheModule, str_const_array->getType(),
                               true,                               // isConstant
                               llvm::GlobalValue::PrivateLinkage,  // Strings are usually private to the module
                               str_const_array,
                               ".str");                        // Name for the global
  gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);  // Mark as unnamed_addr
  gv->setAlignment(llvm::MaybeAlign(1));                       // Strings can be byte-aligned

  // Get a pointer to the first element of the string for use (char*)
  llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  llvm::Constant *indices[] = {zero, zero};
  llvm::Constant *ptr_to_str = llvm::ConstantExpr::getGetElementPtr(str_const_array->getType(), gv, indices, true);

  string_constants_map[str] = ptr_to_str;
  return ptr_to_str;
}

// --- Global Variable and Constant Generation ---
llvm::Constant *getLLVMConstant(Var *var) {
  if (!var || !var->sym) {
    fail("Null Var or Sym in getLLVMConstant");
    return nullptr;
  }
  Sym *sym = var->sym;

  // Check if llvm_value is already a constant (e.g. from a global variable)
  if (var->llvm_value && llvm::isa<llvm::Constant>(var->llvm_value)) {
    return llvm::cast<llvm::Constant>(var->llvm_value);
  }

  llvm::Type *llvm_type = getLLVMType(var->type);
  if (!llvm_type) {
    fail("Could not determine LLVM type for constant %s", sym->name);
    return nullptr;
  }

  // Match cg.cc:825 order: numeric immediates first (so booleans land in
  // the integer path), then string / symbol constants via sym->constant.
  // The string-kind exclusion matters: a Var with type sym_string carries
  // its bytes in sym->constant, not in sym->imm — without the exclusion
  // we enter the numeric path, find the LLVM type isn't int/float, and
  // fail() instead of falling through to the string handling below.
  if (sym->imm.const_kind != IF1_NUM_KIND_NONE &&
      sym->imm.const_kind != IF1_CONST_KIND_STRING) {  // Numeric immediates
    Immediate imm = sym->imm;
    if (llvm_type->isIntegerTy()) {
      uint64_t val = 0;
      bool is_signed = (imm.const_kind == IF1_NUM_KIND_INT);
      switch (imm.num_index) {
        case IF1_INT_TYPE_1:
          val = imm.v_bool;
          break;
        case IF1_INT_TYPE_8:
          val = is_signed ? (uint64_t)(int64_t)imm.v_int8 : imm.v_uint8;
          break;
        case IF1_INT_TYPE_16:
          val = is_signed ? (uint64_t)(int64_t)imm.v_int16 : imm.v_uint16;
          break;
        case IF1_INT_TYPE_32:
          val = is_signed ? (uint64_t)(int64_t)imm.v_int32 : imm.v_uint32;
          break;
        case IF1_INT_TYPE_64:
          val = imm.v_uint64;
          break;  // v_int64 is same bits as v_uint64
        default:
          fail("Unhandled immediate integer type index %d for %s", imm.num_index, sym->name);
          return nullptr;
      }
      return llvm::ConstantInt::get(llvm_type, val, is_signed);
    } else if (llvm_type->isFloatingPointTy()) {
      double val = 0.0;
      switch (imm.num_index) {
        case IF1_FLOAT_TYPE_32:
          val = imm.v_float32;
          break;
        case IF1_FLOAT_TYPE_64:
          val = imm.v_float64;
          break;
        case IF1_FLOAT_TYPE_128:  // LLVM APFloat for 128-bit
          fail("FP128 immediate not yet handled for %s", sym->name);
          return nullptr;  // TODO
        default:
          fail("Unhandled immediate float type index %d for %s", imm.num_index, sym->name);
          return nullptr;
      }
      return llvm::ConstantFP::get(llvm_type, val);
    } else {
      fail("Immediate constant for non-numeric type for %s", sym->name);
      return nullptr;
    }
  } else if (sym->is_constant && sym->constant) {  // String constants (checked after imm)
    if (var->type == sym_string) {
      // cg.cc uses: _CG_String("escaped_string")
      // For LLVM, we need to unescape the string literal (remove quotes and process escapes)
      std::string unescaped = unescapeStringLiteral(sym->constant);
      return getOrCreateLLVMStringConstant(unescaped);
    }
    // For other types, IF1 might store numeric constants as strings.
    // We need to parse them based on llvm_type.
    if (llvm_type->isIntegerTy()) {
      long long val = strtoll(sym->constant, nullptr, 0);                // Auto-detect base
      return llvm::ConstantInt::get(llvm_type, val, true /*isSigned*/);  // Assume signed for strtoll
    } else if (llvm_type->isFloatingPointTy()) {
      double val = strtod(sym->constant, nullptr);
      return llvm::ConstantFP::get(llvm_type, val);
    } else if (llvm_type->isVoidTy()) {
      // Type is unknown/void but we have a string constant - try to parse it as a number
      char *endptr = nullptr;
      long long val = strtoll(sym->constant, &endptr, 0);
      if (endptr && *endptr == '\0') {
        // Successfully parsed as integer - default to i64
        return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), val, true);
      }
      double fval = strtod(sym->constant, &endptr);
      if (endptr && *endptr == '\0') {
        // Successfully parsed as float - default to double
        return llvm::ConstantFP::get(llvm::Type::getDoubleTy(*TheContext), fval);
      }
      fail("Unhandled string constant with void type: %s for var %s", sym->constant, sym->name);
      return nullptr;
    } else {
      fail("Unhandled string constant for non-string, non-numeric type: %s for var %s", sym->constant, sym->name);
      return nullptr;
    }
  } else if (var->type == sym_nil_type) {  // Handle nil constants
    if (llvm_type->isPointerTy()) {
      return llvm::ConstantPointerNull::get(static_cast<llvm::PointerType *>(llvm_type));
    } else if (llvm_type->isVoidTy()) {
      // Void type has no value — caller is asking for nil-of-void.
      DEBUG_LOG("nil constant requested for void-typed var %s; returning nullptr\n", sym->name);
      return nullptr;
    } else {
      // Nil for non-pointer types: best-effort zero.
      DEBUG_LOG("nil constant for non-pointer type %s for var %s; using zero\n",
                getTypeName(llvm_type).c_str(), sym->name);
      return llvm::Constant::getNullValue(llvm_type);
    }
  }
  // TODO: Handle aggregate constants (structs, arrays) if they are Var properties
  // For now, assume globals are simple types or strings.

  fail("Could not create LLVM constant for Var %s", sym->name);
  return nullptr;
}

static void createGlobalVariables(FA *fa) {
  if (!fa) return;

  Vec<Var *> globals_from_fa;
  // In cg.cc, globals are collected via `collect_types_and_globals`.
  // Let's assume `fa->pdb->if1->allsyms` contains symbols that could be globals,
  // and their `var` field would be the Var representation.
  // Or, if `FA` populates a specific list of global Vars, use that.
  // For now, let's iterate allsyms and check if their `var` is a global.
  // Iterate allsyms to find globals (parallels cg.cc's collect_types_and_globals)
  for (Sym *sym_iter : fa->pdb->if1->allsyms) {
    Var *var = sym_iter->var;
    if (!var) continue;
    if (!var || !var->sym) continue;
    Sym *sym = var->sym;

    // Skip unnamed temporaries - they should be computed locally, not created as globals
    if (sym->is_local || var->is_formal || !var->type || var->type->type_kind == Type_FUN || sym->is_fun ||
        !sym->name) {
      continue;
    }
    if (var->llvm_value) continue;

    // pyc declares heap-aggregate globals as pointers (matching the
    // C backend's `_CG_psN g0;`). getLLVMVarType encodes that.
    llvm::Type *gvar_llvm_type = getLLVMVarType(var->type);
    if (!gvar_llvm_type || gvar_llvm_type->isVoidTy()) {
      DEBUG_LOG("Skipping global var %s due to void or unmappable type\n", sym->name);
      continue;
    }

    llvm::Constant *initializer = nullptr;
    if (sym->is_constant || sym->imm.const_kind != IF1_NUM_KIND_NONE || var->type == sym_nil_type) {
      initializer = getLLVMConstant(var);
    } else {
      // Globals without explicit initializers are zero-initialized by default in LLVM
      initializer = llvm::Constant::getNullValue(gvar_llvm_type);
    }

    if (!initializer && !(var->type == sym_string && sym->is_constant && sym->constant)) {
      // Not a string handled by getOrCreateLLVMStringConstant — zero-init.
      DEBUG_LOG("Global variable %s has no initializer; zero-initializing\n", sym->name);
      initializer = llvm::Constant::getNullValue(gvar_llvm_type);
    }

    // Linkage: cg.cc implies most globals are external unless static.
    // IF1 doesn't have a direct static keyword for module-level vars in the same way.
    // Assume ExternalLinkage for now, unless sym->is_external is false (meaning internal to module).
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::ExternalLinkage;
    if (sym->is_builtin && !sym->is_external) {  // Builtin but defined in this module
      linkage = llvm::GlobalValue::InternalLinkage;
    } else if (!sym->is_external && !sym->is_builtin) {  // Not explicitly external, make internal
                                                         // This needs more info from IF1 structure to be correct.
                                                         // linkage = llvm::GlobalValue::InternalLinkage;
    }

    // Special case for string constants created by getLLVMConstant via getOrCreateLLVMStringConstant:
    // Their var->llvm_value might already be set to a GEP of a global.
    // We should not create another GlobalVariable for the Var itself in that case.
    if (var->type == sym_string && sym->is_constant && sym->constant) {
      if (!var->llvm_value) {                    // If getLLVMConstant didn't set it (e.g. if it wasn't called before)
        var->llvm_value = getLLVMConstant(var);  // This will create the string GV and return a GEP
      }
      // The llvm_value is already the pointer to the string, not the GV itself.
      // Debug info for the Var (as a pointer) can still be created.
    } else if (initializer) {
      llvm::GlobalVariable *gvar =
          new llvm::GlobalVariable(*TheModule, gvar_llvm_type,
                                   sym->is_read_only,  // bool isConstant (i.e., read-only after init)
                                   linkage, initializer, sym->name ? sym->name : ("global_" + std::to_string(sym->id)));
      // Set alignment if specified in Sym
      if (sym->alignment > 0) {
        gvar->setAlignment(llvm::Align(sym->alignment));
      }
      var->llvm_value = gvar;  // Store the GlobalVariable itself
    } else {
      DEBUG_LOG("Could not create global variable for %s; missing initializer and not string\n", sym->name);
      continue;
    }

    // Add Debug Info for Global Variable
    if (DBuilder && CU && var->llvm_value) {
      llvm::DIFile *di_file = UnitFile;  // Use CU's file
      unsigned line_num = sym->line();
      llvm::DIType *di_type = getLLVMDIType(var->type, di_file);

      if (di_type) {
        // DIBuilder expects the actual GlobalVariable for createGlobalVariableExpression,
        // not a GEP constant in case of strings.
        llvm::GlobalVariable *gv_for_debug = llvm::dyn_cast<llvm::GlobalVariable>(var->llvm_value);
        if (!gv_for_debug && var->type == sym_string && sym->is_constant && sym->constant) {
          // If it's a string constant, var->llvm_value is a GEP. We need to find the underlying GV.
          // This is a bit hacky. getOrCreateLLVMStringConstant needs to return the GV or make it findable.
          // For now, we might not be able to create perfect DI for these string Vars.
        }

        if (gv_for_debug) {
          DBuilder->createGlobalVariableExpression(
              CU,                                             // Scope (Compile Unit)
              gv_for_debug->getName(),                        // Name
              gv_for_debug->getName(),                        // Linkage Name
              di_file,                                        // File
              line_num,                                       // Line number
              di_type,                                        // DI Type
              linkage == llvm::GlobalValue::InternalLinkage,  // isLocalToUnit (true if internal linkage)
              true,                                           // isDefined
              nullptr                                         // Expression (optional, for complex debug info)
          );
        } else if (var->type == sym_string && llvm::isa<llvm::Constant>(var->llvm_value)) {
          // For string vars that are GEPs to global constants.
          // Create a simpler global variable debug info entry without direct GV linkage
          // This is a fallback.
          DBuilder->createGlobalVariableExpression(CU, sym->name ? sym->name : "string_ptr_global",
                                                   sym->name ? sym->name : "string_ptr_global", di_file, line_num,
                                                   di_type, true, true);
        }
      }
    }
  }
}

// num_string(Sym*) — moved to codegen_common.{h,cc}.

void llvm_build_type_strings(FA *fa) {
#define S(_n) if1_get_builtin(fa->pdb->if1, #_n)->cg_string = "_CG_" #_n;
#include "builtin_symbols.h"
#undef S
  // Annotate=false: LLVM IR identifiers can't carry comments. No
  // globals collection: the LLVM backend discovers globals separately
  // via createGlobalVariables. See codegen_common.{h,cc}.
  assign_fun_cg_strings(fa, /*annotate=*/false, /*globals=*/nullptr);
  Vec<Var *> globals;
  Vec<Sym *> allsyms;
  collect_types_and_globals(fa, allsyms, globals);
  // fp=nullptr: the LLVM backend doesn't emit struct-forward-decls;
  // struct types are built via getLLVMType when needed.
  assign_type_cg_strings_pass1(allsyms, /*fp=*/nullptr);
  assign_type_cg_strings_pass2(allsyms);
}

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun, cchar *input_filename) {
  DEBUG_LOG("llvm_codegen_print_ir started\n");
  llvm_codegen_initialize(fa);
  llvm_build_type_strings(fa);
  if (!fa) {
    fail("FA object is null in llvm_codegen_print_ir");
    return;
  }
  if (!main_fun) {
    fail("Main function is null in llvm_codegen_print_ir");
    return;
  }

  // Create DIBuilder Compile Unit
  // Use the input_filename passed in, which has the actual source file path
  cchar *src_filename =
      input_filename ? input_filename : (fa->pdb->if1->filename ? fa->pdb->if1->filename : "unknown.ifa");
  // Basic path handling, might need improvement for complex paths
  std::string full_path = src_filename;
  std::string dir = ".";
  std::string fname = full_path;
  size_t last_slash = full_path.find_last_of("/\\");
  if (last_slash != std::string::npos) {
    dir = full_path.substr(0, last_slash);
    fname = full_path.substr(last_slash + 1);
  }
  TheModule->setSourceFileName(fname);  // Set source file name on the Module
  if (DBuilder) {
    UnitFile = DBuilder->createFile(fname, dir);
    CU = DBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C, UnitFile, "ifa-compiler", 0 /*isOptimized*/, "" /*flags*/,
                                     0 /*RV*/);

    // Add debug info version to module
    TheModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
  }

  // Create Global Variables
  createGlobalVariables(fa);

  // Iterate over all functions in FA and create them in the LLVM Module
  // Ensure main_fun is processed, and other functions it might call.
  // The order might matter if there are dependencies not captured by FA's list directly.
  // For now, iterate fa->funs then ensure main_fun is included if not already.

  Vec<Fun *> all_funs;

  DEBUG_LOG("fa->funs.n = %d before discovery\n", fa->funs.n);

  // Discover all reachable functions before translation
  // This ensures they all have proper liveness analysis
  discover_all_reachable_functions(fa, main_fun, all_funs);

  DEBUG_LOG("all_funs.n = %d after discovery\n", all_funs.n);

  // Build reverse call graph for constant recovery
  build_reverse_call_graph(fa);

  all_funs_global = &all_funs;

  // First pass: Create all function declarations (signatures only)
  // Only process live functions (like C backend does)
  for (Fun *f : all_funs) {
    if (!f) {
      DEBUG_LOG("Found NULL fun in all_funs\n");
      continue;
    }
    if (!f->live) {
      DEBUG_LOG("Skipping non-live function %s (id %d)\n", f->sym->name ? f->sym->name : "(null)",
              f->sym->id);
      continue;
    }
    DEBUG_LOG("createFunction for %d\n", f->sym->id);
    createFunction(f, TheModule.get());
  }

  // Second pass: Translate all function bodies
  // (This is now done separately to ensure all functions are declared before any body is translated)
  for (Fun *f : all_funs) {
    if (!f || !f->live || !f->llvm || f->is_external || !f->entry) {
      continue;
    }
    DEBUG_LOG("translateFunctionBody for %s (id %d)\n", f->sym->name, f->sym->id);
    translateFunctionBody(f);
  }

  // TODO: Implement PNode translation for each function's body
  // For now, functions will be declared but not defined (if not external)

  // Create a simple main function that calls the IF1 main_fun
  // This assumes the IF1 main_fun doesn't take argc, argv
  llvm::FunctionType *main_func_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(*TheContext), false);
  llvm::Function *llvm_main =
      llvm::Function::Create(main_func_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
  llvm::BasicBlock *main_entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_main);
  Builder->SetInsertPoint(main_entry_bb);

  if (main_fun->llvm) {  // If IF1 main function was generated
    Builder->CreateCall(main_fun->llvm);
  } else {
    // This case should ideally not happen if main_fun is valid and processed
    DEBUG_LOG("IF1 main function '%s' not found or generated in LLVM module\n", main_fun->sym->name);
  }
  Builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));

  // (DBuilder->finalize() is called once below, after module verification.)

  // Debug: Check for unterminated blocks before verification
  DEBUG_LOG("Pre-verification check for unterminated blocks:\n");
  for (llvm::Function &F : *TheModule) {
    for (llvm::BasicBlock &BB : F) {
      if (!BB.getTerminator()) {
        DEBUG_LOG("WARNING: Function %s has unterminated block %s (size=%zu)\n", F.getName().str().c_str(),
                BB.getName().str().c_str(), BB.size());
        DEBUG_LOG("Block instructions:\n");
        int idx = 0;
        for (llvm::Instruction &I : BB) {
          if (ifa_debug) {
            fprintf(stderr, "DEBUG:   [%d] ", idx++);
            I.print(llvm::errs());
            fprintf(stderr, "\n");
          }
        }
      }
    }
  }

  // Verify the module
  std::string error_str;
  llvm::raw_string_ostream rso(error_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    fail("LLVM module verification failed: %s", rso.str().c_str());
    if (fp != stderr) {  // Print to file as well if it's not stderr
      std::string err_msg = rso.str();
      std::stringstream ss(err_msg);
      std::string segment;
      while (std::getline(ss, segment, '\n')) {
        fprintf(fp, "; LLVM module verification failed: %s\n", segment.c_str());
      }
    }
    // Continue despite verification failure (non-fatal fail allows this)
  }

  // Finalize debug info
  if (DBuilder) {
    DBuilder->finalize();
  }

  // Print the module to the file
  std::string ir_string;
  llvm::raw_string_ostream ir_rso(ir_string);
  TheModule->print(ir_rso, nullptr);
  fprintf(fp, "%s", ir_rso.str().c_str());
}
llvm::Value *getLLVMValue(Var *var, Fun *ifa_fun) {
  if (!var) {
    fail("Null Var provided to getLLVMValue");
    return nullptr;
  }
  DEBUG_LOG("getLLVMValue var=%p, sym=%s, type=%p\n", var, var->sym ? var->sym->name : "null", var->type);
  if (var->llvm_value) {
    llvm::Value *val = var->llvm_value;
    llvm::Function *this_func = ifa_fun->llvm;

    DEBUG_LOG("getLLVMValue found cached llvm_value for var %s (id %d)\n",
            var->sym ? var->sym->name : "(null)", var->id);
    // Check what kind of value this is
    if (llvm::isa<llvm::GlobalVariable>(val)) {
      DEBUG_LOG("  It's a GlobalVariable (pointer to value)\n");
    } else if (llvm::isa<llvm::AllocaInst>(val)) {
      DEBUG_LOG("  It's an AllocaInst (pointer to value)\n");
    } else if (llvm::isa<llvm::Instruction>(val)) {
      DEBUG_LOG("  It's an Instruction (direct value)\n");
    } else if (llvm::isa<llvm::Argument>(val)) {
      DEBUG_LOG("  It's an Argument (direct value)\n");
    } else if (llvm::isa<llvm::Constant>(val)) {
      DEBUG_LOG("  It's a Constant\n");
    }

    bool scope_mismatch = false;
    if (llvm::isa<llvm::Instruction>(val)) {
      llvm::Function *val_func = llvm::cast<llvm::Instruction>(val)->getFunction();
      if (val_func != this_func) {
        DEBUG_LOG("Scope mismatch for var %s (id %d). Val func: %s, Current func: %s. Clearing cache.\n",
                var->sym->name, var->id, val_func ? val_func->getName().str().c_str() : "null",
                this_func ? this_func->getName().str().c_str() : "null");
        scope_mismatch = true;
      }
    } else if (llvm::isa<llvm::Argument>(val)) {
      llvm::Function *val_func = llvm::cast<llvm::Argument>(val)->getParent();
      if (val_func != this_func) {
        DEBUG_LOG("Argument scope mismatch for var %s (id %d): val func %s vs current %s; clearing cache\n",
                  var->sym->name, var->id, val_func ? val_func->getName().str().c_str() : "null",
                  this_func ? this_func->getName().str().c_str() : "null");
        scope_mismatch = true;
      }
    }

    if (scope_mismatch) {
      // Stale value, clear it and fall through to reload
      var->llvm_value = nullptr;
    } else {
      // If it's an AllocaInst or GlobalVariable (pointers), we need to load them.
      // If it's an SSA value (Argument or Instruction result), we use it directly.
      if (llvm::isa<llvm::AllocaInst>(var->llvm_value)) {
        llvm::AllocaInst *ai = llvm::cast<llvm::AllocaInst>(var->llvm_value);
        llvm::Type *load_type = ai->getAllocatedType();
        return Builder->CreateLoad(load_type, var->llvm_value,
                                   var->sym->name ? (std::string(var->sym->name) + ".load") : "");
      }
      if (llvm::isa<llvm::GlobalVariable>(var->llvm_value)) {
        llvm::GlobalVariable *gv = llvm::cast<llvm::GlobalVariable>(var->llvm_value);
        llvm::Type *load_type = gv->getValueType();
        return Builder->CreateLoad(
            load_type, var->llvm_value,
            var->sym && var->sym->name ? (std::string(var->sym->name) + ".load") : "global.load");
      }
      return var->llvm_value;
    }
  }

  // Handle global variables or constants if not mapped through allocas
  // Handle function symbols - return the function pointer
  if (var->sym && var->sym->is_fun) {
    DEBUG_LOG("getLLVMValue for function symbol %s (id=%d)\n", var->sym->name ? var->sym->name : "unnamed",
            var->sym->id);
    // Find the function by symbol
    llvm::Function *func = nullptr;
    if (all_funs_global) {
      for (Fun *fx : *all_funs_global) {
        if (fx && fx->sym == var->sym && fx->llvm) {
          func = fx->llvm;
          break;
        }
      }
    }
    if (!func && var->sym->name) {
      // Try by name
      func = TheModule->getFunction(var->sym->name);
    }
    if (func) {
      return func;
    }
    DEBUG_LOG("Function %s not found in module; returning nullptr\n",
              var->sym->name ? var->sym->name : "unnamed");
    return nullptr;
  }

  // Handle symbols (like operator symbols: '<', '+', etc.)
  if (var->sym && var->sym->is_symbol) {
    DEBUG_LOG("getLLVMValue for symbol %s (id=%d)\n", var->sym->name ? var->sym->name : "unnamed",
            var->sym->id);
    // Symbols are represented as integers in the C backend (id)
    // Return the symbol ID as a constant integer
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), var->sym->id);
  }

  // Handle constants (literals)
  if (var->sym &&
      (var->sym->is_constant || var->sym->imm.const_kind != IF1_NUM_KIND_NONE || var->type == sym_nil_type)) {
    llvm::Constant *const_val = getLLVMConstant(var);
    return const_val;
  }

  // Handle tuple field formals: variables marked as both is_local and is_formal
  // These need to be extracted from the tuple argument
  if (var->sym && var->sym->is_local && var->is_formal) {
    DEBUG_LOG("Variable %s (id=%d) is both local and formal - needs tuple extraction\n",
            var->sym->name ? var->sym->name : "(null)", var->sym->id);

    // The tuple argument should be in the function's arguments
    // Find it by looking for an argument with a struct type
    llvm::Function *llvm_func = ifa_fun->llvm;
    llvm::Argument *tuple_arg = nullptr;

    DEBUG_LOG("Function %s has %ld arguments\n", ifa_fun->sym->name ? ifa_fun->sym->name : "(null)",
            llvm_func->arg_size());

    for (llvm::Argument &arg : llvm_func->args()) {
      llvm::Type *arg_type = arg.getType();
      DEBUG_LOG("  Arg type: isPointer=%d, isStruct=%d\n", arg_type->isPointerTy() ? 1 : 0,
              arg_type->isStructTy() ? 1 : 0);

      // LLVM 18 uses opaque pointers - take first pointer/struct as tuple
      if (arg_type->isPointerTy() || arg_type->isStructTy()) {
        tuple_arg = &arg;
        DEBUG_LOG("Found tuple_arg (LLVM Argument)\n");
        break;
      }
    }

    if (tuple_arg) {
      DEBUG_LOG("Looking for tuple formal variable to get struct type\n");

      Var *tuple_formal_var = nullptr;
      for (Var *fv : ifa_fun->fa_all_Vars) {
        if (fv && fv->is_formal && fv->llvm_value == tuple_arg) {
          tuple_formal_var = fv;
          DEBUG_LOG("Found tuple formal var by llvm_value match: sym=%s (id=%d)\n",
                  fv->sym ? fv->sym->name : "(null)", fv->sym ? fv->sym->id : -1);
          break;
        }
      }

      if (!tuple_formal_var) {
        for (Var *fv : ifa_fun->fa_all_Vars) {
          if (fv && fv->is_formal && !fv->sym->is_local && fv->type && fv->type->type_kind == Type_RECORD) {
            tuple_formal_var = fv;
            DEBUG_LOG("Found tuple formal var by type: sym=%s (id=%d)\n",
                    fv->sym ? fv->sym->name : "(null)", fv->sym ? fv->sym->id : -1);
            break;
          }
        }
      }

      if (!tuple_formal_var || !tuple_formal_var->type) {
        DEBUG_LOG("Could not find tuple formal variable for field extraction\n");
      } else {
        llvm::Type *tuple_struct_type = getLLVMType(tuple_formal_var->type);
        if (!tuple_struct_type || !tuple_struct_type->isStructTy()) {
          DEBUG_LOG("Tuple formal variable has invalid struct type\n");
        } else {
          // Use MPosition to determine field index (not sequential counting!)
          // Tuple operators like (a, +, b) have field indices 0, 1, 2, but + is not a formal.
          // Parallels cg.cc:542-550 (write_arg_position) and cg.cc:715-725 (write_c_args)
          int formal_idx = -1;
          MPosition *found_pos = nullptr;

          for (MPosition *p : ifa_fun->positional_arg_positions) {
            Var *formal_var = ifa_fun->args.get(p);
            if (formal_var == var) {
              found_pos = p;
              // Field index from pos[1] for nested positions (see cg.cc:548)
              if (p->pos.n > 1 && is_intPosition(p->pos[1])) {
                formal_idx = (int)Position2int(p->pos[1]) - 1;
              }
              break;
            }
          }

          if (formal_idx < 0 || !found_pos) {
            DEBUG_LOG("Could not find MPosition or field index for var %s (formal_idx=%d, found_pos=%p)\n",
                      var->sym->name ? var->sym->name : "(null)", formal_idx, (void *)found_pos);
          } else {
            DEBUG_LOG("Extracting field %d from tuple argument for var %s (MPos.n=%d)\n", formal_idx,
                    var->sym->name ? var->sym->name : "(null)", found_pos->pos.n);

            llvm::Value *field_ptr =
                Builder->CreateStructGEP(tuple_struct_type, tuple_arg, formal_idx,
                                         var->sym->name ? (std::string(var->sym->name) + ".ptr") : "field.ptr");

            llvm::Type *field_type = getLLVMType(var->type);
            llvm::Value *field_val = Builder->CreateLoad(
                field_type, field_ptr, var->sym->name ? (std::string(var->sym->name) + ".val") : "field.val");

            var->llvm_value = field_val;
            return field_val;
          }
        }
      }
    } else {
      DEBUG_LOG("No tuple argument found for var %s (function has 0 args, likely unspecialized template)\n",
                var->sym->name ? var->sym->name : "(null)");
      // Return undef value for unspecialized templates with no arguments
      llvm::Type *var_type = getLLVMType(var->type);
      if (var_type) {
        return llvm::UndefValue::get(var_type);
      }
      return nullptr;
    }
  }

  // Assume global if not local
  if (!var->sym->is_local && !var->is_formal) {
    if (var->sym->name) {
      llvm::Module *M = ifa_fun->llvm->getParent();
      llvm::GlobalVariable *gv = M->getGlobalVariable(var->sym->name);
      if (gv) return gv;
      // If not found, fail or return null
    } else {
      // Null name global?
      DEBUG_LOG("Global Sym has no name. id=%d\n", var->sym->id);
      return nullptr;  // Will trigger fail in caller
    }
  }

  // Attempt recovery via call graph
  if (llvm::Value *recovered = recover_constant_arg(var, ifa_fun)) {
    setLLVMValue(var, recovered, ifa_fun);
    return recovered;
  }

  fail("Var %s not found (local or global). id=%d", var->sym->name ? var->sym->name : "(null)", var->sym->id);
  return nullptr;
}
void setLLVMValue(Var *var, llvm::Value *val, Fun *ifa_fun) {
  if (!var) {
    fail("Null Var provided to setLLVMValue");
    return;
  }
  if (var->llvm_value && llvm::isa<llvm::AllocaInst>(var->llvm_value)) {
    // It's a local variable allocated with AllocaInst, so we store the new value.
    Builder->CreateStore(val, var->llvm_value);
  } else if (var->llvm_value && llvm::isa<llvm::GlobalVariable>(var->llvm_value)) {
    // Mirror the AllocaInst path for globals — the global slot is
    // backed by a fixed pointer just like an alloca, and the C
    // backend's parallel is plain `g0 = val;`. Without this store,
    // assignments to module-level vars (`a = (1, 2, 3)`) only
    // updated the in-memory cache and never reached @a, so the
    // subsequent `a[i]` reads picked up the zero-init'd global.
    Builder->CreateStore(val, var->llvm_value);
  } else {
    // It's an SSA variable, its llvm_value should be the instruction that defines it.
    // Or it's an argument.
    var->llvm_value = val;
    if (val->getType() != var->llvm_type) {  // Update cached type if different
      if (var->llvm_type) {
        DEBUG_LOG("LLVM value type mismatch for var %s: expected %s, got %s; updating cache\n",
                  var->sym && var->sym->name ? var->sym->name : "??", getTypeName(var->llvm_type).c_str(),
                  getTypeName(val->getType()).c_str());
      }
      var->llvm_type = val->getType();
    }
  }
}

void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *input_filename) {
  char fn[512];
  strncpy(fn, input_filename, sizeof(fn) - 1);
  fn[sizeof(fn) - 1] = '\0';
  char *dot = strrchr(fn, '.');
  if (dot) {
    strcpy(dot, ".ll");  // Replace extension with .ll
  } else {
    strcat(fn, ".ll");  // Append .ll if no extension
  }

  FILE *fp = fopen(fn, "w");
  if (!fp) {
    fail("Unable to open file %s for writing LLVM IR", fn);
    return;
  }
  llvm_codegen_print_ir(fp, fa, main, input_filename);
  fclose(fp);
  DEBUG_LOG("LLVM IR written to %s\n", fn);
}

int llvm_codegen_compile(cchar *input_filename) {
  // Derive .ll / .o / executable paths from the input filename.
  // Bounded `snprintf` with fail-on-truncation so we never silently
  // corrupt paths longer than FILENAME_MAX.
  char ll_file[FILENAME_MAX], obj_file[FILENAME_MAX], exe_file[FILENAME_MAX];
  if (snprintf(ll_file, sizeof(ll_file), "%s", input_filename) >= (int)sizeof(ll_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);
  if (snprintf(obj_file, sizeof(obj_file), "%s", input_filename) >= (int)sizeof(obj_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);
  if (snprintf(exe_file, sizeof(exe_file), "%s", input_filename) >= (int)sizeof(exe_file))
    fail("llvm_codegen_compile: input filename too long: %s", input_filename);

  char *dot_ll = strrchr(ll_file, '.');
  if (dot_ll) strcpy(dot_ll, ".ll");
  else strcat(ll_file, ".ll");
  char *dot_o = strrchr(obj_file, '.');
  if (dot_o) strcpy(dot_o, ".o");
  else strcat(obj_file, ".o");
  char *dot_exe = strrchr(exe_file, '.');
  if (dot_exe) *dot_exe = '\0';

  // Step 1: clang -c -fPIC <ll> -o <obj>
  {
    char *argv[] = {(char *)"clang", (char *)"-c", (char *)"-fPIC", ll_file, (char *)"-o", obj_file, nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) {
      fail("llvm_codegen_compile: clang -c failed for %s (exit=%d)", ll_file, res);
      return res;
    }
  }
  DEBUG_LOG("LLVM IR from %s compiled to %s\n", ll_file, obj_file);

  // Step 2: clang <obj> -o <exe> -lm -lgc -lgccpp
  //
  // -lgc / -lgccpp: the IR emitted by P_prim_make / P_prim_new /
  // P_prim_clone references GC_malloc (Boehm GC). Without these the
  // link fails on undefined `GC_malloc`. See issue 012.
  {
    char *argv[] = {(char *)"clang",   obj_file,
                    (char *)"-o",      exe_file,
                    (char *)"-lm",     (char *)"-lgc",
                    (char *)"-lgccpp", nullptr};
    int res = codegen_spawn("clang", argv);
    if (res != 0) {
      fail("llvm_codegen_compile: linking failed for %s (exit=%d)", obj_file, res);
      return res;
    }
  }
  DEBUG_LOG("Executable %s created successfully\n", exe_file);
  return 0;
}
