/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#include "llvm_internal.h"
#include "builtin.h"
#include "cg.h"
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

// Debug logging
#ifndef LLVM_DEBUG
#define LLVM_DEBUG 0
#endif

#if LLVM_DEBUG
#define DEBUG_LOG(...) fprintf(stderr, "DEBUG: " __VA_ARGS__)
#else
#define DEBUG_LOG(...) do {} while(0)
#endif

// ============================================================================
// Global LLVM State (definitions)
// ============================================================================

std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::DIBuilder> DBuilder;
llvm::DICompileUnit *CU = nullptr;
llvm::DIFile *UnitFile = nullptr;
Vec<Fun*> *all_funs_global = NULL;

static void llvm_codegen_initialize(FA *fa) {
  // Initialize LLVM components
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  //llvm::InitializeAllAsmParsers(); // Not strictly needed for IR generation to .ll
  llvm::InitializeAllAsmPrinters(); // Needed for object file emission if done via PassManager

  TheContext = std::make_unique<llvm::LLVMContext>();
  // Use a more descriptive module ID, perhaps from FA or filename
  cchar* mod_id = fa && fa->pdb && fa->pdb->if1 && fa->pdb->if1->filename ? fa->pdb->if1->filename : "ifa_output";
  TheModule = std::make_unique<llvm::Module>(mod_id, *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  DBuilder = std::make_unique<llvm::DIBuilder>(*TheModule);

  // Set target triple for the module
  std::string TargetTriple = llvm::sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  // Enable PIC for position-independent code generation
  TheModule->setPICLevel(llvm::PICLevel::BigPIC);
  TheModule->setPIELevel(llvm::PIELevel::Default);
  // Optionally, set data layout
  // std::string Error;
  // auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
  // if (Target) {
  //   auto TM = Target->createTargetMachine(TargetTriple, "generic", "", llvm::TargetOptions(), llvm::Optional<llvm::Reloc::Model>());
  //   TheModule->setDataLayout(TM->createDataLayout());
  // } else {
  //   fail("Could not lookup target: %s", Error.c_str());
  // }
}

// Define fail to my_fail to avoid linker conflict with libplib's fail
// and allows us to make it non-fatal locally in this file.
// ONLY for backend/nogc
#ifndef USE_GC
extern "C" void my_fail(const char *fmt, ...);

#undef fail
#define fail my_fail

extern "C" void my_fail(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "FAIL (ignored): ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}
#endif

// Forward declaration

// Helper function from cg.cc:537-540
int is_closure_var(Var *v) {
  Sym *t = v->type;
  return (t && t->type_kind == Type_FUN && !t->fun && t->has.n);
}

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
        case IF1_INT_TYPE_1:  return llvm::Type::getInt1Ty(*TheContext); // bool
        case IF1_INT_TYPE_8:  return llvm::Type::getInt8Ty(*TheContext);
        case IF1_INT_TYPE_16: return llvm::Type::getInt16Ty(*TheContext);
        case IF1_INT_TYPE_32: return llvm::Type::getInt32Ty(*TheContext);
        case IF1_INT_TYPE_64: return llvm::Type::getInt64Ty(*TheContext);
        default:
          fail("Unknown integer type index: %d", sym->num_index);
          return nullptr;
      }
    case IF1_NUM_KIND_FLOAT:
      switch (sym->num_index) {
        case IF1_FLOAT_TYPE_32: return llvm::Type::getFloatTy(*TheContext);
        case IF1_FLOAT_TYPE_64: return llvm::Type::getDoubleTy(*TheContext);
        case IF1_FLOAT_TYPE_128: return llvm::Type::getFP128Ty(*TheContext);
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

// Reverse Call Graph for Constant Recovery
static std::map<Fun*, std::vector<PNode*>> reverse_call_graph;

// Forward declaration for get_target_fun

// Discover all reachable functions by walking the call graph from main
// This ensures all functions are known before liveness analysis
static void discover_all_reachable_functions(FA *fa, Fun *main_fun, Vec<Fun*> &all_funs) {
    if (!fa || !main_fun) return;

    fprintf(stderr, "DEBUG: discover_all_reachable_functions starting from %s\n",
            main_fun->sym->name ? main_fun->sym->name : "(null)");

    std::set<Fun*> visited;
    std::vector<Fun*> worklist;

    // Start with main
    if (main_fun->live && main_fun->entry) {
        worklist.push_back(main_fun);
        visited.insert(main_fun);
    }

    // Add all functions already in fa->funs (only if live)
    int fun_idx = 0;
    forv_Fun(f, fa->funs) {
        if (f) {
            fprintf(stderr, "DEBUG:   fa->funs[%d]: %s (id %d) live=%d\n",
                    fun_idx, f->sym->name ? f->sym->name : "(null)",
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

        fprintf(stderr, "DEBUG: Discovered function %s (id %d), calls.n=%d\n",
                current->sym->name ? current->sym->name : "(null)",
                current->sym->id, current->calls.n);

        // Walk through all call sites in this function
        for (int k = 0; k < current->calls.n; k++) {
            if (current->calls.v[k].key) {
                PNode *call_pnode = (PNode*)current->calls.v[k].key;
                Vec<Fun*> *targets = current->calls.v[k].value;

                if (targets) {
                    forv_Fun(target_fun, *targets) {
                        if (target_fun && visited.find(target_fun) == visited.end()) {
                            fprintf(stderr, "DEBUG:   Found call to %s (id %d)\n",
                                    target_fun->sym->name ? target_fun->sym->name : "(null)",
                                    target_fun->sym->id);
                            worklist.push_back(target_fun);
                            visited.insert(target_fun);
                        }
                    }
                }
            }
        }
    }

    fprintf(stderr, "DEBUG: discover_all_reachable_functions found %d functions\n", all_funs.n);
}

static void build_reverse_call_graph(FA *fa) {
    reverse_call_graph.clear();
    forv_Fun(f, fa->funs) {
        if (!f->live) continue;
        for (int k = 0; k < f->calls.n; k++) {
            if (f->calls.v[k].key) {
                PNode *pn = (PNode*)f->calls.v[k].key;
                Vec<Fun*> *targets = f->calls.v[k].value;
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
    if (arg_idx == -1) return nullptr; // Not an argument

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
        if (call_rval_idx >= caller->rvals.n) return nullptr; // Call has too few args
        
        Var *passed_var = caller->rvals[call_rval_idx];
        if (!passed_var || !passed_var->sym) return nullptr;
        
        // Resolve to constant Sym
        Sym *s = passed_var->sym;
        bool is_const = s->is_constant || s->num_kind != IF1_NUM_KIND_NONE;
        if (!is_const) {
             // Not constant?
             fprintf(stderr, "DEBUG: Recovery: caller passes non-constant %s\n", s->name);
             return nullptr; 
        }
        
        if (!consistent_sym) consistent_sym = s;
        else if (consistent_sym != s) {
            // Mismatch between callers
            return nullptr;
        }
    }
    
    if (consistent_sym) {
        fprintf(stderr, "DEBUG: Recovered constant %s for arg %s\n", consistent_sym->name ? consistent_sym->name : "?", var->sym->name);
        // Create a heap-allocated Var to wrap the Sym (using GC)
        // Stack allocation would create dangling pointers
        Var *tmp = new (UseGC) Var(consistent_sym);
        tmp->type = consistent_sym->type;  // Set the type
        return getLLVMConstant(tmp);
    }
    return nullptr;
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
  fprintf(stderr, "DEBUG: getLLVMType %p\n", (void*)sym);
  if (!sym) {
    // fail("Null Sym provided to getLLVMType");
    fprintf(stderr, "WARNING: getLLVMType(nil), returning VoidTy as fallback\n");
    return llvm::Type::getVoidTy(*TheContext);
  }
  fprintf(stderr, "DEBUG: getLLVMType %s kind %d is_fun %d\n", sym->name ? sym->name : "unnamed", sym->type_kind, sym->is_fun);

  // Check if this is a function symbol (not a type) - this should NOT happen
  // getLLVMType should only be called with type symbols, not function symbols
  if (sym->type_kind == 0 && sym->is_fun) {
    fprintf(stderr, "ERROR: getLLVMType called with function symbol '%s' - this is incorrect!\n", sym->name ? sym->name : "unnamed");
    fprintf(stderr, "       Function symbols should not be passed to getLLVMType.\n");
    fprintf(stderr, "       sym=%p, sym->type=%p, sym->type == sym: %d\n", (void*)sym, (void*)sym->type, sym->type == sym);
    if (sym->type && sym->type != sym) {
      fprintf(stderr, "       Attempting to use sym->type instead...\n");
      fprintf(stderr, "       sym->type name=%s, type_kind=%d, is_fun=%d\n",
              sym->type->name ? sym->type->name : "unnamed",
              sym->type->type_kind,
              sym->type->is_fun);
      // Try to use the type, but only if it's not self-referential
      return getLLVMType(sym->type);
    }
    fail("getLLVMType called with function symbol '%s' and cannot recover", sym->name ? sym->name : "unnamed");
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
  if (unaliased_sym->llvm_type) { // Check again after unaliasing
    sym->llvm_type = unaliased_sym->llvm_type;
    return sym->llvm_type;
  }

  llvm::Type *type = nullptr;

  if (unaliased_sym == sym_void || unaliased_sym == sym_void_type) {
    type = llvm::Type::getVoidTy(*TheContext);
  } else if (unaliased_sym == sym_string) {
    // Represent string as char* or a custom struct {i8*, i64} for length
    // For now, let's use i8* (pointer to char)
    // This might need adjustment based on runtime string representation
    type = llvm::PointerType::getUnqual(*TheContext);
  } else if (unaliased_sym->num_kind != IF1_NUM_KIND_NONE) {
    type = mapNumericType(unaliased_sym);
  } else {
    switch (unaliased_sym->type_kind) {
      case Type_RECORD: {
        if (unaliased_sym->is_vector) {
          // This is a language-level vector/array, not necessarily an LLVM <N x T> fixed-size vector.
          // IF1 vectors are often dynamically sized.
          // cg.cc seems to treat these as structs with a flexible array member or special list types.
          // For LLVM, a common representation for a dynamic array/list is a struct:
          // struct List { ElementType* data; int64_t length; int64_t capacity; };
          // Or simply ElementType* if length is handled elsewhere or it's a pointer to the first element.
          if (unaliased_sym->element && unaliased_sym->element->type) {
            llvm::Type *element_llvm_type = getLLVMType(unaliased_sym->element->type);
            if (element_llvm_type) {
              // Let's represent it as a pointer to its element type for now.
              // This implies the actual data structure (length, capacity) is managed by runtime calls.
              // A more complete representation would be a specific struct type e.g. _CG_list_struct
              // For now, ElementType*
              type = llvm::PointerType::getUnqual(element_llvm_type);
              // Alternative: create a struct like { T* data, size_t length }
              // std::vector<llvm::Type*> list_fields = {
              //   llvm::PointerType::getUnqual(element_llvm_type), // data
              //   llvm::Type::getInt64Ty(*TheContext)             // length
              // };
              // type = llvm::StructType::create(*TheContext, list_fields,
              //                                unaliased_sym->name ? (std::string(unaliased_sym->name) + ".vecdata") : ("vecdata.anon" + std::to_string(unaliased_sym->id)));
            } else {
              fail("Could not get LLVM type for element of vector type %s", unaliased_sym->name ? unaliased_sym->name : "unnamed_vector");
              return nullptr;
            }
          } else {
            fail("Vector type %s has no element type", unaliased_sym->name ? unaliased_sym->name : "unnamed_vector");
            return nullptr;
          }
        } else {
          // Handle regular structs
          llvm::StructType *struct_type = llvm::StructType::create(*TheContext, unaliased_sym->name ? unaliased_sym->name : ("struct.anon" + std::to_string(unaliased_sym->id)));
          sym->llvm_type = struct_type; // Store opaque type to break recursion
          unaliased_sym->llvm_type = struct_type;

          std::vector<llvm::Type *> field_types;
          for (int i = 0; i < unaliased_sym->has.n; ++i) {
            Sym *field_sym = unaliased_sym->has[i];
            // In IF1, fields of a struct are symbols themselves, their type is field_sym->type
            if (field_sym && field_sym->type) {
              llvm::Type *field_llvm_type = getLLVMType(field_sym->type);
              if (field_llvm_type) {
                field_types.push_back(field_llvm_type);
              } else {
                fail("Could not determine LLVM type for field %s (index %d) of struct %s",
                     field_sym->name ? field_sym->name : "unnamed_field", i,
                     unaliased_sym->name ? unaliased_sym->name : "unnamed_struct");
                struct_type->setBody(field_types); // Set with what we have so far
                return nullptr;
              }
            } else {
               fail("Null field_sym or field_sym->type for field index %d of struct %s", i, unaliased_sym->name ? unaliased_sym->name : "unnamed_struct");
               struct_type->setBody(field_types);  // Set with what we have so far
               return nullptr;
            }
          }
          if (struct_type->isOpaque()) { // Only set body if still opaque (no recursive error)
            struct_type->setBody(field_types);
          }
          type = struct_type;
        }
        break;
      }
      case Type_FUN: {
        // Handle function types
        Sym *ret_type_sym = unaliased_sym->ret;
        if (!ret_type_sym || !ret_type_sym->type) {
            fail("Function symbol %s has no return type symbol or type for getLLVMType", unaliased_sym->name ? unaliased_sym->name : "unnamed_fun_type");
            return nullptr;
        }
        llvm::Type *return_llvm_type = getLLVMType(ret_type_sym->type);
        if (!return_llvm_type) {
            fail("Could not get LLVM return type for function symbol %s", unaliased_sym->name ? unaliased_sym->name : "unnamed_fun_type");
            return nullptr;
        }

        std::vector<llvm::Type *> arg_llvm_types;
        bool success = true;
        for (int i = 0; i < unaliased_sym->has.n; ++i) {
          Sym *arg_sym = unaliased_sym->has[i];
           if (arg_sym && arg_sym->type) {
            llvm::Type* ll_arg_type = getLLVMType(arg_sym->type);
            if (ll_arg_type) {
              arg_llvm_types.push_back(ll_arg_type);
            } else {
              fail("Could not get LLVM type for argument %d of function type %s", i, unaliased_sym->name ? unaliased_sym->name : "unnamed_fun_type");
              success = false;
              break;
            }
          } else {
            fail("Null arg_sym or arg_sym->type for argument %d of function type %s", i, unaliased_sym->name ? unaliased_sym->name : "unnamed_fun_type");
            success = false;
            break;
          }
        }
        if (!success) return nullptr;

#if 0
         bool is_var_arg = false;
        // TODO: Handle varargs if unaliased_sym->is_varargs or similar flag exists
        if (unaliased_sym->fun && unaliased_sym->fun->is_varargs) {
            is_var_arg = true;
        }
#endif
        
        // Function type in LLVM is the signature. 
        // Variables of function type are pointers to functions.
        type = llvm::PointerType::getUnqual(*TheContext); // All function values are pointers
        // Wait, if we return type, do we return FunctionType or PointerType?
        // getLLVMType returns the type of the VALUE.
        // A function value is a pointer.
        // But for declaring functions, we need FunctionType.
        // Since getLLVMType is used for variable ALLOCATION, we want the pointer type.
        // Ideally we should cache FunctionType somewhere else if needed.
        // But llvm_type field on Sym is used for both?
        // Let's return PointerType (opaque in LLVM 18) for now.
        // Special case: if we need signature, we re-derive it.
        type = llvm::PointerType::getUnqual(*TheContext);
        break;
      }
      case Type_REF: // Explicit pointer types, if they exist in IF1
        // Assuming unaliased_sym->element holds the pointed-to type
        if (unaliased_sym->element && unaliased_sym->element->type) {
          llvm::Type *element_type = getLLVMType(unaliased_sym->element->type);
          if (element_type) {
            type = llvm::PointerType::getUnqual(element_type);
          } else {
            fail("Could not get LLVM type for pointee of REF type %s", unaliased_sym->name ? unaliased_sym->name : "unnamed_ref");
            return nullptr;
          }
        } else {
          fail("REF type %s has no element type", unaliased_sym->name ? unaliased_sym->name : "unnamed_ref");
          return nullptr;
        }
        break;
      // Array/Vector types might be represented as Type_RECORD with is_vector flag
      // or a dedicated Type_kind. For now, assuming Type_RECORD with is_vector.
      // This logic is partly in Type_RECORD, but if there's a specific Type_ARRAY or Type_VECTOR:
      /*
      case Type_ARRAY: // Or Type_VECTOR
        if (unaliased_sym->element && unaliased_sym->element->type) {
          llvm::Type *element_type = getLLVMType(unaliased_sym->element->type);
          uint64_t num_elements = 0; // IF1 needs a way to specify array size for fixed-size arrays
                                     // Or it's a dynamic array (slice), often {ptr, len} struct.
                                     // For LLVM ArrayType, size must be known.
          if (unaliased_sym->size > 0 && element_type->getPrimitiveSizeInBits() > 0) { // A guess
             num_elements = unaliased_sym->size / (element_type->getPrimitiveSizeInBits()/8);
          }
          if (element_type && num_elements > 0) { // Condition for fixed size array
            type = llvm::ArrayType::get(element_type, num_elements);
          } else if (element_type) {
            // Fallback for dynamic arrays or if size is unknown: treat as pointer to element
            // Or better, a struct {element_type*, size_type}
            // For now, pointer to element. This needs refinement.
            type = llvm::PointerType::getUnqual(element_type);
            fprintf(stderr, "Warning: Array/Vector type %s without fixed size, mapped to pointer.\\n", unaliased_sym->name);
          } else {
             fail("Could not get LLVM type for element of ARRAY/VECTOR type %s", unaliased_sym->name);
             return nullptr;
          }
        } else {
          fail("ARRAY/VECTOR type %s has no element type", unaliased_sym->name);
          return nullptr;
        }
        break;
      */
      case Type_SUM: // Union or tagged union. LLVM doesn't have direct sum types.
                     // This needs careful handling. For now, map to largest element or i8 array.
                     // A common approach is a struct { i8 tag, union_data }
                     // Or, if it's just an optional pointer (e.g. Foo | Nil), map to Foo*.
        if (unaliased_sym->has.n == 2) { // Potential optional pointer like (type | nil)
            Sym* t1 = unaliased_sym->has[0]->type;
            Sym* t2 = unaliased_sym->has[1]->type;
            if (t1 == sym_nil_type && t2) {
                llvm::Type* underlying_type = getLLVMType(t2);
                if (underlying_type && underlying_type->isPointerTy()) type = underlying_type;
                // else if (underlying_type) type = llvm::PointerType::getUnqual(underlying_type); // if it's not already a pointer, make it one?
            } else if (t2 == sym_nil_type && t1) {
                llvm::Type* underlying_type = getLLVMType(t1);
                if (underlying_type && underlying_type->isPointerTy()) type = underlying_type;
                // else if (underlying_type) type = llvm::PointerType::getUnqual(underlying_type);
            }
        }
        if (!type) {
            // Default for SUM: an opaque pointer or a sufficiently large integer/byte array.
            // This is a placeholder and needs a proper strategy for sum types.
            // For now, let's use a pointer to i8 as a very generic type.
            fprintf(stderr, "Warning: SUM type %s mapped to i8*. Needs proper handling.\\n", unaliased_sym->name ? unaliased_sym->name : "unnamed_sum");
            type = llvm::PointerType::getUnqual(*TheContext);
        }
        break;
      case Type_PRIMITIVE: 
          // Non-numeric primitives (e.g. symbol) -> i8*
          type = llvm::PointerType::getUnqual(*TheContext);
          break;
      case Type_UNKNOWN:
      case Type_APPLICATION:
      case Type_VARIABLE:
      case Type_ALIAS: // Should be resolved by unalias_type
      default:
        fail("Unhandled or unknown Sym type_kind: %d for Sym %s",
             unaliased_sym->type_kind, unaliased_sym->name ? unaliased_sym->name : "unnamed");
        return nullptr;
    }
  }

  if (type) {
    sym->llvm_type = type;
    if (unaliased_sym != sym) {
        unaliased_sym->llvm_type = type; // Cache on unaliased too
    }
  } else {
    fail("Failed to map Sym %s (kind %d) to LLVM type",
         sym->name ? sym->name : "unnamed", sym->type_kind);
  }
  return type;
}

// Basic implementation of getLLVMDIType for Debug Info
llvm::DIType *getLLVMDIType(Sym *sym, llvm::DIFile *di_file) {
    if (!sym || !DBuilder) return nullptr;
    if (sym->llvm_type_di_cache) return sym->llvm_type_di_cache;

    // TODO: Implement proper type mapping for Debug Info
    // This is a minimal implementation to satisfy the linker and provide basic type info.
    
    llvm::DIType *di_type = nullptr;
    uint64_t size_in_bits = 0; // Default
    unsigned encoding = llvm::dwarf::DW_ATE_unsigned;

    if (sym->num_kind == IF1_NUM_KIND_INT) {
         size_in_bits = 32; // Placeholder
         encoding = llvm::dwarf::DW_ATE_signed;
         di_type = DBuilder->createBasicType(sym->name ? sym->name : "int", size_in_bits, encoding);
    } else if (sym->num_kind == IF1_NUM_KIND_FLOAT) {
         size_in_bits = 64; // Placeholder
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
        len--; // Also skip trailing quote
    }

    // Process characters until the trailing quote
    while (i < len && s[i] != '"') {
        if (s[i] == '\\' && i + 1 < len) {
            // Handle escape sequences
            i++;
            switch (s[i]) {
                case 'n': result += '\n'; break;
                case 't': result += '\t'; break;
                case 'r': result += '\r'; break;
                case 'b': result += '\b'; break;
                case 'f': result += '\f'; break;
                case 'v': result += '\v'; break;
                case 'a': result += '\a'; break;
                case '\\': result += '\\'; break;
                case '\"': result += '\"'; break;
                case '\'': result += '\''; break;
                case '0': result += '\0'; break;
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

// Helper to get or create string constants as global variables
static llvm::Constant *getOrCreateLLVMStringConstant(const std::string &str) {
    // Check if already exists
    // This simple caching might need to be more robust, e.g., per-module manager
    static std::map<std::string, llvm::Constant *> string_constants_map;

    if (string_constants_map.count(str)) {
        return string_constants_map[str];
    }

    llvm::Constant *str_const_array = llvm::ConstantDataArray::getString(*TheContext, str, true); // Add null terminator
    // Create a global variable for this string
    llvm::GlobalVariable *gv = new llvm::GlobalVariable(
        *TheModule,
        str_const_array->getType(),
        true, // isConstant
        llvm::GlobalValue::PrivateLinkage, // Strings are usually private to the module
        str_const_array,
        ".str"); // Name for the global
    gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global); // Mark as unnamed_addr
    gv->setAlignment(llvm::MaybeAlign(1)); // Strings can be byte-aligned

    // Get a pointer to the first element of the string for use (char*)
    llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
    llvm::Constant *indices[] = {zero, zero};
    llvm::Constant *ptr_to_str = llvm::ConstantExpr::getGetElementPtr(
        str_const_array->getType(), gv, indices, true);

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

    // Match cg.cc:934-938 order: check sym->imm FIRST, then sym->constant
    // This ensures numeric immediates (including booleans) are handled before string parsing
    if (sym->imm.const_kind != IF1_NUM_KIND_NONE) { // Numeric immediates
        Immediate imm = sym->imm;
        if (llvm_type->isIntegerTy()) {
            uint64_t val = 0;
            bool is_signed = (imm.const_kind == IF1_NUM_KIND_INT);
            switch (imm.num_index) {
                case IF1_INT_TYPE_1:
                    val = imm.v_bool;
                    break;
                case IF1_INT_TYPE_8:  val = is_signed ? (uint64_t)(int64_t)imm.v_int8 : imm.v_uint8; break;
                case IF1_INT_TYPE_16: val = is_signed ? (uint64_t)(int64_t)imm.v_int16 : imm.v_uint16; break;
                case IF1_INT_TYPE_32: val = is_signed ? (uint64_t)(int64_t)imm.v_int32 : imm.v_uint32; break;
                case IF1_INT_TYPE_64: val = imm.v_uint64; break; // v_int64 is same bits as v_uint64
                default: fail("Unhandled immediate integer type index %d for %s", imm.num_index, sym->name); return nullptr;
            }
            return llvm::ConstantInt::get(llvm_type, val, is_signed);
        } else if (llvm_type->isFloatingPointTy()) {
            double val = 0.0;
            switch (imm.num_index) {
                case IF1_FLOAT_TYPE_32: val = imm.v_float32; break;
                case IF1_FLOAT_TYPE_64: val = imm.v_float64; break;
                case IF1_FLOAT_TYPE_128: // LLVM APFloat for 128-bit
                    fail("FP128 immediate not yet handled for %s", sym->name); return nullptr; // TODO
                default: fail("Unhandled immediate float type index %d for %s", imm.num_index, sym->name); return nullptr;
            }
            return llvm::ConstantFP::get(llvm_type, val);
        } else {
            fail("Immediate constant for non-numeric type for %s", sym->name);
            return nullptr;
        }
    } else if (sym->is_constant && sym->constant) { // String constants (checked after imm)
        if (var->type == sym_string) {
             // cg.cc uses: _CG_String("escaped_string")
             // For LLVM, we need to unescape the string literal (remove quotes and process escapes)
            std::string unescaped = unescapeStringLiteral(sym->constant);
            return getOrCreateLLVMStringConstant(unescaped);
        }
        // For other types, IF1 might store numeric constants as strings.
        // We need to parse them based on llvm_type.
        if (llvm_type->isIntegerTy()) {
            long long val = strtoll(sym->constant, nullptr, 0); // Auto-detect base
            return llvm::ConstantInt::get(llvm_type, val, true /*isSigned*/);
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
    } else if (var->type == sym_nil_type) { // Handle nil constants
        if (llvm_type->isPointerTy()) {
            return llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(llvm_type));
        } else if (llvm_type->isVoidTy()) {
            // Void type has no value - this should not be used as a constant
            fprintf(stderr, "Warning: Attempting to create nil constant for void type for var %s. Returning nullptr.\n", sym->name);
            return nullptr;
        } else {
            // How to represent nil for non-pointer types? Often zero.
            fprintf(stderr, "Warning: Nil constant for non-pointer type %s for var %s. Using zero.\n", getTypeName(llvm_type).c_str(), sym->name);
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
    forv_Sym(sym_iter, fa->pdb->if1->allsyms) {
        Var *var = sym_iter->var;
        if (!var) continue;
        if (!var || !var->sym) continue;
        Sym* sym = var->sym;

        // Skip unnamed temporaries - they should be computed locally, not created as globals
        if (sym->is_local || var->is_formal || !var->type || var->type->type_kind == Type_FUN || sym->is_fun || !sym->name) {
            continue;
        }
        if (var->llvm_value) continue;

        llvm::Type *gvar_llvm_type = getLLVMType(var->type);
        if (!gvar_llvm_type || gvar_llvm_type->isVoidTy()) {
            fprintf(stderr, "Skipping global var %s due to void or unmappable type\n", sym->name);
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
             // If it's not a string that getOrCreateLLVMStringConstant handles by creating its own GV.
             fprintf(stderr, "Warning: Global variable %s has no initializer, will be zero-initialized.\n", sym->name);
             initializer = llvm::Constant::getNullValue(gvar_llvm_type);
        }

        // Linkage: cg.cc implies most globals are external unless static.
        // IF1 doesn't have a direct static keyword for module-level vars in the same way.
        // Assume ExternalLinkage for now, unless sym->is_external is false (meaning internal to module).
        llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::ExternalLinkage;
        if (sym->is_builtin && !sym->is_external) { // Builtin but defined in this module
             linkage = llvm::GlobalValue::InternalLinkage;
        } else if (!sym->is_external && !sym->is_builtin) { // Not explicitly external, make internal
             // This needs more info from IF1 structure to be correct.
             // linkage = llvm::GlobalValue::InternalLinkage;
        }


        // Special case for string constants created by getLLVMConstant via getOrCreateLLVMStringConstant:
        // Their var->llvm_value might already be set to a GEP of a global.
        // We should not create another GlobalVariable for the Var itself in that case.
        if (var->type == sym_string && sym->is_constant && sym->constant) {
            if (!var->llvm_value) { // If getLLVMConstant didn't set it (e.g. if it wasn't called before)
                 var->llvm_value = getLLVMConstant(var); // This will create the string GV and return a GEP
            }
            // The llvm_value is already the pointer to the string, not the GV itself.
            // Debug info for the Var (as a pointer) can still be created.
        } else if (initializer) {
             llvm::GlobalVariable *gvar = new llvm::GlobalVariable(
                *TheModule,
                gvar_llvm_type,
                sym->is_read_only, // bool isConstant (i.e., read-only after init)
                linkage,
                initializer,
                sym->name ? sym->name : ("global_" + std::to_string(sym->id))
            );
            // Set alignment if specified in Sym
            if (sym->alignment > 0) {
                gvar->setAlignment(llvm::Align(sym->alignment));
            }
            var->llvm_value = gvar; // Store the GlobalVariable itself
        } else {
            fprintf(stderr, "Warning: Could not create global variable for %s, missing initializer and not string.\n", sym->name);
            continue;
        }

        // Add Debug Info for Global Variable
        if (DBuilder && CU && var->llvm_value) {
            llvm::DIFile* di_file = UnitFile; // Use CU's file
            unsigned line_num = sym->line();
            llvm::DIType* di_type = getLLVMDIType(var->type, di_file);

            if (di_type) {
                 // DIBuilder expects the actual GlobalVariable for createGlobalVariableExpression,
                 // not a GEP constant in case of strings.
                 llvm::GlobalVariable* gv_for_debug = llvm::dyn_cast<llvm::GlobalVariable>(var->llvm_value);
                 if (!gv_for_debug && var->type == sym_string && sym->is_constant && sym->constant) {
                     // If it's a string constant, var->llvm_value is a GEP. We need to find the underlying GV.
                     // This is a bit hacky. getOrCreateLLVMStringConstant needs to return the GV or make it findable.
                     // For now, we might not be able to create perfect DI for these string Vars.
                 }

                if (gv_for_debug) {
                    DBuilder->createGlobalVariableExpression(
                        CU,                             // Scope (Compile Unit)
                        gv_for_debug->getName(),        // Name
                        gv_for_debug->getName(),        // Linkage Name
                        di_file,                        // File
                        line_num,                       // Line number
                        di_type,                        // DI Type
                        linkage == llvm::GlobalValue::InternalLinkage, // isLocalToUnit (true if internal linkage)
                        true,                           // isDefined
                        nullptr                         // Expression (optional, for complex debug info)
                    );
                } else if (var->type == sym_string && llvm::isa<llvm::Constant>(var->llvm_value)) {
                     // For string vars that are GEPs to global constants.
                     // Create a simpler global variable debug info entry without direct GV linkage
                     // This is a fallback.
                     DBuilder->createGlobalVariableExpression(
                        CU,
                        sym->name ? sym->name : "string_ptr_global",
                        sym->name ? sym->name : "string_ptr_global",
                        di_file, line_num, di_type, true, true);
                }
            }
        }
    }
}

static cchar *num_string(Sym *s) {
  switch (s->num_kind) {
    default: assert(!"case");
    case IF1_NUM_KIND_UINT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1: return "_CG_bool";
        case IF1_INT_TYPE_8: return "_CG_uint8";
        case IF1_INT_TYPE_16: return "_CG_uint16";
        case IF1_INT_TYPE_32: return "_CG_uint32";
        case IF1_INT_TYPE_64: return "_CG_uint64";
        default: assert(!"case");
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1: return "_CG_bool";
        case IF1_INT_TYPE_8: return "_CG_int8";
        case IF1_INT_TYPE_16: return "_CG_int16";
        case IF1_INT_TYPE_32: return "_CG_int32";
        case IF1_INT_TYPE_64: return "_CG_int64";
        default: assert(!"case");
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32: return "_CG_float32";
        case IF1_FLOAT_TYPE_64: return "_CG_float64";
        case IF1_FLOAT_TYPE_128: return "_CG_float128";
        default: assert(!"case");
      }
      break;
  }
  return 0;
}

void llvm_build_type_strings(FA *fa) {
  #define S(_n) if1_get_builtin(fa->pdb->if1, #_n)->cg_string = "_CG_" #_n;
  #include "builtin_symbols.h"
  #undef S
  int f_index = 0;
  forv_Fun(f, fa->funs) {
    if (!f->live) continue;
    char s[100];
    if (f->sym->name) {
      if (f->sym->has.n > 1 && f->sym->has[1]->must_specialize)
        sprintf(s, "_CG_f_%d_%d", f->sym->id, f_index);
      else
        sprintf(s, "_CG_f_%d_%d", f->sym->id, f_index);
    } else
      sprintf(s, "_CG_f_%d_%d", f->sym->id, f_index);
    f->cg_string = dupstr(s);
    sprintf(s, "_CG_pf%d", f_index);
    f->cg_structural_string = dupstr(s);
    f->sym->cg_string = f->cg_structural_string;
    f_index++;
  }
  Vec<Var *> globals;
  Vec<Sym *> allsyms;
  collect_types_and_globals(fa, allsyms, globals);
  forv_Sym(s, allsyms) {
    if (s->num_kind) s->cg_string = num_string(s);
    else if (s->is_symbol) { s->cg_string = "_CG_symbol"; }
    else {
      if (!s->cg_string) {
        switch (s->type_kind) {
          default: s->cg_string = dupstr("_CG_any"); break;
          case Type_FUN: if (s->fun) break;
          case Type_RECORD: {
            if (s->has.n) {
              char ss[100]; sprintf(ss, "_CG_ps%d", s->id);
              s->cg_string = dupstr(ss);
            } else s->cg_string = "_CG_void";
            break;
          }
        }
      }
    }
  }
  forv_Sym(s, allsyms) {
    if (s->fun) s->cg_string = s->fun->cg_structural_string;
    else if (s->is_symbol) s->cg_string = sym_symbol->cg_string;
    if (s->type_kind == Type_SUM && s->has.n == 2) {
      if (s->has[0] == sym_nil_type) s->cg_string = s->has[1]->cg_string;
      else if (s->has[1] == sym_nil_type) s->cg_string = s->has[0]->cg_string;
    }
  }
}

void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun, cchar *input_filename) {
  fprintf(stderr, "DEBUG: llvm_codegen_print_ir started\n");
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
  cchar* src_filename = input_filename ? input_filename : (fa->pdb->if1->filename ? fa->pdb->if1->filename : "unknown.ifa");
  // Basic path handling, might need improvement for complex paths
  std::string full_path = src_filename;
  std::string dir = ".";
  std::string fname = full_path;
  size_t last_slash = full_path.find_last_of("/\\");
  if (last_slash != std::string::npos) {
    dir = full_path.substr(0, last_slash);
    fname = full_path.substr(last_slash + 1);
  }
  TheModule->setSourceFileName(fname); // Set source file name on the Module
  if (DBuilder) {
      UnitFile = DBuilder->createFile(fname, dir);
      CU = DBuilder->createCompileUnit(
          llvm::dwarf::DW_LANG_C, UnitFile, "ifa-compiler", 0 /*isOptimized*/, "" /*flags*/, 0 /*RV*/);

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

  fprintf(stderr, "DEBUG: fa->funs.n = %d before discovery\n", fa->funs.n);

  // Discover all reachable functions before translation
  // This ensures they all have proper liveness analysis
  discover_all_reachable_functions(fa, main_fun, all_funs);

  fprintf(stderr, "DEBUG: all_funs.n = %d after discovery\n", all_funs.n);

  // Build reverse call graph for constant recovery
  build_reverse_call_graph(fa);

  all_funs_global = &all_funs;
  
  // First pass: Create all function declarations (signatures only)
  // Only process live functions (like C backend does)
  forv_Fun(f, all_funs) {
    if (!f) {
        fprintf(stderr, "DEBUG: Found NULL fun in all_funs\n");
        continue;
    }
    if (!f->live) {
        fprintf(stderr, "DEBUG: Skipping non-live function %s (id %d)\n",
                f->sym->name ? f->sym->name : "(null)", f->sym->id);
        continue;
    }
    fprintf(stderr, "DEBUG: createFunction for %d\n", f->sym->id);
    createFunction(f, TheModule.get());
  }

  // Second pass: Translate all function bodies
  // (This is now done separately to ensure all functions are declared before any body is translated)
  forv_Fun(f, all_funs) {
    if (!f || !f->live || !f->llvm || f->is_external || !f->entry) {
        continue;
    }
    fprintf(stderr, "DEBUG: translateFunctionBody for %s (id %d)\n", f->sym->name, f->sym->id);
    translateFunctionBody(f);
  }

  // TODO: Implement PNode translation for each function's body
  // For now, functions will be declared but not defined (if not external)

  // Create a simple main function that calls the IF1 main_fun
  // This assumes the IF1 main_fun doesn't take argc, argv
  llvm::FunctionType *main_func_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(*TheContext), false);
  llvm::Function *llvm_main = llvm::Function::Create(main_func_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
  llvm::BasicBlock *main_entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_main);
  Builder->SetInsertPoint(main_entry_bb);

  if (main_fun->llvm) { // If IF1 main function was generated
    Builder->CreateCall(main_fun->llvm);
  } else {
    // This case should ideally not happen if main_fun is valid and processed
    fprintf(stderr, "Warning: IF1 main function '%s' not found or generated in LLVM module.\\n", main_fun->sym->name);
  }
  Builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));


  // Finalize DI builder after all IR and debug info is generated
  if (DBuilder) DBuilder->finalize();

  // Debug: Check for unterminated blocks before verification
  fprintf(stderr, "DEBUG: Pre-verification check for unterminated blocks:\n");
  for (llvm::Function &F : *TheModule) {
    for (llvm::BasicBlock &BB : F) {
      if (!BB.getTerminator()) {
        fprintf(stderr, "DEBUG: WARNING: Function %s has unterminated block %s (size=%zu)\n",
                F.getName().str().c_str(), BB.getName().str().c_str(), BB.size());
        fprintf(stderr, "DEBUG: Block instructions:\n");
        int idx = 0;
        for (llvm::Instruction &I : BB) {
          fprintf(stderr, "DEBUG:   [%d] ", idx++);
          I.print(llvm::errs());
          fprintf(stderr, "\n");
        }
      }
    }
  }

  // Verify the module
  std::string error_str;
  llvm::raw_string_ostream rso(error_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    fail("LLVM module verification failed: %s", rso.str().c_str());
    if (fp != stderr) { // Print to file as well if it's not stderr
        std::string err_msg = rso.str();
        std::stringstream ss(err_msg);
        std::string segment;
        while(std::getline(ss, segment, '\n')) {
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
llvm::Value* getLLVMValue(Var *var, Fun *ifa_fun) {
    if (!var) {
        fail("Null Var provided to getLLVMValue");
        return nullptr;
    }
    fprintf(stderr, "DEBUG: getLLVMValue var=%p, sym=%s, type=%p\n", var, var->sym ? var->sym->name : "null", var->type);
    if (var->llvm_value) {
        llvm::Value *val = var->llvm_value;
        llvm::Function *this_func = ifa_fun->llvm;

        fprintf(stderr, "DEBUG: getLLVMValue found cached llvm_value for var %s (id %d)\n",
                var->sym ? var->sym->name : "(null)", var->id);
        // Check what kind of value this is
        if (llvm::isa<llvm::GlobalVariable>(val)) {
            fprintf(stderr, "DEBUG:   It's a GlobalVariable (pointer to value)\n");
        } else if (llvm::isa<llvm::AllocaInst>(val)) {
            fprintf(stderr, "DEBUG:   It's an AllocaInst (pointer to value)\n");
        } else if (llvm::isa<llvm::Instruction>(val)) {
            fprintf(stderr, "DEBUG:   It's an Instruction (direct value)\n");
        } else if (llvm::isa<llvm::Argument>(val)) {
            fprintf(stderr, "DEBUG:   It's an Argument (direct value)\n");
        } else if (llvm::isa<llvm::Constant>(val)) {
            fprintf(stderr, "DEBUG:   It's a Constant\n");
        }


        bool scope_mismatch = false;
        if (llvm::isa<llvm::Instruction>(val)) {
             llvm::Function *val_func = llvm::cast<llvm::Instruction>(val)->getFunction();
             if (val_func != this_func) {
                 fprintf(stderr, "DEBUG: Scope mismatch for var %s (id %d). Val func: %s, Current func: %s. Clearing cache.\n",
                         var->sym->name, var->id, 
                         val_func ? val_func->getName().str().c_str() : "null",
                         this_func ? this_func->getName().str().c_str() : "null");
                 scope_mismatch = true;
             }
        } else if (llvm::isa<llvm::Argument>(val)) {
             llvm::Function *val_func = llvm::cast<llvm::Argument>(val)->getParent();
             if (val_func != this_func) {
                 fprintf(stderr, "DEBUG: Argument Scope mismatch for var %s (id %d). Val func: %s, Current func: %s. Clearing cache.\n",
                         var->sym->name, var->id,
                         val_func ? val_func->getName().str().c_str() : "null",
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
                return Builder->CreateLoad(load_type, var->llvm_value, var->sym->name ? (std::string(var->sym->name) + ".load") : "");
            }
            if (llvm::isa<llvm::GlobalVariable>(var->llvm_value)) {
                llvm::GlobalVariable *gv = llvm::cast<llvm::GlobalVariable>(var->llvm_value);
                llvm::Type *load_type = gv->getValueType();
                return Builder->CreateLoad(load_type, var->llvm_value, var->sym && var->sym->name ? (std::string(var->sym->name) + ".load") : "global.load");
            }
            return var->llvm_value;
        }
    }

    // Handle global variables or constants if not mapped through allocas
    // Handle function symbols - return the function pointer
    if (var->sym && var->sym->is_fun) {
        fprintf(stderr, "DEBUG: getLLVMValue for function symbol %s (id=%d)\n",
                var->sym->name ? var->sym->name : "unnamed", var->sym->id);
        // Find the function by symbol
        llvm::Function *func = nullptr;
        if (all_funs_global) {
            forv_Fun(fx, *all_funs_global) {
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
        fprintf(stderr, "WARNING: Function %s not found in module\n",
                var->sym->name ? var->sym->name : "unnamed");
        return nullptr;
    }

    // Handle symbols (like operator symbols: '<', '+', etc.)
    if (var->sym && var->sym->is_symbol) {
        fprintf(stderr, "DEBUG: getLLVMValue for symbol %s (id=%d)\n", var->sym->name ? var->sym->name : "unnamed", var->sym->id);
        // Symbols are represented as integers in the C backend (id)
        // Return the symbol ID as a constant integer
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), var->sym->id);
    }

    // Handle constants (literals)
    if (var->sym && (var->sym->is_constant || var->sym->imm.const_kind != IF1_NUM_KIND_NONE || var->type == sym_nil_type)) {
        llvm::Constant *const_val = getLLVMConstant(var);
        return const_val;
    }

    // Handle tuple field formals: variables marked as both is_local and is_formal
    // These need to be extracted from the tuple argument
    if (var->sym && var->sym->is_local && var->is_formal) {
        fprintf(stderr, "DEBUG: Variable %s (id=%d) is both local and formal - needs tuple extraction\n",
                var->sym->name ? var->sym->name : "(null)", var->sym->id);

        // The tuple argument should be in the function's arguments
        // Find it by looking for an argument with a struct type
        llvm::Function *llvm_func = ifa_fun->llvm;
        llvm::Argument *tuple_arg = nullptr;

        fprintf(stderr, "DEBUG: Function %s has %ld arguments\n",
                ifa_fun->sym->name ? ifa_fun->sym->name : "(null)",
                llvm_func->arg_size());

        for (llvm::Argument &arg : llvm_func->args()) {
            llvm::Type *arg_type = arg.getType();
            fprintf(stderr, "DEBUG:   Arg type: isPointer=%d, isStruct=%d\n",
                    arg_type->isPointerTy() ? 1 : 0,
                    arg_type->isStructTy() ? 1 : 0);

            // LLVM 18 uses opaque pointers - take first pointer/struct as tuple
            if (arg_type->isPointerTy() || arg_type->isStructTy()) {
                tuple_arg = &arg;
                fprintf(stderr, "DEBUG: Found tuple_arg (LLVM Argument)\n");
                break;
            }
        }

        if (tuple_arg) {
            fprintf(stderr, "DEBUG: Looking for tuple formal variable to get struct type\n");

            Var *tuple_formal_var = nullptr;
            forv_Var(fv, ifa_fun->fa_all_Vars) {
                if (fv && fv->is_formal && fv->llvm_value == tuple_arg) {
                    tuple_formal_var = fv;
                    fprintf(stderr, "DEBUG: Found tuple formal var by llvm_value match: sym=%s (id=%d)\n",
                            fv->sym ? fv->sym->name : "(null)",
                            fv->sym ? fv->sym->id : -1);
                    break;
                }
            }

            if (!tuple_formal_var) {
                forv_Var(fv, ifa_fun->fa_all_Vars) {
                    if (fv && fv->is_formal && !fv->sym->is_local && fv->type &&
                        fv->type->type_kind == Type_RECORD) {
                        tuple_formal_var = fv;
                        fprintf(stderr, "DEBUG: Found tuple formal var by type: sym=%s (id=%d)\n",
                                fv->sym ? fv->sym->name : "(null)",
                                fv->sym ? fv->sym->id : -1);
                        break;
                    }
                }
            }

            if (!tuple_formal_var || !tuple_formal_var->type) {
                fprintf(stderr, "WARNING: Could not find tuple formal variable for field extraction\n");
            } else {
                llvm::Type *tuple_struct_type = getLLVMType(tuple_formal_var->type);
                if (!tuple_struct_type || !tuple_struct_type->isStructTy()) {
                    fprintf(stderr, "WARNING: Tuple formal variable has invalid struct type\n");
                } else {
                    // Use MPosition to determine field index (not sequential counting!)
                    // Tuple operators like (a, +, b) have field indices 0, 1, 2, but + is not a formal.
                    // Parallels cg.cc:542-550 (write_arg_position) and cg.cc:715-725 (write_c_args)
                    int formal_idx = -1;
                    MPosition *found_pos = nullptr;

                    forv_MPosition(p, ifa_fun->positional_arg_positions) {
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
                        fprintf(stderr, "WARNING: Could not find MPosition or field index for var %s (formal_idx=%d, found_pos=%p)\n",
                                var->sym->name ? var->sym->name : "(null)", formal_idx, (void*)found_pos);
                    } else {
                        fprintf(stderr, "DEBUG: Extracting field %d from tuple argument for var %s (MPos.n=%d)\n",
                                formal_idx, var->sym->name ? var->sym->name : "(null)", found_pos->pos.n);

                        llvm::Value *field_ptr = Builder->CreateStructGEP(
                            tuple_struct_type,
                            tuple_arg,
                            formal_idx,
                            var->sym->name ? (std::string(var->sym->name) + ".ptr") : "field.ptr"
                        );

                        llvm::Type *field_type = getLLVMType(var->type);
                        llvm::Value *field_val = Builder->CreateLoad(
                            field_type,
                            field_ptr,
                            var->sym->name ? (std::string(var->sym->name) + ".val") : "field.val"
                        );

                        var->llvm_value = field_val;
                        return field_val;
                    }
                }
            }
        } else {
            fprintf(stderr, "WARNING: No tuple argument found for var %s (function has 0 args, likely unspecialized template)\n",
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
             llvm::GlobalVariable* gv = M->getGlobalVariable(var->sym->name);
             if (gv) return gv;
             // If not found, fail or return null
         } else {
             // Null name global?
             fprintf(stderr, "DEBUG: Global Sym has no name. id=%d\n", var->sym->id);
             return nullptr; // Will trigger fail in caller
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
    } else {
        // It's an SSA variable, its llvm_value should be the instruction that defines it.
        // Or it's an argument.
        var->llvm_value = val;
        if (val->getType() != var->llvm_type) { // Update cached type if different
             if (var->llvm_type) {
                fprintf(stderr, "Warning: LLVM value type mismatch for var %s. Expected %s, got %s. Updating cache.\n",
                var->sym && var->sym->name ? var->sym->name : "??",
                getTypeName(var->llvm_type).c_str(), getTypeName(val->getType()).c_str());
             }
             var->llvm_type = val->getType();
        }
    }
}


void llvm_codegen_write_ir(FA *fa, Fun *main, cchar *input_filename) {
  char fn[512];
  strncpy(fn, input_filename, sizeof(fn) - 1);
  fn[sizeof(fn)-1] = '\0';
  char *dot = strrchr(fn, '.');
  if (dot) {
    strcpy(dot, ".ll"); // Replace extension with .ll
  } else {
    strcat(fn, ".ll"); // Append .ll if no extension
  }

  FILE *fp = fopen(fn, "w");
  if (!fp) {
    fail("Unable to open file %s for writing LLVM IR", fn);
    return;
  }
  llvm_codegen_print_ir(fp, fa, main, input_filename);
  fclose(fp);
  fprintf(stderr, "LLVM IR written to %s\n", fn);
}

int llvm_codegen_compile(cchar *input_filename) {
  char ll_file[512];
  char obj_file[512];
  char cmd[1024];

  // Construct .ll filename from input_filename (e.g., test.ifa -> test.ll)
  strncpy(ll_file, input_filename, sizeof(ll_file) - 1);
  ll_file[sizeof(ll_file)-1] = '\0';
  char *dot_ll = strrchr(ll_file, '.');
  if (dot_ll) strcpy(dot_ll, ".ll");
  else strcat(ll_file, ".ll");

  // Construct .o filename from input_filename (e.g., test.ifa -> test.o)
  strncpy(obj_file, input_filename, sizeof(obj_file) - 1);
  obj_file[sizeof(obj_file)-1] = '\0';
  char *dot_o = strrchr(obj_file, '.');
  if (dot_o) strcpy(dot_o, ".o");
  else strcat(obj_file, ".o");

  // Ensure TheModule is initialized (e.g. by a prior call to write_ir or print_ir)
  // or re-initialize if this function is called standalone.
  // For now, we assume TheModule and its TargetTriple are set.
  // If TheModule is null, this path is problematic.
  // A robust solution would pass FA* and Fun* here too, or ensure state.

  // Compile LLVM IR directly to object file using clang
  // This handles debug info directives properly
  sprintf(cmd, "clang -c -fPIC %s -o %s", ll_file, obj_file);
  int res = system(cmd);

  if (res != 0) {
    fail("LLVM IR compilation failed for %s using clang.", ll_file);
    return res;
  }

  fprintf(stderr, "LLVM IR from %s compiled to %s\n", ll_file, obj_file);

  // Step 3: Link the object file to create executable
  char exe_file[512];
  strncpy(exe_file, input_filename, sizeof(exe_file) - 1);
  exe_file[sizeof(exe_file)-1] = '\0';
  char *dot_exe = strrchr(exe_file, '.');
  if (dot_exe) *dot_exe = '\0';  // Remove extension to get executable name

  // Link with necessary libraries (matching Makefile.cg)
  sprintf(cmd, "clang %s -o %s -lgc -lm -lpcre -ldl -lrt", obj_file, exe_file);
  res = system(cmd);

  if (res != 0) {
    fail("Linking failed for %s", obj_file);
    return res;
  }

  fprintf(stderr, "Executable %s created successfully\n", exe_file);
  return 0;
}

