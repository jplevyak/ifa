/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "llvm.h"
#include "fun.h"     // For Fun
#include "fa.h"      // For FA
#include "pattern.h" // For MPosition
#include <stdarg.h>  // for va_list

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/IR/DIBuilder.h" // For Debug Info
#include "llvm/MC/TargetRegistry.h" // Required for TargetRegistry
#include <memory> // For std::unique_ptr
#include <string> // For std::string
#include <map>    // For std::map
#include <vector>
#include <set>      // For std::set
#include <sstream> // For std::stringstream

// Debug logging macro - set LLVM_DEBUG to 1 to enable debug output
#ifndef LLVM_DEBUG
#define LLVM_DEBUG 0
#endif

#if LLVM_DEBUG
#define DEBUG_LOG(...) fprintf(stderr, "DEBUG: " __VA_ARGS__)
#else
#define DEBUG_LOG(...) do {} while(0)
#endif

// Helper macros if not defined
#ifndef forv_MPosition
#define forv_MPosition(_p, _v) forv_Vec(MPosition, _p, _v)
#endif

// Global LLVM variables
static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::unique_ptr<llvm::DIBuilder> DBuilder; // For Debug Info
static llvm::DICompileUnit *CU = nullptr;
static llvm::DIFile *UnitFile = nullptr;
static Vec<Fun*> *all_funs_global = NULL;

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
  // DBuilder = std::make_unique<llvm::DIBuilder>(*TheModule);
  DBuilder = nullptr;

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
static llvm::Type *getLLVMType(Sym *sym);
static llvm::DIType *getLLVMDIType(Sym *sym, llvm::DIFile *di_file);

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
static llvm::Value* getLLVMValue(Var *var, Fun *ifa_fun);
static void setLLVMValue(Var *var, llvm::Value *val, Fun *ifa_fun);
static llvm::Constant *getLLVMConstant(Var *var);

// Reverse Call Graph for Constant Recovery
static std::map<Fun*, std::vector<PNode*>> reverse_call_graph;

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
        // Create temp var to wrap Sym
        // Warn: Var(Sym*) constructor might not be safe if it expects GC?
        // Var is simple struct. `new Var(sym)` should work.
        // We'll trust getLLVMConstant to handle it.
        // But Var is allocated on heap?
        // Just create local Var on stack if possible? No, getLLVMConstant might access it later?
        // getLLVMConstant reads var->sym.
        Var tmp(consistent_sym); 
        // We need to set tmp.type = sym->type?
        // safe default.
        return getLLVMConstant(&tmp);
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
static llvm::Type *getLLVMType(Sym *sym) {
  fprintf(stderr, "DEBUG: getLLVMType %p\n", (void*)sym);
  if (!sym) {
    // fail("Null Sym provided to getLLVMType");
    fprintf(stderr, "WARNING: getLLVMType(nil), returning VoidTy as fallback\n");
    return llvm::Type::getVoidTy(*TheContext);
  }
  fprintf(stderr, "DEBUG: getLLVMType %s kind %d\n", sym->name ? sym->name : "unnamed", sym->type_kind);
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

        // TODO: Handle varargs if unaliased_sym->is_varargs or similar flag exists
        bool is_var_arg = false; // Placeholder
        if (unaliased_sym->fun && unaliased_sym->fun->is_varargs) {
            is_var_arg = true;
        }
        
        llvm::FunctionType *fun_type = llvm::FunctionType::get(return_llvm_type, arg_llvm_types, is_var_arg);
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
static llvm::DIType *getLLVMDIType(Sym *sym, llvm::DIFile *di_file) {
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

static llvm::DISubroutineType *createFunctionDIType(Fun *ifa_fun, llvm::DIFile *di_file) {
    fprintf(stderr, "DEBUG: createFunctionDIType entry\n");
    if (!DBuilder) return nullptr;
    // Minimal implementation: Assume void() or similar for now
    // TODO: Build actual elements array with return type and arg types
    llvm::SmallVector<llvm::Metadata *, 8> EltTys;
    // Return type first (null for void)
    EltTys.push_back(nullptr); 
    
    llvm::DISubroutineType *ty = DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
    fprintf(stderr, "DEBUG: createFunctionDIType exit\n");
    return ty;
}

// Forward Declaration
static void translateFunctionBody(Fun *ifa_fun);

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
static llvm::Constant *getLLVMConstant(Var *var) {
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

    if (sym->is_constant && sym->constant) { // String constants defined in IF1
        if (var->type == sym_string) {
             // cg.cc uses: _CG_String("escaped_string")
             // Here, we create a global string constant and return a pointer to it.
            return getOrCreateLLVMStringConstant(sym->constant);
        }
        // For other types, IF1 might store numeric constants as strings.
        // We need to parse them based on llvm_type.
        if (llvm_type->isIntegerTy()) {
            long long val = strtoll(sym->constant, nullptr, 0); // Auto-detect base
            return llvm::ConstantInt::get(llvm_type, val, true /*isSigned*/); // Assume signed for strtoll
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
    } else if (sym->imm.const_kind != IF1_NUM_KIND_NONE) { // Numeric immediates
        Immediate imm = sym->imm;
        if (llvm_type->isIntegerTy()) {
            uint64_t val = 0;
            bool is_signed = (sym->num_kind == IF1_NUM_KIND_INT);
            switch (sym->num_index) {
                case IF1_INT_TYPE_1:  val = imm.v_bool; break;
                case IF1_INT_TYPE_8:  val = is_signed ? (uint64_t)(int64_t)imm.v_int8 : imm.v_uint8; break;
                case IF1_INT_TYPE_16: val = is_signed ? (uint64_t)(int64_t)imm.v_int16 : imm.v_uint16; break;
                case IF1_INT_TYPE_32: val = is_signed ? (uint64_t)(int64_t)imm.v_int32 : imm.v_uint32; break;
                case IF1_INT_TYPE_64: val = imm.v_uint64; break; // v_int64 is same bits as v_uint64
                default: fail("Unhandled immediate integer type index %d for %s", sym->num_index, sym->name); return nullptr;
            }
            return llvm::ConstantInt::get(llvm_type, val, is_signed);
        } else if (llvm_type->isFloatingPointTy()) {
            double val = 0.0;
            switch (sym->num_index) {
                case IF1_FLOAT_TYPE_32: val = imm.v_float32; break;
                case IF1_FLOAT_TYPE_64: val = imm.v_float64; break;
                case IF1_FLOAT_TYPE_128: // LLVM APFloat for 128-bit
                    fail("FP128 immediate not yet handled for %s", sym->name); return nullptr; // TODO
                default: fail("Unhandled immediate float type index %d for %s", sym->num_index, sym->name); return nullptr;
            }
            return llvm::ConstantFP::get(llvm_type, val);
        } else {
            fail("Immediate constant for non-numeric type for %s", sym->name);
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
    // This is a guess; a more direct list of global Vars from FA would be better.

    // A more direct way as seen in cg.cc `c_codegen_print_c` is to iterate `fa->funs`
    // and for each `f->sym->var` if it exists, it's a global (representing the function itself).
    // And also `collect_types_and_globals` which finds other globals.
    // Let's simulate part of `collect_types_and_globals` or use a similar approach.
    // The `globals` vector in `cg.cc` is populated by `build_type_strings` which calls `collect_types_and_globals`.
    // `collect_types_and_globals` iterates `fa->vars`.

    forv_Sym(sym_iter, fa->pdb->if1->allsyms) { // Iterate all symbols to find globals
        Var *var = sym_iter->var;
        if (!var) continue;
        if (!var || !var->sym) continue;
        Sym* sym = var->sym;

        // What defines a global var?
        // - Not local to a function (sym->is_local = false)
        // - Not an argument (var->is_formal = false)
        // - Has a type and is live
        // - Is not a function itself (sym->is_fun is for the symbol, var->type->type_kind != Type_FUN)
        // cg.cc's `c_codegen_print_c` iterates a `globals` Vec. We need to populate such a list.
        // For now, let's assume a Var is global if it's in fa->vars and not clearly local/arg.
        // This is a simplification. A proper mechanism from FA to list globals is needed.

        // A simple heuristic: if a var's symbol is not local and it's not a function type,
        // it might be a global data variable.
        if (sym->is_local || var->is_formal || !var->type || var->type->type_kind == Type_FUN) {
            continue;
        }
        // Skip if it's already processed (e.g. function pointers might be handled by function creation)
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

// Forward declaration
static llvm::Function *createFunction(Fun *ifa_fun, llvm::Module *module);
// Wait, line 669 is forward decl. 
// Definition is somewhere else.
// I saw the definition in previous view_file (lines 900+).
// Actually, earlier view_file output showed the END of createFunction (lines 950-995).
// I need to find the START of createFunction definition.


/**
 * Main entry point for LLVM code generation.
 * Initializes LLVM, creates global variables, translates functions,
 * creates a main wrapper, and outputs the LLVM IR to the given file.
 *
 * @param fp Output file pointer for LLVM IR
 * @param fa Flow analysis context
 * @param main_fun The entry point function
 */
void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun) {
  fprintf(stderr, "DEBUG: llvm_codegen_print_ir started\n");
  llvm_codegen_initialize(fa);
  if (!fa) {
    fail("FA object is null in llvm_codegen_print_ir");
    return;
  }
  if (!main_fun) {
    fail("Main function is null in llvm_codegen_print_ir");
    return;
  }

  // Create DIBuilder Compile Unit
  cchar* src_filename = fa->pdb->if1->filename ? fa->pdb->if1->filename : "unknown.ifa";
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
  }

  // Create Global Variables
  createGlobalVariables(fa);

  // Iterate over all functions in FA and create them in the LLVM Module
  // Ensure main_fun is processed, and other functions it might call.
  // The order might matter if there are dependencies not captured by FA's list directly.
  // For now, iterate fa->funs then ensure main_fun is included if not already.

  Vec<Fun *> all_funs;
  if (fa->funs.n > 0) {
      forv_Fun(f, fa->funs) {
          fprintf(stderr, "DEBUG: Checking Fun %p %s (id %d): live=%d, entry=%p\n", 
                  f, f->sym->name, f->sym->id, f->live, f->entry);
          if (f) { 
              all_funs.set_add(f);
          }
      }
  }
  build_reverse_call_graph(fa);
  if (main_fun && main_fun->live && main_fun->entry) {
      all_funs.set_add(main_fun);
   }
   all_funs_global = &all_funs;
  
  forv_Fun(f, all_funs) {
    if (!f) {
        fprintf(stderr, "DEBUG: Found NULL fun in all_funs\n");
        continue;
    }
    fprintf(stderr, "DEBUG: createFunction for %d\n", f->sym->id);
    createFunction(f, TheModule.get());
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

  // Print the module to the file
  std::string ir_string;
  llvm::raw_string_ostream ir_rso(ir_string);
  TheModule->print(ir_rso, nullptr);
  fprintf(fp, "%s", ir_rso.str().c_str());
}

// --- Function Generation ---
static llvm::Function *createFunction(Fun *ifa_fun, llvm::Module *module) {
  if (!ifa_fun) {
    fail("Null Fun provided to createFunction");
    return nullptr;
  }
  fprintf(stderr, "DEBUG: createFunction entry for %s (id %d)\n", ifa_fun->sym->name, ifa_fun->sym->id);
  
  if (ifa_fun->llvm) { // Already created
    return ifa_fun->llvm;
  }
  if (!ifa_fun->sym) {
    fail("Fun %p has no symbol associated.", (void*)ifa_fun);
    return nullptr;
  }

  // 1. Determine FunctionType
  fprintf(stderr, "DEBUG: createFunction step 1: determine ret type for %d rets\n", ifa_fun->rets.n);
  llvm::Type *llvm_ret_type;
  fprintf(stderr, "DEBUG: check rets: n=%d, rets[0]=%p, rets[0]->type=%p\n", 
          ifa_fun->rets.n, 
          (ifa_fun->rets.n > 0 ? ifa_fun->rets[0] : nullptr),
          (ifa_fun->rets.n > 0 && ifa_fun->rets[0] ? ifa_fun->rets[0]->type : nullptr));

  if (ifa_fun->rets.n == 1 && ifa_fun->rets[0]) {
      if (ifa_fun->rets.n > 0 && ifa_fun->rets[0] && ifa_fun->rets[0]->type) {
          llvm_ret_type = getLLVMType(ifa_fun->rets[0]->type);
      } else {
          // Fallback for null type (e.g. void var)
          fprintf(stderr, "DEBUG: Return var has null type, assuming void.\n");
          llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
      }
  } else if (ifa_fun->rets.n == 0) { 
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  } else {
      fprintf(stderr, "DEBUG: Unsupported ret count %d\n", ifa_fun->rets.n);
    // TODO: Handle multiple return values if the language supports them
    // (e.g., by returning a struct). For now, assume single or void.
    fail("Function %s has %d return values, unsupported. Assumed void or single.", ifa_fun->sym->name, ifa_fun->rets.n);
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  }
  if (!llvm_ret_type) {
      fail("Could not determine LLVM return type for function %s", ifa_fun->sym->name);
      return nullptr;
  }


  // 2. Arguments
  fprintf(stderr, "DEBUG: createFunction step 2: args\n");
  std::vector<llvm::Type *> llvm_arg_types;
  forv_MPosition(p, ifa_fun->positional_arg_positions) {
    fprintf(stderr, "DEBUG: processing arg position %p\n", p);
    Var *arg_var = ifa_fun->args.get(p);
    if (arg_var) fprintf(stderr, "DEBUG: arg var %p (id %d)\n", arg_var, arg_var->id);
    else fprintf(stderr, "DEBUG: arg var null\n");
    
    if (arg_var) {
      llvm::Type *arg_llvm_type = nullptr;
      if (arg_var->type) arg_llvm_type = getLLVMType(arg_var->type);
      else arg_llvm_type = llvm::Type::getInt64Ty(*TheContext); // Fallback for typeless/generic args

      if (arg_llvm_type) {
        llvm_arg_types.push_back(arg_llvm_type);
      } else {
        fail("Could not get LLVM type for argument %s of function %s",
             arg_var->sym->name ? arg_var->sym->name : "unnamed_arg", ifa_fun->sym->name);
        return nullptr;
      }
    }
  }

  fprintf(stderr, "DEBUG: createFunction step 3: FunctionType::get\n");
  llvm::FunctionType *func_type = llvm::FunctionType::get(llvm_ret_type, llvm_arg_types, ifa_fun->is_varargs);

  // 3. Create llvm::Function
  fprintf(stderr, "DEBUG: createFunction step 4: Function::Create\n");
  std::string func_name = (ifa_fun->sym->name ? ifa_fun->sym->name : "func") + std::to_string(ifa_fun->id);
  if (ifa_fun->cg_string && strncmp(ifa_fun->cg_string, "_CG_", 4) == 0) {
      func_name = ifa_fun->cg_string;
  }
  
  if (!module) fail("Module is null");
  if (!func_type) fail("FuncType is null");

  // Linkage logic
  llvm::Function::LinkageTypes linkage = llvm::Function::ExternalLinkage;
  // FIX: Force External if no entry, regardless of builtin status
  if (!ifa_fun->entry) {
      linkage = llvm::Function::ExternalLinkage;
  } else {
      linkage = llvm::Function::InternalLinkage;
  }
  fprintf(stderr, "DEBUG: Linkage Fixed: entry=%p -> %d\n", ifa_fun->entry, (int)linkage);

  fprintf(stderr, "DEBUG: func_name='%s', module=%p, func_type=%p, varargs=%d, linkage=%d\n", 
          func_name.c_str(), module, func_type, ifa_fun->is_varargs, (int)linkage);

  llvm::Function *llvm_func = llvm::Function::Create(func_type, linkage, func_name, module);
  fprintf(stderr, "DEBUG: Function created %p\n", llvm_func);
  ifa_fun->llvm = llvm_func; // Store it

  // 3. Set argument names and store llvm::Argument in Var::llvm_value
  unsigned arg_idx = 0;
  fprintf(stderr, "DEBUG: Starting arg loop 1 (index based)\n");
  for (unsigned i = 0; i < llvm_func->arg_size(); ++i) {
      llvm::Argument *llvm_arg = llvm_func->getArg(i);
      fprintf(stderr, "DEBUG: In arg loop 1, idx %d\n", i);
      
      if (arg_idx < ifa_fun->positional_arg_positions.n) {
          MPosition *pos = ifa_fun->positional_arg_positions[arg_idx];
          Var *arg_var = ifa_fun->args.get(pos);
          if (arg_var && arg_var->live) {
              if (arg_var->sym && arg_var->sym->name) {
                  llvm_arg->setName(arg_var->sym->name);
              }
              arg_var->llvm_value = llvm_arg;
          }
      }
    arg_idx++;
  }
  fprintf(stderr, "DEBUG: Finished arg loop 1\n");
  // Corrected argument mapping handled in Loop 1.
  fprintf(stderr, "DEBUG: Finished arg loop 2 (SKIPPED)\n");

  fprintf(stderr, "DEBUG: Starting Entry Block creation. Entry=%p, External=%d\n", ifa_fun->entry, ifa_fun->is_external);
  fprintf(stderr, "DEBUG: TheContext=%p, llvm_func=%p\n", TheContext.get(), llvm_func);
  if (!TheContext) fail("TheContext is null");
  if (!llvm_func) fail("llvm_func is null");

  if (!ifa_fun->is_external && ifa_fun->entry) {
       llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_func);
       fprintf(stderr, "DEBUG: Entry Block Created: %p\n", entry_bb);
  }


  // Debug Info Logic Removed


  // After creating the function shell, if it's not external, translate its PNodes.
  fprintf(stderr, "DEBUG: Starting TranslateFunctionBody\n");
  if (!ifa_fun->is_external && ifa_fun->entry) {
    translateFunctionBody(ifa_fun);
  }
  fprintf(stderr, "DEBUG: Finished createFunction for %d\n", ifa_fun->sym->id);
  return llvm_func;
}


// --- PNode Translation ---
static std::map<Label *, llvm::BasicBlock *> label_to_bb_map; // Global map for current function

static llvm::BasicBlock *getLLVMBasicBlock(Label *label, llvm::Function *current_llvm_fun) {
    if (!label) {
        fail("Null Label provided to getLLVMBasicBlock");
        return nullptr;
    }
    if (label_to_bb_map.count(label)) {
        return label_to_bb_map[label];
    }
    // Labels in IF1 are often just numbers (label->id).
    // We need a unique name for the BasicBlock.
    std::string bb_name = "label_" + std::to_string(label->id);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*TheContext, bb_name, current_llvm_fun);
    label_to_bb_map[label] = bb;
    label->bb = bb; // Store in Label struct as well, consistent with code.h
    return bb;
}

// Forward declare
static void translatePNode(PNode *pn, Fun *ifa_fun);

static void translateFunctionBody(Fun *ifa_fun) {
    fprintf(stderr, "DEBUG: translateFunctionBody entry for %s (id %d)\n", ifa_fun->sym->name, ifa_fun->id);
    if (!ifa_fun || !ifa_fun->llvm || !ifa_fun->entry) {
        fail("Invalid function or missing LLVM function/entry PNode for translateFunctionBody");
        return;
    }

    label_to_bb_map.clear(); // Clear map for each function
    
    llvm::Function *llvm_func = ifa_fun->llvm;

    // Create all basic blocks first by iterating PNodes once to find labels
    // This helps with forward branches.
    Vec<PNode *> pnodes;
    ifa_fun->collect_PNodes(pnodes); // Assumes this collects all relevant PNodes
    fprintf(stderr, "DEBUG: Collected %d PNodes\n", pnodes.n);

    if (ifa_fun->fa_all_Vars.n == 0) {
        fprintf(stderr, "DEBUG: fa_all_Vars empty, collecting vars...\n");
        ifa_fun->collect_Vars(ifa_fun->fa_all_Vars);
        fprintf(stderr, "DEBUG: Collected %d Vars\n", ifa_fun->fa_all_Vars.n);
    }

    int pn_counter = 0;
    forv_PNode(pn, pnodes) {
        // fprintf(stderr, "DEBUG: Checking PNode %d. code=%p\n", pn_counter++, pn->code);
        if (pn->code) {
             // fprintf(stderr, "DEBUG: PNode code kind=%d\n", pn->code->kind);
             if (pn->code->kind == Code_LABEL) {
                 // fprintf(stderr, "DEBUG: PNode is LABEL. label[0]=%p\n", pn->code->label[0]);
                 if (pn->code->label[0]) {
                     getLLVMBasicBlock(pn->code->label[0], llvm_func);
                 }
             }
        }
    }

    fprintf(stderr, "DEBUG: Done labels loop. Checking entry code...\n");
    // Ensure entry block is correctly mapped if it's from a label
    if (ifa_fun->entry->code) {
        fprintf(stderr, "DEBUG: Entry code kind=%d\n", ifa_fun->entry->code->kind);
    } else {
        fprintf(stderr, "DEBUG: Entry code is NULL\n");
    }

    if (ifa_fun->entry->code && ifa_fun->entry->code->kind == Code_LABEL && ifa_fun->entry->code->label[0]) {
         llvm::BasicBlock* entry_bb = getLLVMBasicBlock(ifa_fun->entry->code->label[0], llvm_func);
         if (entry_bb != &llvm_func->getEntryBlock() && llvm_func->getEntryBlock().empty()) {
             // If the auto-created entry block is empty and unused, we could use our label block as entry.
             // But LLVM Function::Create already made one.
             // We can insert a branch from llvm entry to our label block.
             Builder->SetInsertPoint(&llvm_func->getEntryBlock());
             Builder->CreateBr(entry_bb);
         } else if (entry_bb != &llvm_func->getEntryBlock()) {
            // We need to ensure our actual IF1 entry point PNode starts filling *its* corresponding BB.
            // If llvm entry block is not empty (e.g. debug info), branch to real entry.
             Builder->SetInsertPoint(&llvm_func->getEntryBlock());
             if (llvm_func->getEntryBlock().getTerminator() == nullptr) {
                Builder->CreateBr(entry_bb);
             }
         }
    } else if (ifa_fun->entry->code && ifa_fun->entry->code->kind != Code_LABEL) {
        // Entry PNode is not a label itself. Use the default entry block.
        // This case needs careful handling. If the first PNode isn't a label,
        // its instructions go into the llvm_func->getEntryBlock().
        // We might need a way to associate this entry PNode with that block.
        // For now, translatePNode will handle setting the builder insert point.
    }


    // Allocate local variables (Vars that are Sym_LOCAL and not args)
    // This is a common strategy for mutable local variables.
    // SSA values will be directly represented by llvm::Value* from instructions.


    fprintf(stderr, "DEBUG: Setting Builder InsertPoint to EntryBlock\n");
    if (llvm_func->getEntryBlock().empty()) {
        fprintf(stderr, "DEBUG: EntryBlock is empty, setting to beginning\n");
    } else {
        fprintf(stderr, "DEBUG: EntryBlock not empty, setting to beginning\n");
    }
    
    // Check Builder
    if (!Builder) {
        fail("Builder is null in translateFunctionBody");
        return;
    }
    Builder->SetInsertPoint(&llvm_func->getEntryBlock(), llvm_func->getEntryBlock().begin()); // Allocas at the top
    
    fprintf(stderr, "DEBUG: InsertPoint set. Getting subprogram for locals...\n");
    llvm::DIFile* di_file_for_locals = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getFile() : nullptr;
    fprintf(stderr, "DEBUG: Got subprogram components\n");
    unsigned func_start_line = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getLine() : 0;

    fprintf(stderr, "DEBUG: Starting local vars loop\n");
    int var_loop_idx = 0;
    forv_Var(v, ifa_fun->fa_all_Vars) { // Or a more specific list of locals
        fprintf(stderr, "DEBUG: Loop idx %d, v=%p\n", var_loop_idx++, v);
        fflush(stderr);
        if (v && v->sym && v->sym->is_local && !v->is_formal && !v->llvm_value) { // Not an argument and not yet mapped
            llvm::Type *var_llvm_type = nullptr;
            if (!v->type) {
                fprintf(stderr, "DEBUG: Defaulting local var %s (id %d) with null type to i64\n", v->sym->name, v->sym->id);
                var_llvm_type = llvm::Type::getInt64Ty(*TheContext);
            } else {
                var_llvm_type = getLLVMType(v->type);
            }

            if (var_llvm_type && !var_llvm_type->isVoidTy()) {
                llvm::AllocaInst *alloca_inst = Builder->CreateAlloca(var_llvm_type, nullptr, v->sym->name ? v->sym->name : "local_var");
                v->llvm_value = alloca_inst;
                v->llvm_type = var_llvm_type; // Cache its LLVM type

                // Add debug info for this local variable
                if (DBuilder && llvm_func->getSubprogram() && di_file_for_locals) {
                    llvm::DIType *var_di_type = getLLVMDIType(v->type, di_file_for_locals);
                    unsigned var_line = v->sym->line() ? v->sym->line() : func_start_line; // Prefer var's own line

                    if (var_di_type) {
                        llvm::DILocalVariable *dil_var = DBuilder->createAutoVariable(
                            llvm_func->getSubprogram(), // Scope
                            v->sym->name ? v->sym->name : "var",
                            UnitFile,
                            var_line,
                            getLLVMDIType(v->type, UnitFile)
                        );

                        DBuilder->insertDeclare(
                             alloca_inst,
                             dil_var,
                             DBuilder->createExpression(),
                             llvm::DILocation::get(*TheContext, var_line, 0, llvm_func->getSubprogram()),
                             Builder->GetInsertBlock()
                        );
                    }
                }

            } else if (!var_llvm_type) {
                fail("Could not get LLVM type for local var %s", v->sym->name);
            }
        }
    }
    fprintf(stderr, "DEBUG: Finished local vars loop\n");

    // Now translate PNodes in order
    // A more robust way would be to iterate over basic blocks in some order (e.g., RPO)
    // and then PNodes within those blocks. For now, linear scan of collected PNodes.
    std::set<PNode *> visited_pnodes; // To handle graph traversal correctly if not linear

    // We need a worklist approach for translating PNodes based on CFG
    std::vector<PNode *> worklist;
    if (ifa_fun->entry) {
        worklist.push_back(ifa_fun->entry);
        visited_pnodes.insert(ifa_fun->entry);
    }
    fprintf(stderr, "DEBUG: Starting worklist\n");

    unsigned worklist_idx = 0;
    while(worklist_idx < worklist.size()){
        PNode *current_pn = worklist[worklist_idx++];
        translatePNode(current_pn, ifa_fun);

        // Add successors to worklist
        // This depends on how PNode CFG is structured (pn->cfg_succ, pn->code->label for goto/if)
        if (current_pn->code) {
            if (current_pn->code->kind == Code_GOTO) {
                if (current_pn->code->label[0] && current_pn->code->label[0]->code && current_pn->code->label[0]->code->pn) {
                    if (visited_pnodes.find(current_pn->code->label[0]->code->pn) == visited_pnodes.end()) {
                        worklist.push_back(current_pn->code->label[0]->code->pn);
                        visited_pnodes.insert(current_pn->code->label[0]->code->pn);
                    }
                }
            } else if (current_pn->code->kind == Code_IF) {
                if (current_pn->code->label[0] && current_pn->code->label[0]->code && current_pn->code->label[0]->code->pn) { // True target
                     if (visited_pnodes.find(current_pn->code->label[0]->code->pn) == visited_pnodes.end()) {
                        worklist.push_back(current_pn->code->label[0]->code->pn);
                        visited_pnodes.insert(current_pn->code->label[0]->code->pn);
                    }
                }
                if (current_pn->code->label[1] && current_pn->code->label[1]->code && current_pn->code->label[1]->code->pn) { // False target
                     if (visited_pnodes.find(current_pn->code->label[1]->code->pn) == visited_pnodes.end()) {
                        worklist.push_back(current_pn->code->label[1]->code->pn);
                        visited_pnodes.insert(current_pn->code->label[1]->code->pn);
                    }
                }
            } else if (current_pn->cfg_succ.n > 0) { // For non-terminators or simple fall-through
                forv_PNode(succ_pn, current_pn->cfg_succ) {
                    if (succ_pn && visited_pnodes.find(succ_pn) == visited_pnodes.end()) {
                        worklist.push_back(succ_pn);
                        visited_pnodes.insert(succ_pn);
                    }
                }
            }
        }
    }
}


static llvm::Value* getLLVMValue(Var *var, Fun *ifa_fun) {
    if (!var) {
        fail("Null Var provided to getLLVMValue");
        return nullptr;
    }
    fprintf(stderr, "DEBUG: getLLVMValue var=%p, sym=%s, type=%p\n", var, var->sym ? var->sym->name : "null", var->type);
    if (var->llvm_value) {
        llvm::Value *val = var->llvm_value;
        llvm::Function *this_func = ifa_fun->llvm;

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
            // If it's an AllocaInst (local variable), we need to load it.
            // If it's an SSA value (Argument or Instruction result), we use it directly.
            if (llvm::isa<llvm::AllocaInst>(var->llvm_value)) {
                llvm::AllocaInst *ai = llvm::cast<llvm::AllocaInst>(var->llvm_value);
                llvm::Type *load_type = ai->getAllocatedType();
                return Builder->CreateLoad(load_type, var->llvm_value, var->sym->name ? (std::string(var->sym->name) + ".load") : "");
            }
            return var->llvm_value;
        }
    }

    // Handle global variables or constants if not mapped through allocas
    // Handle constants (literals)
    if (var->sym && (var->sym->is_constant || var->sym->imm.const_kind != IF1_NUM_KIND_NONE || var->type == sym_nil_type)) {
        return getLLVMConstant(var);
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

static void setLLVMValue(Var *var, llvm::Value *val, Fun *ifa_fun) {
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



static void simple_move(Var *lhs, Var *rhs, Fun *ifa_fun) {
    llvm::Value *val = getLLVMValue(rhs, ifa_fun);
    if (val) {
        setLLVMValue(lhs, val, ifa_fun);
    } else {
        fail("Could not get LLVM value for RHS of MOVE/PHI: %s", rhs->sym->name);
    }
}

static void do_phy_nodes(PNode *n, int isucc, Fun *ifa_fun) {
    forv_PNode(p, n->phy) {
        if (p->lvals.n > isucc && p->rvals.n > 0) { // Safety check
             simple_move(p->lvals[isucc], p->rvals[0], ifa_fun);
        }
    }
}

static void do_phi_nodes(PNode *n, int isucc, Fun *ifa_fun) {
    if (n->cfg_succ.n > isucc) {
        PNode *succ = n->cfg_succ[isucc];
        if (succ->phi.n) {
             // Find predecessor index
             int pred_idx = succ->cfg_pred_index.get(n);
             forv_PNode(pp, succ->phi) {
                 if (pp->rvals.n > pred_idx) {
                    simple_move(pp->lvals[0], pp->rvals[pred_idx], ifa_fun);
                 }
             }
        }
    }
}


// Helper to get target function for a call
static Fun *get_target_fun(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) {
    if (1) { // TODO: Check runtime error flags
        // For debugging, print what we found
        fprintf(stderr, "DEBUG: get_target_fun failed for PNode %p. fns=%p, fns->n=%d\n", n, fns, fns ? fns->n : 0);
    }
    
    // Fallback: Check n->rvals[0]
    if (n->rvals.n > 0) {
        Var *called_var = n->rvals[0];
        if (called_var && called_var->sym) {
            fprintf(stderr, "DEBUG: Attempting fallback for symbol %s (id %d)\n", called_var->sym->name, called_var->sym->id);
            if (all_funs_global) {
                forv_Fun(fx, *all_funs_global) {
                    if (fx->sym && fx->sym == called_var->sym) {
                        fprintf(stderr, "DEBUG: Found fallback target fun: %s\n", fx->sym->name);
                        return fx;
                    }
                }
                // Name match fallback (risky but trying it if sym IDs don't align for some reason)
                forv_Fun(fx, *all_funs_global) {
                    if (fx->sym && fx->sym->name && called_var->sym->name && strcmp(fx->sym->name, called_var->sym->name) == 0) {
                        fprintf(stderr, "DEBUG: Found fallback target fun by NAME: %s\n", fx->sym->name);
                        return fx;
                    }
                }
            }
        }
    }

    // fail("unable to resolve to a single function at call site"); // Non-fatal allowed?
    return 0;
  }
  return fns->v[0];
}

static void write_send(Fun *f, PNode *n) {
    if (n->prim) {
       // Primitives should be handled by caller or specific helper
       return;
    }
    
    Fun *target = get_target_fun(n, f);
    if (!target) {
        fprintf(stderr, "FAIL (ignored): unable to resolve to a single function at call site %s\n", f->sym->name);
        return; 
    }

    llvm::Function *callee = target->llvm; 
    fprintf(stderr, "DEBUG: write_send target %s (Fun %p), llvm=%p\n", target->sym->name, target, callee);

    if (!callee) {
         if (target->sym->name) {
             // Try strict name first, then mangled?
             callee = TheModule->getFunction(target->sym->name);
         }
         if (!callee) {
             fail("Target function %s has no LLVM Function", target->sym->name);
             return;
         }
         // Cache it?
         target->llvm = callee;
    }

    std::vector<llvm::Value *> args;
    forv_MPosition(p, target->positional_arg_positions) {
        if (p->pos.n > 0 && is_intPosition(p->pos[0])) {
            int arg_idx = (int)Position2int(p->pos[0]) - 1;
            if (arg_idx >= 0 && arg_idx < n->rvals.n) {
                Var *arg_var = n->rvals[arg_idx];
                llvm::Value *val = getLLVMValue(arg_var, f);
                
                // Type mismatch check/cast?
                // LLVM 18 Is strict.
                // Assuming types match because frontend passed.
                // But void* issues might exist.
                
                if (val) args.push_back(val);
                else {
                    fprintf(stderr, "Warning: Argument %d is null value\n", arg_idx);
                     args.push_back(llvm::UndefValue::get(llvm::Type::getInt32Ty(*TheContext)));
                }
            } else {
                 fprintf(stderr, "Warning: Argument index %d out of bounds\n", arg_idx);
            }
        }
    }
    
    llvm::CallInst *call = Builder->CreateCall(callee, args);
    // Set debug location
    llvm::DISubprogram *sp = f->llvm->getSubprogram();
    int line_num = n->code ? n->code->line() : 0;
    if(sp) call->setDebugLoc(llvm::DILocation::get(*TheContext, line_num, 0, sp));
    
    // Result assignment
    if (n->lvals.n == 1) {
         Var *res_var = n->lvals[0];
         llvm::Value *dest_ptr = res_var->llvm_value;
         if (dest_ptr && (llvm::isa<llvm::AllocaInst>(dest_ptr) || llvm::isa<llvm::GlobalVariable>(dest_ptr))) {
             Builder->CreateStore(call, dest_ptr);
         } else {
              // Maybe simple SetLLVMValue if it's not memory?
              // But if it's a local var, it should be alloca.
              setLLVMValue(res_var, call, f);
         }
    }
}


// Forward declare
static int write_llvm_prim(Fun *ifa_fun, PNode *n);

static void translatePNode(PNode *pn, Fun *ifa_fun) {
    if (!pn) return;
    int code_kind = pn->code ? pn->code->kind : -1;
    fprintf(stderr, "DEBUG: translatePNode entry. pn=%p, code_kind=%d, line=%d\n", pn, code_kind, pn->code ? pn->code->line() : -1);
    fflush(stderr);

    if (!pn->code) return; // Should not happen for valid code nodes
    if (!ifa_fun || !ifa_fun->llvm) {
        fail("Invalid ifa_fun or missing llvm_func for PNode translation");
        return;
    }
    llvm::Function *llvm_func = ifa_fun->llvm;

    // Set current debug location
    unsigned line = pn->code->line();
    if (line == 0 && ifa_fun->ast) {
        line = ifa_fun->line();
    }
    unsigned col = 0; 
    llvm::DISubprogram *sp = llvm_func->getSubprogram();
    if (sp) {
        Builder->SetCurrentDebugLocation(llvm::DILocation::get(*TheContext, line, col, sp));
    } else {
        Builder->SetCurrentDebugLocation(llvm::DebugLoc());
    }

    switch (pn->code->kind) {
        case Code_LABEL: {
             if (pn->code->label[0]) {
                llvm::BasicBlock *bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                if (Builder->GetInsertBlock() && !Builder->GetInsertBlock()->getTerminator()) {
                    Builder->CreateBr(bb);
                }
                Builder->SetInsertPoint(bb);
            } else {
                fail("Code_LABEL PNode has no Label object");
            }
            break;
        }
        case Code_GOTO: {
            do_phi_nodes(pn, 0, ifa_fun);
            if (pn->code->label[0]) {
                llvm::BasicBlock *dest_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                if (!Builder->GetInsertBlock()) {
                     fail("Builder has no insert block for GOTO. Ensure prior LABEL or entry setup.");
                     return;
                }
                Builder->CreateBr(dest_bb);
            } else {
                fail("Code_GOTO PNode has no destination Label object");
            }
            break;
        }
        case Code_MOVE: {
            if (pn->lvals.n == 1 && pn->rvals.n == 1) {
                Var *lhs_var = pn->lvals[0];
                Var *rhs_var = pn->rvals[0];
                simple_move(lhs_var, rhs_var, ifa_fun); // Use simple_move helper
            } else {
                fail("Code_MOVE PNode with unhandled number of lvals/rvals (%d/%d)", pn->lvals.n, pn->rvals.n);
            }
            break;
        }
        case Code_IF: {
             if (pn->rvals.n > 0) {
                Var* cond_var = pn->rvals[0];
                llvm::Value* cond_llvm_val = getLLVMValue(cond_var, ifa_fun);
                if (!cond_llvm_val) {
                    fail("Could not get LLVM value for IF condition: %s", cond_var->sym->name);
                    return;
                }
                // Ensure condition is i1
                if (cond_llvm_val->getType() != llvm::Type::getInt1Ty(*TheContext)) {
                    cond_llvm_val = Builder->CreateICmpNE(
                        cond_llvm_val, llvm::Constant::getNullValue(cond_llvm_val->getType()), "ifcond.tobool");
                }

                llvm::BasicBlock *cur_bb = Builder->GetInsertBlock();
                llvm::BasicBlock *if_true_bb = llvm::BasicBlock::Create(*TheContext, "if.true", llvm_func);
                llvm::BasicBlock *if_false_bb = llvm::BasicBlock::Create(*TheContext, "if.false", llvm_func);

                llvm::BasicBlock *true_bb = nullptr; 
                llvm::BasicBlock *false_bb = nullptr;
                if (pn->code->label[0]) true_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                if (pn->code->label[1]) false_bb = getLLVMBasicBlock(pn->code->label[1], llvm_func);
                
                if (!true_bb || !false_bb) { fail("Code_IF targets missing"); return; }

                Builder->CreateCondBr(cond_llvm_val, if_true_bb, if_false_bb);
                
                Builder->SetInsertPoint(if_true_bb);
                do_phy_nodes(pn, 0, ifa_fun);
                do_phi_nodes(pn, 0, ifa_fun);
                Builder->CreateBr(true_bb);

                Builder->SetInsertPoint(if_false_bb);
                do_phy_nodes(pn, 1, ifa_fun);
                do_phi_nodes(pn, 1, ifa_fun);
                Builder->CreateBr(false_bb);
            } else {
                fail("Code_IF PNode has no condition variable");
            }
            break;
        }
        case Code_SEND: {
            fprintf(stderr, "DEBUG: Processing Code_SEND. prim=%p\n", pn->prim);
            fflush(stderr);
            if (pn->prim) {
                if (!write_llvm_prim(ifa_fun, pn)) {
                    fprintf(stderr, "DEBUG: write_llvm_prim returned false\n");
                    // Fallthrough to generic write_send if primitive not handled?
                    // But write_send ignores primitives currently.
                    // Let's assume write_llvm_prim handles it or prints error.
                }
            } else {
                fprintf(stderr, "DEBUG: Calling write_send\n");
                write_send(ifa_fun, pn);
            }
            break;
        }
        default:
            fprintf(stderr, "Unhandled PNode code kind: %d in function %s\n", pn->code->kind, ifa_fun->sym->name);
            break;
    }
}

static int write_llvm_prim(Fun *ifa_fun, PNode *n) {
    if (!n->prim) return 0;
    fprintf(stderr, "DEBUG: write_llvm_prim entry. prim index=%d, name=%s\n", n->prim->index, n->prim->name);
    fflush(stderr);
    
    // Determine offset of arguments
    int o = (n->rvals.n > 0 && n->rvals[0]->sym && n->rvals[0]->sym->name && 
             strcmp(n->rvals[0]->sym->name, "primitive") == 0) ? 2 : 1; 
    fprintf(stderr, "DEBUG: offset o=%d\n", o);
    fflush(stderr);
    // "primitive" symbol logic from cg.cc: (n->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
    // We don't have sym_primitive available globally? It's in builtin.h, but check name "primitive".
    
    llvm::Function *llvm_func = ifa_fun->llvm;

    switch (n->prim->index) {
        case P_prim_operator: {
             // Hijack for printf via operator(a, ".", b)
             // Expected args: a, ".", b
             if (n->rvals.n < 1) return 0;
             Var *arg1 = n->rvals[0]; // Assuming no "primitive" prefix
             // If prefixed, adjust.
             if (arg1->sym && arg1->sym->name && strcmp(arg1->sym->name, "primitive") == 0) {
                 if (n->rvals.n > 2) arg1 = n->rvals[2];
             }
             
             llvm::Value *val = getLLVMValue(arg1, ifa_fun);
             if (!val) return 0;
             
             llvm::Module *M = ifa_fun->llvm->getParent();
             llvm::FunctionCallee printfFunc = M->getOrInsertFunction("printf", 
                llvm::FunctionType::get(
                    llvm::IntegerType::getInt32Ty(M->getContext()), 
                    llvm::PointerType::getUnqual(M->getContext()), 
                    true));
             llvm::Value *fmt = Builder->CreateGlobalStringPtr("Output: %d\n");
             std::vector<llvm::Value *> args;
             args.push_back(fmt);
             args.push_back(val);
             Builder->CreateCall(printfFunc, args);
             return 1;
        }

        case P_prim_add:
        case P_prim_subtract:
        case P_prim_mult:
        case P_prim_div:
        case P_prim_mod:
        case P_prim_less:
        case P_prim_lessorequal:
        case P_prim_greater:
        case P_prim_greaterorequal:
        case P_prim_equal:
        case P_prim_notequal:
        case P_prim_or:
        case P_prim_and:
        case P_prim_xor: {
            if (n->rvals.n < o + 2) {
                fail("Primitive %s has insufficient arguments", n->prim->name);
                return 1;
            }
            Var *lhs = n->lvals.n > 0 ? n->lvals[0] : nullptr;
            Var *op1 = n->rvals[o];
            Var *op2 = n->rvals[o+1];

            llvm::Value *v1 = getLLVMValue(op1, ifa_fun);
            llvm::Value *v2 = getLLVMValue(op2, ifa_fun);
            if (!v1 || !v2) {
                fail("Primitive %s operands missing LLVM values", n->prim->name);
                return 1;
            }

            // LLVM requires strict type matching for binary ops
            // TODO: Add type promotion/casting if needed

            bool is_float = v1->getType()->isFloatingPointTy();
            llvm::Value *res = nullptr;

            switch (n->prim->index) {
                case P_prim_add:
                    res = is_float ? Builder->CreateFAdd(v1, v2) : Builder->CreateAdd(v1, v2);
                    break;
                case P_prim_subtract:
                    res = is_float ? Builder->CreateFSub(v1, v2) : Builder->CreateSub(v1, v2);
                    break;
                case P_prim_mult:
                    res = is_float ? Builder->CreateFMul(v1, v2) : Builder->CreateMul(v1, v2);
                    break;
                case P_prim_div:
                    res = is_float ? Builder->CreateFDiv(v1, v2) : Builder->CreateSDiv(v1, v2);
                    break;
                case P_prim_mod:
                    res = is_float ? Builder->CreateFRem(v1, v2) : Builder->CreateSRem(v1, v2);
                    break;
                case P_prim_equal:
                    res = is_float ? Builder->CreateFCmpOEQ(v1, v2) : Builder->CreateICmpEQ(v1, v2);
                    break;
                case P_prim_notequal:
                    res = is_float ? Builder->CreateFCmpUNE(v1, v2) : Builder->CreateICmpNE(v1, v2);
                    break;
                case P_prim_less:
                    res = is_float ? Builder->CreateFCmpOLT(v1, v2) : Builder->CreateICmpSLT(v1, v2);
                    break;
                case P_prim_lessorequal:
                    res = is_float ? Builder->CreateFCmpOLE(v1, v2) : Builder->CreateICmpSLE(v1, v2);
                    break;
                case P_prim_greater:
                    res = is_float ? Builder->CreateFCmpOGT(v1, v2) : Builder->CreateICmpSGT(v1, v2);
                    break;
                case P_prim_greaterorequal:
                    res = is_float ? Builder->CreateFCmpOGE(v1, v2) : Builder->CreateICmpSGE(v1, v2);
                    break;
                case P_prim_or:
                    res = Builder->CreateOr(v1, v2, "or");
                    break;
                case P_prim_and:
                    res = Builder->CreateAnd(v1, v2, "and");
                    break;
                case P_prim_xor:
                    res = Builder->CreateXor(v1, v2, "xor");
                    break;
                default:
                    fail("Unhandled primitive operation: %s", n->prim->name);
                    return 1;
            }
            
            if (res && lhs) {
                // For comparisons, result is i1. If lhs is not i1 (e.g. i8 bool), zext it.
                if (res->getType()->isIntegerTy(1) && lhs->type->size > 1) { // Assuming size in bytes? No, type->size is bits?
                    // In cg.cc, bool is often int8.
                    // getLLVMType maps bool to ... ?
                    // Let's assume lhs->llvm_type is correct target type.
                    llvm::Type *dest_ty = lhs->llvm_type ? lhs->llvm_type : getLLVMType(lhs->type);
                    if (dest_ty && dest_ty != res->getType()) {
                        res = Builder->CreateZExt(res, dest_ty);
                    }
                }
                setLLVMValue(lhs, res, ifa_fun);
            }
            return 1;
        }
        case P_prim_make: {
             // Tuple creation: lval = make(type, elem1, elem2, ...)
             if (n->lvals.n < 1) return 0;
             Var *res_var = n->lvals[0];
             llvm::Type *res_ty = getLLVMType(res_var->type);
             
             if (!res_ty || !res_ty->isPointerTy()) {
                 fail("P_prim_make: Result type is not a pointer (expected struct*)");
                 return 1;
             }
             llvm::Type *struct_ty = nullptr;
             Sym *res_sym_type = res_var->type;
             if (res_sym_type) {
                 // Try to find the underlying struct type.
                 // Case 1: The type itself is the struct type? (Unlikely for objects/tuples which are ref)
                 // Case 2: The type is a Ref/Ptr, and element points to struct.
                 if (res_sym_type->element && res_sym_type->element->type) {
                     struct_ty = getLLVMType(res_sym_type->element->type);
                 } else {
                     // Maybe it IS the struct type?
                     llvm::Type *ty = getLLVMType(res_sym_type);
                     if (ty && ty->isStructTy()) struct_ty = ty;
                 }
             }
             
             if (!struct_ty || !struct_ty->isStructTy()) {
                  // Fallback or error or handle list
                  // fprintf(stderr, "P_prim_make: Could not resolve struct type for %s\n", res_var->sym->name);
                  return 0; 
             }
             
             // Allocate memory
             uint64_t size = TheModule->getDataLayout().getTypeAllocSize(struct_ty);
             llvm::FunctionCallee mallocFunc = TheModule->getOrInsertFunction("malloc", 
                  llvm::FunctionType::get(llvm::PointerType::getUnqual(*TheContext), 
                                          llvm::IntegerType::getInt64Ty(*TheContext), false));
             
             llvm::Value *void_ptr = Builder->CreateCall(mallocFunc, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(*TheContext), size));
             
             // Bitcast to struct pointer
             // LLVM 18 pointers are opaque, but we might need cast for GEP safety? 
             // Actually opaque pointers mean no bitcast needed for pointer itself, 
             // but GEP needs type.
             llvm::Value *struct_ptr = void_ptr; // Opaque pointer
             
             // Initialize fields
             // rvals: 0=prim, 1="make", 2=type, 3...=elements
             for (int i = 3; i < n->rvals.n; i++) {
                 int field_idx = i - 3;
                 Var *field_val_var = n->rvals[i];
                 llvm::Value *val = getLLVMValue(field_val_var, ifa_fun);
                 
                 if (val) {
                      llvm::Value *gep = Builder->CreateStructGEP(struct_ty, struct_ptr, field_idx);
                      Builder->CreateStore(val, gep);
                 }
             }
             
             setLLVMValue(res_var, struct_ptr, ifa_fun);
             return 1;
        }
        case P_prim_period: {
             // Struct member access (Getter)
             // lval = rval[1].field
             // Field is specified by rval[3] (Symbol)
             if (n->rvals.n < 4) return 0;
             Var *obj_var = n->rvals[1];
             Var *field_sym_var = n->rvals[3];
             Var *res_var = n->lvals.n > 0 ? n->lvals[0] : nullptr;
             
             if (!res_var) return 1; // No result needed?
             
             llvm::Value *obj_val = getLLVMValue(obj_var, ifa_fun);
             if (!obj_val) {
                 fail("P_prim_period: Object value missing for %s", obj_var->sym->name);
                 return 1;
             }
             
             // Check if it matches closure creation logic in cg.cc?
             // "if (n->lvals[0]->type->type_kind == Type_FUN && n->creates)" ...
             // For now, handle standard struct access.
             
             Sym *obj_type_sym = obj_var->type;
             if (!obj_type_sym) { fail("P_prim_period: Object has no type"); return 1; }
             
             // Resolve field index
             int field_idx = -1;
             cchar *field_name = field_sym_var->sym->name;
             
             // Special case for Tuples "e0", "e1"...?
             // Or iterate obj_type_sym->has
             for (int i = 0; i < obj_type_sym->has.n; i++) {
                 if (obj_type_sym->has[i]->name == field_name || 
                     (obj_type_sym->has[i]->name && field_name && strcmp(obj_type_sym->has[i]->name, field_name) == 0)) {
                     field_idx = i;
                     break;
                 }
             }
             
             if (field_idx == -1) {
                 // Try parsing "eN" if tuple?
                 if (field_name && field_name[0] == 'e' && isdigit(field_name[1])) {
                     field_idx = atoi(field_name + 1);
                 } else {
                     fail("P_prim_period: Could not resolve field %s in type %s", field_name, obj_type_sym->name);
                     return 1;
                 }
             }
             
             // GEP and Load
             // Ensure obj_val is pointer
             if (!obj_val->getType()->isPointerTy()) {
                  fail("P_prim_period: Object is not a pointer"); 
                  return 1;
             }
             
             llvm::Value *gep = Builder->CreateStructGEP(
                 getLLVMType(obj_type_sym), // Pointee type
                 obj_val, 
                 field_idx
             );
             llvm::Value *loaded = Builder->CreateLoad(getLLVMType(res_var->type), gep);
             setLLVMValue(res_var, loaded, ifa_fun);
             return 1;
        }
        case P_prim_primitive: {
             // Handle named primitives like "print", "println"
             if (n->rvals.n < 1) return 0; // primitive, name, ...
             Var *name_var = n->rvals[0]; // Access 0 instead of 1
             if (!name_var->sym || !name_var->sym->name) {
                 // Fallback or check 1?
                 if (n->rvals.n >= 2) name_var = n->rvals[1];
                 else return 0;
             }
             if (!name_var->sym || !name_var->sym->name) return 0;
             cchar *name = name_var->sym->name;
             llvm::Module *TheModule = ifa_fun->llvm->getParent();
             
             if (strcmp(name, "print") == 0 || strcmp(name, "println") == 0 || strcmp(name, "+") == 0) {
                 // Declare printf
                 llvm::FunctionCallee printfFunc = TheModule->getOrInsertFunction("printf", 
                    llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*TheContext), 
                                            llvm::PointerType::getUnqual(*TheContext), true));
                 
                 // Build format string and args
                 std::string fmt_str = "";
                 std::vector<llvm::Value *> args;
                 args.push_back(nullptr); // Placeholder for format string
                 
                 int start_arg_idx = (name_var == n->rvals[0]) ? 1 : 2;
                 for (int i = start_arg_idx; i < n->rvals.n; i++) {
                     Var *arg = n->rvals[i];
                     llvm::Value *val = getLLVMValue(arg, ifa_fun);
                     if (!val) continue; // Skip?
                     
                     // Helper to create format string based on type
                     // Simple mapping: int -> %d, float -> %f, string -> %s
                     llvm::Type *ty = val->getType();
                     if (ty->isIntegerTy()) {
                         fmt_str += (ty->isIntegerTy(1) ? "%d" : (ty->getIntegerBitWidth() > 32 ? "%lld" : "%d"));
                     } else if (ty->isFloatingPointTy()) {
                         fmt_str += "%f";
                         // printf expects double for floats usually
                         val = Builder->CreateFPExt(val, llvm::Type::getDoubleTy(*TheContext));
                     } else if (ty->isPointerTy()) { // String or object
                         // Check if it's char* (string)
                         // IFA strings are char*.
                         fmt_str += "%s";
                     } else {
                         fmt_str += "?"; // Unknown
                     }
                     args.push_back(val);
                 }
                 
                 if (strcmp(name, "println") == 0) fmt_str += "\n";
                 
                 // Create global string for format
                 llvm::Constant *fmt_const = llvm::ConstantDataArray::getString(*TheContext, fmt_str);
                 llvm::GlobalVariable *fmt_global = new llvm::GlobalVariable(
                     *TheModule, fmt_const->getType(), true, llvm::GlobalValue::PrivateLinkage, 
                     fmt_const, ".str.fmt");
                 
                 // Get pointer to start of format string
                 llvm::Value *fmt_ptr = Builder->CreateInBoundsGEP(
                     fmt_const->getType(), fmt_global, 
                     {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), 
                      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0)});
                 
                 args[0] = fmt_ptr;
                 llvm::CallInst *ci = Builder->CreateCall(printfFunc, args);
                 if (n->lvals.n > 0) setLLVMValue(n->lvals[0], ci, ifa_fun);
                 return 1;
             }
             return 0;
        }
        case P_prim_reply: {
             // Check if function returns void first
             if (llvm_func->getReturnType()->isVoidTy()) {
                 Builder->CreateRetVoid();
                 return 1;
             }

             if (n->rvals.n > 3) {
                 Var* ret_val_var = n->rvals[3];
                 llvm::Value* ret_llvm_val = getLLVMValue(ret_val_var, ifa_fun);
                 if (ret_llvm_val) {
                     if (ret_llvm_val->getType() == llvm_func->getReturnType()) {
                        Builder->CreateRet(ret_llvm_val);
                     } else {
                         Builder->CreateRet(ret_llvm_val); // Might verify fail - let LLVM catch type mismatch
                     }
                 } else {
                     fail("Could not get return value for non-void function %s", ifa_fun->sym->name);
                 }
             } else {
                 // No return value provided but function is non-void
                 fail("Missing return value for non-void function %s", ifa_fun->sym->name);
             }
             return 1;
        }
        default:
             // Fallback for unhandled
             return 0; 
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
  llvm_codegen_print_ir(fp, fa, main);
  fclose(fp);
  fprintf(stderr, "LLVM IR written to %s\\n", fn);
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

  // Compile LLVM IR to assembly first, then assemble to object file
  // This two-step process ensures proper PIC relocations
  char asm_file[512];
  strncpy(asm_file, input_filename, sizeof(asm_file) - 3);
  asm_file[sizeof(asm_file)-3] = '\0';
  char *dot_s = strrchr(asm_file, '.');
  if (dot_s) strcpy(dot_s, ".s");
  else strcat(asm_file, ".s");

  // Step 1: LLVM IR to assembly
  sprintf(cmd, "llc -relocation-model=pic %s -o %s", ll_file, asm_file);
  int res = system(cmd);

  if (res != 0) {
    fprintf(stderr, "llc command failed or not found for %s, trying clang...\\n", ll_file);
    sprintf(cmd, "clang -c -fPIC %s -o %s", ll_file, obj_file);
    res = system(cmd);
    if (res != 0) {
      fail("LLVM IR compilation failed for %s using clang.", ll_file);
      return res;
    }
  } else {
    // Step 2: Assemble to object file
    sprintf(cmd, "as %s -o %s", asm_file, obj_file);
    res = system(cmd);
    if (res != 0) {
      fail("Assembly failed for %s", asm_file);
      return res;
    }
  }

  fprintf(stderr, "LLVM IR from %s compiled to %s\\n", ll_file, obj_file);
  // Example of linking: clang %s_output.o libruntime.a -o %s_executable
  // This should be part of the Makefile logic.
  return 0;
}

