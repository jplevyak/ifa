/* -*-Mode: c++;-*-
   Copyright (c) 2023 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "llvm.h"
#include "fun.h" // For Fun
#include "fa.h"  // For FA
#include "fail.h" // For fail
#include "pattern.h" // For MPosition

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

// Main type mapping function
static llvm::Type *getLLVMType(Sym *sym) {
  if (!sym) {
    fail("Null Sym provided to getLLVMType");
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

        // TODO: Handle varargs if unaliased_sym->is_varargs or similar flag exists
        bool is_var_arg = false; // Placeholder
        if (unaliased_sym->fun && unaliased_sym->fun->is_varargs) {
            is_var_arg = true;
        }
        type = llvm::FunctionType::get(return_llvm_type, arg_llvm_types, is_var_arg);
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
      case Type_UNKNOWN:
      case Type_PRIMITIVE: // Should be handled by num_kind or specific syms like sym_string
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
    if (!DBuilder) return nullptr;
    // Minimal implementation: Assume void() or similar for now
    // TODO: Build actual elements array with return type and arg types
    llvm::SmallVector<llvm::Metadata *, 8> EltTys;
    // Return type first (null for void)
    EltTys.push_back(nullptr); 
    
    return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
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
        if (sym->is_local || var->is_formal || var->type->type_kind == Type_FUN) {
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


void llvm_codegen_print_ir(FILE *fp, FA *fa, Fun *main_fun) {
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
  UnitFile = DBuilder->createFile(fname, dir);
  CU = DBuilder->createCompileUnit(
      llvm::dwarf::DW_LANG_C, UnitFile, "ifa-compiler", 0 /*isOptimized*/, "" /*flags*/, 0 /*RV*/);

  // Create Global Variables
  createGlobalVariables(fa);

  // Iterate over all functions in FA and create them in the LLVM Module
  // Ensure main_fun is processed, and other functions it might call.
  // The order might matter if there are dependencies not captured by FA's list directly.
  // For now, iterate fa->funs then ensure main_fun is included if not already.

  Vec<Fun *> all_funs;
  if (fa->funs.n > 0) {
      forv_Fun(f, fa->funs) {
          if (f && f->live && f->entry) { // Process live functions with an entry point
              all_funs.set_add(f);
          }
      }
  }
  if (main_fun && main_fun->live && main_fun->entry) {
      all_funs.set_add(main_fun);
  }


  forv_Fun(f, all_funs) {
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
  DBuilder->finalize();

  // Verify the module
  std::string error_str;
  llvm::raw_string_ostream rso(error_str);
  if (llvm::verifyModule(*TheModule, &rso)) {
    fail("LLVM module verification failed: %s", rso.str().c_str());
    if (fp != stderr) { // Print to file as well if it's not stderr
        fprintf(fp, "; LLVM module verification failed: %s\\n", rso.str().c_str());
    }
    return;
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
  if (ifa_fun->llvm) { // Already created
    return ifa_fun->llvm;
  }
  if (!ifa_fun->sym) {
    fail("Fun %p has no symbol associated.", (void*)ifa_fun);
    return nullptr;
  }

  // 1. Determine FunctionType
  llvm::Type *llvm_ret_type;
  if (ifa_fun->rets.n == 1 && ifa_fun->rets[0] && ifa_fun->rets[0]->type) {
    llvm_ret_type = getLLVMType(ifa_fun->rets[0]->type);
  } else if (ifa_fun->rets.n == 0) { // No explicit return might mean void
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  } else {
    // TODO: Handle multiple return values if the language supports them
    // (e.g., by returning a struct). For now, assume single or void.
    fail("Function %s has %d return values, unsupported. Assumed void or single.", ifa_fun->sym->name, ifa_fun->rets.n);
    llvm_ret_type = llvm::Type::getVoidTy(*TheContext);
  }
  if (!llvm_ret_type) {
      fail("Could not determine LLVM return type for function %s", ifa_fun->sym->name);
      return nullptr;
  }

  std::vector<llvm::Type *> llvm_arg_types;
  // Iterate based on ifa_fun->positional_arg_positions and ifa_fun->args
  // This ensures correct order and live arguments are considered.
  forv_MPosition(p, ifa_fun->positional_arg_positions) {
    Var *arg_var = ifa_fun->args.get(p);
    if (arg_var && arg_var->live && arg_var->type) {
      llvm::Type *arg_llvm_type = getLLVMType(arg_var->type);
      if (arg_llvm_type) {
        llvm_arg_types.push_back(arg_llvm_type);
      } else {
        fail("Could not get LLVM type for argument %s of function %s",
             arg_var->sym->name ? arg_var->sym->name : "unnamed_arg", ifa_fun->sym->name);
        return nullptr;
      }
    } else if (arg_var && !arg_var->live) {
      // Skip non-live arguments
    } else {
      fail("Invalid argument Var or missing type at position for function %s", ifa_fun->sym->name);
      return nullptr;
    }
  }

  llvm::FunctionType *func_type = llvm::FunctionType::get(llvm_ret_type, llvm_arg_types, ifa_fun->is_varargs);

  // 2. Create llvm::Function
  // Use ifa_fun->cg_string if available and valid for LLVM, otherwise generate from sym->name
  // cg_string might contain C-specific naming like "_CG_f_..."
  // LLVM functions typically don't need such prefixes unless for specific ABI/mangling.
  std::string func_name = ifa_fun->sym->name ? ifa_fun->sym->name : ("func" + std::to_string(ifa_fun->id));
  if (ifa_fun->cg_string && strncmp(ifa_fun->cg_string, "_CG_", 4) == 0) { // Basic check if it's a cg.cc name
      func_name = ifa_fun->cg_string; // Use it if it looks like a C-mangled name
  }


  llvm::Function::LinkageTypes linkage = llvm::Function::ExternalLinkage;
  if (ifa_fun->is_external) { // External means defined elsewhere
      linkage = llvm::Function::ExternalLinkage;
  } else if (ifa_fun->sym->is_builtin) { // Builtins might be external or defined by runtime
      // This needs a clear strategy. For now, assume external if no body.
      if (!ifa_fun->entry) linkage = llvm::Function::ExternalLinkage;
      else linkage = llvm::Function::InternalLinkage; // If has body, make internal unless specified
  } else {
      // TODO: Determine linkage based on IF1 symbol properties (e.g. static, public)
      // For now, default to ExternalLinkage for non-external, non-builtin functions that might be main
      // and InternalLinkage for others to allow optimization.
      // A simple heuristic: if it's the main_fun, it's external.
      // bool is_if1_main = (ifa_fun == main_fun); // This check needs main_fun to be passed or accessible.
      // linkage = is_if1_main ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;
      // Let's make non-external functions internal by default for now.
      linkage = llvm::Function::InternalLinkage;
  }


  llvm::Function *llvm_func = llvm::Function::Create(func_type, linkage, func_name, module);
  ifa_fun->llvm = llvm_func; // Store it

  // 3. Set argument names and store llvm::Argument in Var::llvm_value
  unsigned arg_idx = 0;
  for (auto &llvm_arg : llvm_func->args()) {
    if (arg_idx < ifa_fun->positional_arg_positions.n) {
      MPosition *pos = ifa_fun->positional_arg_positions[arg_idx];
      Var *arg_var = ifa_fun->args.get(pos);
      if (arg_var && arg_var->live) {
        if (arg_var->sym && arg_var->sym->name) {
          llvm_arg.setName(arg_var->sym->name);
        }
        arg_var->llvm_value = &llvm_arg;
        arg_idx++; // Increment only when a live IF1 arg is matched.
                   // This assumes llvm_arg_types was built only from live IF1 args.
      } else if (arg_var && !arg_var->live) {
        // This case should ideally not happen if llvm_arg_types was built correctly
        // from live args only. If it does, we have a mismatch.
        // For now, we just skip to find the next live arg for the current llvm_arg.
        // This part of the logic needs to be robust.
        // A better way: iterate positional_arg_positions again to map.
        continue;
      }
    }
  }
  // Corrected argument mapping:
  arg_idx = 0;
  forv_MPosition(p, ifa_fun->positional_arg_positions) {
    Var *arg_var = ifa_fun->args.get(p);
    if (arg_var && arg_var->live) {
        if (arg_idx < llvm_func->arg_size()) {
            llvm::Argument *llvm_argument = llvm_func->getArg(arg_idx);
            if (arg_var->sym && arg_var->sym->name) {
                llvm_argument->setName(arg_var->sym->name);
            }
            arg_var->llvm_value = llvm_argument;
            arg_var->llvm_type = llvm_argument->getType(); // Cache argument type
        } else {
            fail("Mismatch between number of live IF1 args and LLVM function args for %s", func_name.c_str());
            break;
        }
        arg_idx++;
    }
  }


  // 4. Create Debug Info for the function (DISubprogram)
  if (DBuilder && CU) { // Ensure DIBuilder and Compile Unit exist
    llvm::DIFile* di_file = UnitFile; // Use the CU's file for now
    unsigned line_num = ifa_fun->line(); // Get line number from Fun

    // Create DIType for function
    llvm::DISubroutineType *di_func_type = createFunctionDIType(ifa_fun, di_file);

    llvm::DISubprogram *sp = DBuilder->createFunction(
        CU,                                // Scope (Compile Unit)
        func_name,                         // Name
        llvm_func->getName(),              // Linkage Name (mangled name)
        di_file,                           // File
        line_num,                          // Line Number
        di_func_type,                      // DI Function Type
        line_num,                          // Scope Line (where definition starts)
        llvm::DINode::FlagPrototyped | llvm::DINode::FlagStaticMember, // Example flags
        llvm::DISubprogram::SPFlagDefinition // SPFlags
    );
    llvm_func->setSubprogram(sp);

    // Create DILocalVariable for arguments and insert dbg.declare
    // This should be done after the entry block is created and builder is set.
    llvm::BasicBlock *entry_bb_for_arg_dbg = nullptr;
    if (!ifa_fun->is_external && ifa_fun->entry) {
        // If there's a body, use its entry block.
        // The actual entry block might be different from llvm_func->getEntryBlock() if IF1 entry is a label.
        // For now, assume llvm_func->getEntryBlock() is where allocas/dbg.declares go.
         entry_bb_for_arg_dbg = &llvm_func->getEntryBlock();
         if (entry_bb_for_arg_dbg->empty()) { // If it's empty, set insert point. Otherwise, insert at beginning.
            Builder->SetInsertPoint(entry_bb_for_arg_dbg);
         } else {
            Builder->SetInsertPoint(entry_bb_for_arg_dbg, entry_bb_for_arg_dbg->getFirstInsertionPt());
         }
    }


    unsigned current_arg_idx = 0;
    forv_MPosition(p, ifa_fun->positional_arg_positions) {
        Var *arg_var = ifa_fun->args.get(p);
        if (arg_var && arg_var->live && arg_var->llvm_value) {
            llvm::DIType *arg_di_type = getLLVMDIType(arg_var->type, di_file);
            if (arg_di_type && entry_bb_for_arg_dbg) { // Only if we have a block to insert into
            llvm::Argument *llvm_argument = static_cast<llvm::Argument*>(arg_var->llvm_value);
            llvm::DILocalVariable *dil_arg = DBuilder->createParameterVariable(
                sp, arg_var->sym->name ? arg_var->sym->name : "arg",
                current_arg_idx + 1, // ArgNo
                UnitFile,
                ifa_fun->line(),
                getLLVMDIType(arg_var->type, UnitFile),
                true // Always preserve
            );
            
            DBuilder->insertDeclare(
                llvm_argument,
                dil_arg,
                DBuilder->createExpression(),
                llvm::DILocation::get(*TheContext, ifa_fun->line(), 0, sp), // Location of the declare
                Builder->GetInsertBlock()
            );
            current_arg_idx++;
        }
    }
  }


  // 5. Create entry basic block (if function has a body)
  if (!ifa_fun->is_external && ifa_fun->entry) { // ifa_fun->entry means it has a body
    llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_func);
    // PNode translation will populate this block later.
    // For now, if it's a non-void function, add a dummy return.
    // This will be removed when actual PNode translation happens.
    if (llvm_ret_type != llvm::Type::getVoidTy(*TheContext)) {
        // Builder->SetInsertPoint(entry_bb); // Temporarily set builder
        // if (llvm_ret_type->isIntegerTy())
        //     Builder->CreateRet(llvm::ConstantInt::get(llvm_ret_type, 0));
        // else if (llvm_ret_type->isFloatingPointTy())
        //     Builder->CreateRet(llvm::ConstantFP::get(llvm_ret_type, 0.0));
        // else if (llvm_ret_type->isPointerTy())
        //     Builder->CreateRet(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(llvm_ret_type)));
        // else {
        //      Builder->CreateRet(llvm::UndefValue::get(llvm_ret_type));
        // }
    } else {
        // Builder->SetInsertPoint(entry_bb);
        // Builder->CreateRetVoid();
    }
    // Actual PNode translation will handle returns and block population.
  }


  // After creating the function shell, if it's not external, translate its PNodes.
  if (!ifa_fun->is_external && ifa_fun->entry) {
    translateFunctionBody(ifa_fun);
  }

  return llvm_func;
}
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

    forv_PNode(pn, pnodes) {
        if (pn->code && pn->code->kind == Code_LABEL && pn->code->label[0]) {
            getLLVMBasicBlock(pn->code->label[0], llvm_func);
        }
    }
    // Ensure entry block is correctly mapped if it's from a label
    if (ifa_fun->entry->code && ifa_fun->entry->code->kind == Code_LABEL && ifa_fun->entry->code->label[0]) {
         llvm::BasicBlock* entry_bb = getLLVMBasicBlock(ifa_fun->entry->code->label[0], llvm_func);
         // Move this block to the beginning of the function if it's not already the first one.
         // If the auto-created entry block by LLVM is empty and not our target, we can remove it.
         // However, it's safer to just ensure our real entry block is properly linked.
         // For now, translatePNode for the entry node will handle setting the insert point.
         if (entry_bb != &llvm_func->getEntryBlock() && llvm_func->getEntryBlock().empty()) {
             // This means llvm_func->getEntryBlock() was the one created by Function::Create.
             // We need to ensure our actual IF1 entry point PNode starts filling *its* corresponding BB.
         } else if (entry_bb != &llvm_func->getEntryBlock()) {
            // This means llvm_func->getEntryBlock() was the one created by Function::Create.
            // We need to ensure our actual IF1 entry point PNode starts filling *its* corresponding BB.
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
    Builder->SetInsertPoint(&llvm_func->getEntryBlock(), llvm_func->getEntryBlock().begin()); // Allocas at the top
    llvm::DIFile* di_file_for_locals = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getFile() : nullptr;
    unsigned func_start_line = llvm_func->getSubprogram() ? llvm_func->getSubprogram()->getLine() : 0;

    forv_Var(v, ifa_fun->fa_all_Vars) { // Or a more specific list of locals
        if (v && v->sym && v->sym->is_local && !v->is_formal && !v->llvm_value) { // Not an argument and not yet mapped
            llvm::Type *var_llvm_type = getLLVMType(v->type);
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
    if (var->llvm_value) {
        // If it's an AllocaInst (local variable), we need to load it.
        // If it's an SSA value (Argument or Instruction result), we use it directly.
        if (llvm::isa<llvm::AllocaInst>(var->llvm_value)) {
            if (!var->type) { // Should have type if alloca was created
                 fail("Var %s is alloca but has no type for load", var->sym->name); return nullptr;
            }
            llvm::Type* expected_type = getLLVMType(var->type);
            if (!expected_type) return nullptr;
            return Builder->CreateLoad(expected_type, var->llvm_value, var->sym->name ? (std::string(var->sym->name) + ".load") : "");
        }
        return var->llvm_value;
    }

    // Handle global variables or constants if not mapped through allocas
    if (var->sym && var->sym->is_constant) {
        // This needs a proper constant materialization function
        // For now, assume it's handled elsewhere or error
        fail("Constant Var %s not yet materialized as LLVM Value", var->sym->name);
        return nullptr;
    }
    if (var->sym && !var->sym->is_local) { // Could be a global
        // Global variable handling will be separate
        fail("Global Var %s not yet supported in getLLVMValue", var->sym->name);
        return nullptr;
    }

    fail("Var %s has no LLVM value and is not a recognized constant/global", var->sym->name ? var->sym->name : "unnamed_var");
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


static void translatePNode(PNode *pn, Fun *ifa_fun) {
    if (!pn || !pn->code) return;
    if (!ifa_fun || !ifa_fun->llvm) {
        fail("Invalid ifa_fun or missing llvm_func for PNode translation");
        return;
    }
    llvm::Function *llvm_func = ifa_fun->llvm;

    // Set current debug location
    // TODO: Get correct line/col from PNode or its AST
    unsigned line = pn->code->line() ? pn->code->line() : ifa_fun->line();
    unsigned col = 0; // TODO: Get column info
    Builder->SetCurrentDebugLocation(llvm::DILocation::get(*TheContext, line, col, llvm_func->getSubprogram()));

    switch (pn->code->kind) {
        case Code_LABEL: {
            if (pn->code->label[0]) {
                llvm::BasicBlock *bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                // If previous block wasn't terminated, branch to this new label.
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
            if (pn->code->label[0]) {
                llvm::BasicBlock *dest_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                // Ensure current block is set for the Builder
                if (!Builder->GetInsertBlock()) {
                    // This can happen if a GOTO is the first instruction in a new BB that wasn't set up by a LABEL
                    // It's a bit of a chicken-and-egg. Assume getLLVMBasicBlock for this GOTO's "natural" block
                    // should have been called or this PNode is part of an existing block.
                    // For safety, try to find or create a BB for this PNode's location.
                    // This part is tricky and implies PNodes should be associated with BBs more directly.
                    // Let's assume if we are here, the insert point is already valid from a prior LABEL or entry.
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
                llvm::Value *rhs_llvm_val = getLLVMValue(rhs_var, ifa_fun);
                if (rhs_llvm_val) {
                    setLLVMValue(lhs_var, rhs_llvm_val, ifa_fun);
                } else {
                    fail("Could not get LLVM value for RHS of MOVE: %s", rhs_var->sym->name);
                }
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

                llvm::BasicBlock *true_bb = nullptr;
                llvm::BasicBlock *false_bb = nullptr;
                // In IF1, label[0] is True, label[1] is False target for IF.
                if (pn->code->label[0]) true_bb = getLLVMBasicBlock(pn->code->label[0], llvm_func);
                if (pn->code->label[1]) false_bb = getLLVMBasicBlock(pn->code->label[1], llvm_func);

                if (!true_bb || !false_bb) {
                    fail("Code_IF PNode missing true or false destination labels");
                    return;
                }
                Builder->CreateCondBr(cond_llvm_val, true_bb, false_bb);
            } else {
                fail("Code_IF PNode has no condition variable");
            }
            break;
        }
        case Code_SEND: {
            // This will be very complex. For now, a placeholder.
            // Need to differentiate between primops and function calls.
            if (pn->prim) {
                // Handle primitive operations (P_prim_add, P_prim_reply etc.)
                // Example: P_prim_reply for return
                if (pn->prim->index == P_prim_reply) {
                    if (pn->rvals.n > 3) { // As per cg.cc: rvals[3] is the return value
                        Var* ret_val_var = pn->rvals[3];
                        llvm::Value* ret_llvm_val = getLLVMValue(ret_val_var, ifa_fun);
                        if (ret_llvm_val) {
                            if (ret_llvm_val->getType() == llvm_func->getReturnType()) {
                                Builder->CreateRet(ret_llvm_val);
                            } else if (llvm_func->getReturnType()->isVoidTy() && ret_val_var->type == sym_void_type) {
                                Builder->CreateRetVoid();
                            }
                             else {
                                fail("Return value type mismatch for %s. Expected %s, got %s.",
                                     ifa_fun->sym->name,
                                     getTypeName(llvm_func->getReturnType()).c_str(),
                                     getTypeName(ret_llvm_val->getType()).c_str());
                                // As a fallback, if it's void, just return void.
                                Builder->CreateRetVoid();
                            }
                        } else {
                            fail("Could not get LLVM value for return variable %s in function %s", ret_val_var->sym->name, ifa_fun->sym->name);
                            Builder->CreateRetVoid(); // Fallback
                        }
                    } else if (llvm_func->getReturnType()->isVoidTy()){ // Reply with no explicit value for void function
                        Builder->CreateRetVoid();
                    } else {
                         fail("P_prim_reply for non-void function %s has insufficient rvals (%d)", ifa_fun->sym->name, pn->rvals.n);
                         // Create some dummy return to make LLVM happy if possible
                         if (llvm_func->getReturnType()->isPointerTy())
                            Builder->CreateRet(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(llvm_func->getReturnType())));
                         else if (llvm_func->getReturnType()->isIntegerTy())
                            Builder->CreateRet(llvm::ConstantInt::get(llvm_func->getReturnType(), 0));
                         else if (llvm_func->getReturnType()->isFloatingPointTy())
                            Builder->CreateRet(llvm::ConstantFP::get(llvm_func->getReturnType(), 0.0));
                         else // Default to void if we can't make a dummy for the type. This might be invalid.
                            Builder->CreateRetVoid();
                    }
                } else {
                    // Other primitives to be implemented
                    fprintf(stderr, "Unhandled primitive SEND: %s in function %s\n", pn->prim->name, ifa_fun->sym->name);
                }
            } else {
                // Handle direct function calls
                fprintf(stderr, "Unhandled function call SEND in function %s\n", ifa_fun->sym->name);
            }
            break;
        }
        // TODO: Handle Code_PHI, Code_SEQ, Code_CONC, Code_NOP, etc.
        default:
            fprintf(stderr, "Unhandled PNode code kind: %d in function %s\n", pn->code->kind, ifa_fun->sym->name);
            break;
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

  // Try to use llc first
  // Note: TheModule might not be populated if this is called without prior codegen
  // This function should ideally take FA* and Fun* to ensure TheModule is ready
  // or rely on `llvm_codegen_write_ir` having been called.
  sprintf(cmd, "llc -filetype=obj %s -o %s", ll_file, obj_file);
  int res = system(cmd);

  if (res != 0) {
    fprintf(stderr, "llc command failed or not found for %s, trying clang...\\n", ll_file);
    sprintf(cmd, "clang -c %s -o %s", ll_file, obj_file);
    res = system(cmd);
    if (res != 0) {
      fail("LLVM IR compilation failed for %s using clang.", ll_file);
      return res;
    }
  }

  fprintf(stderr, "LLVM IR from %s compiled to %s\\n", ll_file, obj_file);
  // Example of linking: clang %s_output.o libruntime.a -o %s_executable
  // This should be part of the Makefile logic.
  return 0;
}

