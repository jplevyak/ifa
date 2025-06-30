#ifndef _LIBRUNTIME_H_
#define _LIBRUNTIME_H_

#include <stdint.h> // For int64_t, uint64_t, int8_t, int32_t
#include <stddef.h> // For size_t

// --- Memory Management ---
// Initializes the runtime memory system.
void RT_Mem_Init(void);

// Allocates raw memory of 'size' bytes, typically for an object.
// Corresponds to _CG_prim_new.
void* RT_Prim_New(size_t size);

// --- Opaque Runtime Types ---
// These are forward declarations. The actual structs would be defined
// in the runtime's C/C++ implementation. They represent runtime-managed data.
typedef struct RT_String_s RT_String;
typedef struct RT_List_s RT_List;
typedef struct RT_Tuple_s RT_Tuple;
typedef struct RT_Closure_s RT_Closure;

// A generic pointer type, can be used for elements in collections
// or for any type whose specifics are handled by the runtime.
typedef void* RT_Any;

// Represents an IF1 symbol at runtime, if needed for features like reflection.
typedef struct {
    int32_t id;         // Unique ID of the symbol
    const char* name;   // Name of the symbol
} RT_Symbol;


// --- String Operations ---
// Creates a runtime string from a C literal.
// The runtime is responsible for managing the memory of the new RT_String.
RT_String* RT_String_Create(const char* literal_chars);

// Returns the length of a runtime string (number of characters).
int64_t RT_String_Len(RT_String* str);

// Returns the character (as int8_t) at a given index from a runtime string.
// Behavior for out-of-bounds index is defined by the runtime (e.g., error or specific value).
int8_t RT_String_GetCharAtIndex(RT_String* str, int64_t index);

// Concatenates two runtime strings, returning a new RT_String.
// The runtime manages the memory for the new string. s1 and s2 are not modified.
RT_String* RT_String_Concat(RT_String* s1, RT_String* s2);

// Compares two runtime strings.
// Returns 0 if s1 == s2, <0 if s1 < s2, >0 if s1 > s2.
int RT_String_Compare(RT_String* s1, RT_String* s2);


// --- List/Array Operations ---
// Creates a new list, potentially with an initial capacity.
// 'element_type_info' could be a tag or pointer used by the runtime for type-specific operations.
RT_List* RT_List_Create(void* element_type_info, int64_t initial_capacity);

// Returns the number of elements currently in the list.
int64_t RT_List_Len(RT_List* list);

// Returns a raw pointer to the underlying data buffer of the list.
// Useful for direct element access if the element layout is known.
void* RT_List_GetDataPtr(RT_List* list);

// Sets the element at 'index' in the list to 'element'.
// 'element' is RT_Any, implying it's often a pointer or a value fitting in void*.
void RT_List_SetElement(RT_List* list, int64_t index, RT_Any element);

// Gets the element at 'index' from the list.
RT_Any RT_List_GetElement(RT_List* list, int64_t index);

// Appends 'element' to the end of the list.
void RT_List_AppendElement(RT_List* list, RT_Any element);


// --- Tuple Operations ---
// Creates a tuple with a fixed 'count' of elements.
// 'element_type_info_array' could be an array of type information for each element if heterogeneous.
RT_Tuple* RT_Tuple_Create(int64_t count /*, void** element_type_info_array */);

// Sets the element at 'index' in the tuple to 'element'.
void RT_Tuple_SetElement(RT_Tuple* tuple, int64_t index, RT_Any element);

// Gets the element at 'index' from the tuple.
RT_Any RT_Tuple_GetElement(RT_Tuple* tuple, int64_t index);

// Note: Tuple length is fixed at creation and cannot be changed. Tuple elements can be modified.


// --- Closure Operations ---
// Creates a closure object.
// 'function_ptr' is a pointer to the compiled code of the function.
// 'env_data' is a tuple (or similar structure) holding the captured free variables.
RT_Closure* RT_Closure_Create(void* function_ptr, RT_Tuple* env_data);

// Retrieves the function pointer from a closure object.
void* RT_Closure_GetFunctionPtr(RT_Closure* closure);

// Retrieves the environment data (captured variables) from a closure object.
RT_Tuple* RT_Closure_GetEnv(RT_Tuple* closure);


// --- Generic Operations ---
// Generic length function. 'collection_type_info' might help dispatch.
// Could apply to strings, lists, etc., if not using specialized versions.
int64_t RT_Prim_Len(void* collection_type_info, RT_Any collection);

// Generic clone function for any runtime-managed object.
RT_Any RT_Prim_Clone(RT_Any object_to_clone);

// Generic clone specifically for vector-like structures, if behavior differs.
RT_Any RT_Prim_Clone_Vector(RT_Any vector_to_clone);


// --- Printing Operations ---
// These functions print values to standard output.
// 'do_newline' (if 1) appends a newline character after printing the value.

void RT_Float_Print(double val, int do_newline);
void RT_String_Print(RT_String* str, int do_newline); // Assumes RT_String is printable
void RT_Char_Print(int8_t val, int do_newline);
void RT_Int_Print(int64_t val, int do_newline);
void RT_UInt_Print(uint64_t val, int do_newline);
void RT_Bool_Print(int8_t val, int do_newline); // LLVM i1 is often passed as i8
void RT_Pointer_Print(void* ptr, int do_newline); // Prints a pointer address
void RT_Unknown_Type_Print(int do_newline); // For types that cannot be printed directly
void RT_Newline_Print(void); // Prints just a newline

// --- Symbol Operations ---
// Creates a runtime symbol representation.
RT_Symbol RT_Symbol_Create(int32_t id, const char* name);


#endif // _LIBRUNTIME_H_
