# LLVM IR Serialization Format

This document describes the custom serialization format used to persist the Intermediate Representation (IR) to a file (typically with a `.ir` extension) and the process for serializing and deserializing it. The implementation is found in `ir_serialize.cc` and `ir_deserialize.cc`.

## Format Overview

The serialization format acts as a bridge between the frontend (which generates the IR) and the backend (which consumes it, e.g., for LLVM code generation). It is a text-based format designed to be mostly linear but supports circular references through ID-based lookups.

The file structure consists of several sections appearing in a specific order:

1.  **Header**
2.  **Symbols (`SYMS`)**
3.  **Labels (`LABELS`)**
4.  **Codes (`CODES`)**
5.  **Functions (`FUNS`)**
6.  **Entry Point (`ENTRY`)**

### 1. Header
The file MUST start with the magic string:
```
IFA_IR_V1
```

### 2. Symbols
This section defines all *live* symbols in the program.

*   **Header:** `SYMS <count>`
*   **Item:** `SYM <id> <kind> <name_len> <name> <type_id> <flags> ...`

**Fields:**
*   `id`: unique integer ID (reassigned to be contiguous 0..N-1 during serialization).
*   `kind`: integer corresponding to `Type_kind` enum.
*   `name`: length-prefixed string (space + length + space + string).
*   `type_id`: ID of the symbol's type (reference to another SYM).
*   `flags`: Bitmask:
    *   `1`: `is_local`
    *   `2`: `is_constant` (or treats constant)
    *   `4`: `is_external`
    *   `8`: `is_fun`
*   **Constant Data:** (If `is_constant` flag is set)
    *   `num_kind`: kind of numeric (NONE, INT, FLOAT).
    *   Value:
        *   If NONE: String constant (length-prefixed).
        *   If INT: `int64` value.
        *   If FLOAT: `double` value.
*   **Has List:** (for composite types/scopes)
    *   `count` followed by list of symbol IDs.
*   **Return Type:** ID of return type symbol.
*   **Element Type:** ID of element type symbol (for arrays/refs).
*   **Num Index:** Integer index for primitives.

### 3. Labels
This section defines jump targets (labels).

*   **Header:** `LABELS <count>`
*   **Item:** `LABEL <id>`
    *   `id`: Reassigned contiguous index (0..N-1).

### 4. Codes (PNodes)
This section defines the code graph (AST/CFG nodes), known as `Code` or `PNode` structures.

*   **Header:** `CODES <count>`
*   **Item:** `CODE <id> <kind> <rvals...> <lvals...> <labels...> <subs...> <prim> <line>`

**Fields:**
*   `id`: Unique integer ID (1-based, 1..N).
*   `kind`: Integer corresponding to `Code_kind` enum.
*   `rvals`: Count followed by list of Symbol IDs (inputs).
*   `lvals`: Count followed by list of Symbol IDs (outputs).
*   `labels`: Two integers (indices into the LABELS section) for control flow targets.
*   `subs`: Count followed by list of Code IDs (children/sub-blocks).
*   `prim`: Integer index of the associated Primitive (if any).
*   `line`: Source line number.

### 5. Functions (Closures)
This section reconstructs the high-level Function abstractions (`Fun` objects) and links them to the Code graph and Symbols.

*   **Header:** `FUNS <count>`
*   **Item:** `FUN <sym_id> <code_id> ...`

**Fields:**
*   `sym_id`: ID of the Symbol representing the function.
*   `code_id`: ID of the root Code node for the function's body.
*   **Args:** Count followed by list of Symbol IDs (positional arguments).
*   **Rets:** Count followed by list of Symbol IDs (return values).
*   **Calls Map:** Mapping of call sites to target functions.
    *   Count followed by pairs of `<call_code_id> <target_sym_id>`.
    *   `call_code_id`: ID of the `PNode` (Code) performing the call.
    *   `target_sym_id`: ID of the function Symbol being called.
*   `is_varargs`: Boolean flag (0/1).
*   `is_external`: Boolean flag (0/1).
*   `cg_string`: Length-prefixed string (Code Gen specific data).

### 6. Entry Point
Defines the main entry point symbol of the program.

*   **Format:** `ENTRY <sym_id>`

---

## Process

### Serialization (`ir_serialize.cc`)
The serialization process involves traversing the in-memory IR structures (`IF1`, `FA`) and writing them to disk.

1.  **Liveness Analysis:** Only "live" symbols and codes are serialized. The serializer checks `sym->live` and `code->live` flags.
2.  **ID Reassignment:**
    *   **Symbols:** Traverses `if1->allsyms` collecting live symbols and mapping their original IDs to new contiguous IDs (0..N).
    *   **Codes:** Recursively visits the Code tree starting from live closures (`if1->allclosures`), assigning temporary IDs (1..M).
    *   **Labels:** Collects live labels and assigns indices.
3.  **Writing:**
    *   Writes sections in order.
    *   Strings are length-prefixed to handle arbitrary content.
    *   Pointers (to Syms, Codes, Labels) are converted to their mapped integer IDs.

### Deserialization (`ir_deserialize.cc`)
The deserialization process reconstructs the IR in memory.

1.  **Validation:** Checks the `IFA_IR_V1` header.
2.  **Symbols (Two Passes):**
    *   **Pass 1:** Allocate `Sym` objects for all IDs to handle forward references (circular types, etc.).
    *   **Pass 2:** Read details for each Sym and fill fields (linking types/children using IDs).
3.  **Labels:** Create `Label` objects.
4.  **Codes (Two Passes):**
    *   **Pass 1:** Allocate `Code` objects for all IDs to handle forward references (parent/child pointers).
    *   **Pass 2:** Read details and link `rvals`, `lvals`, `labels`, and `sub` (children) using IDs.
5.  **Functions:**
    *   Creates `Fun` objects associated with Symbols.
    *   Rebuilds argument and return value lists (creating `Var` wrappers around `Syms`).
    *   **Calls Map:** reconstructs the `calls` map in `Fun`, linking call site PNodes (found via Code ID) to target Functions (found via Sym ID).
    *   **CFG Reconstruction:** Implicitly triggers `fun->build_cfg()` or similar logic if the function entry is missing, ensuring the Control Flow Graph is ready for the backend.
6.  **Entry Point:** Sets `if1->top` to the entry symbol.
7.  **Post-Process:** Ensures all Symbols have associated `Var` objects (needed by some backend passes like `llvm.cpp`).
