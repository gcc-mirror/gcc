/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*! \file
    \brief Function and Variable tables used by the runtime library
*/

#ifndef OFFLOAD_TABLE_H_INCLUDED
#define OFFLOAD_TABLE_H_INCLUDED

#include "offload_util.h"

#define OFFLOAD_VERSION_16   1600
#define OFFLOAD_VERSION_17   1700

// Template representing double linked list of tables
template <typename T> class TableList {
public:
    // table type
    typedef T Table;

    // List node
    struct Node {
        Table   table;
        Node*   prev;
        Node*   next;
    };

public:
    explicit TableList(Node *node = 0) : m_head(node) {}

    void add_table(Node *node) {
        m_lock.lock();
        if (m_head != 0) {
            node->next = m_head;
            m_head->prev = node;
        }
        m_head = node;

        m_lock.unlock();
    }

    void remove_table(Node *node) {
        if (node->next != 0) {
            node->next->prev = node->prev;
        }
        if (node->prev != 0) {
            node->prev->next = node->next;
        }
        if (m_head == node) {
            m_head = node->next;
        }
    }

protected:
    Node*           m_head;
    mutex_t         m_lock;
};

// Function lookup table.
struct FuncTable {
    //! Function table entry
    /*! This table contains functions created from offload regions.   */
    /*! Each entry consists of a pointer to the function's "key"
        and the function address.                                     */
    /*! Each shared library or executable may contain one such table. */
    /*! The end of the table is marked with an entry whose name field
        has value -1.                                                 */
    struct Entry {
        const char* name; //!< Name of the function
        void*       func; //!< Address of the function
    };

    // entries
    const Entry *entries;

    // max name length
    int64_t max_name_len;
};

// Function table
class DLL_LOCAL FuncList : public TableList<FuncTable> {
public:
    explicit FuncList(Node *node = 0) : TableList<Table>(node),
                                        m_max_name_len(-1)
    {}

    // add table to the list
    void add_table(Node *node) {
        // recalculate max function name length
        m_max_name_len = -1;

        // add table
        TableList<Table>::add_table(node);
    }

    // find function address for the given name
    const void* find_addr(const char *name);

    // find function name for the given address
    const char* find_name(const void *addr);

    // max name length from all tables in the list
    int64_t max_name_length(void);

    // debug dump
    void dump(void);

private:
    // max name length within from all tables
    int64_t m_max_name_len;
};

#define VAR_ALLOC_TYPE  uint64_t
#define OPENMP_IMPLICIT   1    // Compiler promoted openmp declare var
                               // due to implicit use without openmp declare 
#define OPENMP_LINK       2    // Openmp link clause in openmp declare

#define IS_OPENMP_IMPLICIT(var_alloc_type)         (var_alloc_type & 1)
#define IS_OPENMP_LINK(var_alloc_type)             (var_alloc_type & 2)
#define IS_OPENMP_IMPLICIT_OR_LINK(var_alloc_type) (var_alloc_type & 3)

// Table entry for static variables
struct VarTable {
    //! Variable table entry
    /*! This table contains statically allocated variables marked with
        __declspec(target(mic) or #pragma omp declare target.           */
    /*! Each entry consists of a pointer to the variable's "key",
        the variable address and its size in bytes.                     */
    /*! Because memory allocation is done from the host,
        the MIC table does not need the size of the variable.           */
    /*! Padding to make the table entry size a power of 2 is necessary
        to avoid "holes" between table contributions from different object
        files on Windows when debug information is specified with /Zi.  */
    struct Entry {
        const char* name; //!< Name of the variable
        void*       addr; //!< Address of the variable

#if HOST_LIBRARY
        VAR_ALLOC_TYPE  var_alloc_type;
        uint64_t    size;
#endif
    };

    // Table terminated by an entry with name == -1
    const Entry *entries;
};

// List of var tables
class DLL_LOCAL VarList : public TableList<VarTable> {
public:
    VarList() : TableList<Table>()
    {}

    // debug dump
    void dump();

public:

    Node * get_head() {
        return m_head;
    }

public:
    // Entry representation in a copy buffer
    struct BufEntry {
        intptr_t name;
        intptr_t addr;
    };

    // Calculate the number of elements in the table and
    // returns the size of buffer for the table
    int64_t table_size(int64_t &nelems);

    // Copy table contents to given buffer. It is supposed to be large
    // enough to hold all elements as string table.
    void table_copy(void *buf, int64_t nelems);

    // Patch name offsets in a table after it's been copied to other side
    static void table_patch_names(void *buf, int64_t nelems);
};

DLL_LOCAL extern FuncList __offload_entries;
DLL_LOCAL extern FuncList __offload_funcs;
DLL_LOCAL extern VarList  __offload_vars;

// Section names where the lookup tables are stored
#ifdef TARGET_WINNT
#define OFFLOAD_ENTRY_TABLE_SECTION_START   ".OffloadEntryTable$a"
#define OFFLOAD_ENTRY_TABLE_SECTION_END     ".OffloadEntryTable$z"

#define OFFLOAD_FUNC_TABLE_SECTION_START    ".OffloadFuncTable$a"
#define OFFLOAD_FUNC_TABLE_SECTION_END      ".OffloadFuncTable$z"

#define OFFLOAD_VAR_TABLE_SECTION_START     ".OffloadVarTable$a"
#define OFFLOAD_VAR_TABLE_SECTION_END       ".OffloadVarTable$z"

#define OFFLOAD_CRTINIT_SECTION_START       ".CRT$XCT"

#pragma section(OFFLOAD_CRTINIT_SECTION_START, read)

#else  // TARGET_WINNT

#define OFFLOAD_ENTRY_TABLE_SECTION_START   ".OffloadEntryTable."
#define OFFLOAD_ENTRY_TABLE_SECTION_END     ".OffloadEntryTable."

#define OFFLOAD_FUNC_TABLE_SECTION_START    ".OffloadFuncTable."
#define OFFLOAD_FUNC_TABLE_SECTION_END      ".OffloadFuncTable."

#define OFFLOAD_VAR_TABLE_SECTION_START     ".OffloadVarTable."
#define OFFLOAD_VAR_TABLE_SECTION_END       ".OffloadVarTable."
#endif // TARGET_WINNT

#pragma section(OFFLOAD_ENTRY_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_ENTRY_TABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_FUNC_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_FUNC_TABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_VAR_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_VAR_TABLE_SECTION_END, read, write)


// Set library version
extern "C" void __offload_set_version(int v);

// register/unregister given tables
extern "C" void __offload_register_tables(
    FuncList::Node *entry_table,
    FuncList::Node *func_table,
    VarList::Node *var_table
);

extern "C" void __offload_unregister_tables(
    FuncList::Node *entry_table,
    FuncList::Node *func_table,
    VarList::Node *var_table
);


#ifdef MYO_SUPPORT

#include <myotypes.h>
#include <myoimpl.h>
#include <myo.h>

#ifdef TARGET_WINNT
#define MYO_TABLE_END_MARKER() reinterpret_cast<const char*>(-1)
#else // TARGET_WINNT
#define MYO_TABLE_END_MARKER() reinterpret_cast<const char*>(0)
#endif // TARGET_WINNT

// Host and Target-side MYO shared variable table entry layout
typedef MyoiSharedVarEntry SharedTableEntry;

#if HOST_LIBRARY

// Host-side MYO function table entry layout
typedef struct {
    //! Function Name
    const char *funcName;
    //! Function Address
    void *funcAddr;
    //! Local Thunk Address
    void *localThunkAddr;
#ifdef TARGET_WINNT
    // Dummy to pad up to 32 bytes
    void *dummy;
#endif // TARGET_WINNT
} FptrTableEntry;

// Host-side MYO init routine table entry layout
typedef struct {
#ifdef TARGET_WINNT
    // Dummy to pad up to 16 bytes
    // Function Name
    const char *funcName;
#endif // TARGET_WINNT
    void (*func)(MyoArena);
} InitTableEntry;

#else // HOST_LIBRARY

// Target-side MYO function table entry layout
typedef MyoiTargetSharedFptrEntry   FptrTableEntry;

// Target-side MYO init routine table entry layout
struct InitTableEntry {
    void (*func)(void);
};

#endif // HOST_LIBRARY

#ifdef TARGET_WINNT

#define OFFLOAD_MYO_SHARED_TABLE_SECTION_START          ".MyoSharedTable$a"
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_END            ".MyoSharedTable$z"

#define OFFLOAD_MYO_SHARED_VTABLE_SECTION_START         ".MyoSharedVTable$a"
#define OFFLOAD_MYO_SHARED_VTABLE_SECTION_END           ".MyoSharedVTable$z"

#define OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_START     ".MyoSharedInitTable$a"
#define OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_END       ".MyoSharedInitTable$z"

#define OFFLOAD_MYO_FPTR_TABLE_SECTION_START            ".MyoFptrTable$a"
#define OFFLOAD_MYO_FPTR_TABLE_SECTION_END              ".MyoFptrTable$z"

#else  // TARGET_WINNT

#define OFFLOAD_MYO_SHARED_TABLE_SECTION_START          ".MyoSharedTable."
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_END            ".MyoSharedTable."

#define OFFLOAD_MYO_SHARED_VTABLE_SECTION_START         ".MyoSharedVTable."
#define OFFLOAD_MYO_SHARED_VTABLE_SECTION_END           ".MyoSharedVTable."

#define OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_START     ".MyoSharedInitTable."
#define OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_END       ".MyoSharedInitTable."

#define OFFLOAD_MYO_FPTR_TABLE_SECTION_START            ".MyoFptrTable."
#define OFFLOAD_MYO_FPTR_TABLE_SECTION_END              ".MyoFptrTable."

#endif // TARGET_WINNT

#pragma section(OFFLOAD_MYO_SHARED_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_SHARED_TABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_MYO_SHARED_VTABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_SHARED_VTABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_MYO_FPTR_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_FPTR_TABLE_SECTION_END, read, write)

// List of MYO shared variable tables
struct MYOVarTable {
    typedef SharedTableEntry Entry;
    const Entry *entries;
};

class MYOVarTableList : public TableList<MYOVarTable> {
public:
    MYOVarTableList() : TableList<Table>()
    {}

    // add table to the list
    void add_table(Node *node) {
        // add table
        TableList<Table>::add_table(node);
    }

    // debug dump
    void dump(void);

    // check if any shared variables
    bool is_empty();

    // process the table contents for ordinary variables
    void process();

    // process the table contents for vtable objects
    void process_vtable();
};

// List of MYO shared function tables
struct MYOFuncTable {
    typedef FptrTableEntry Entry;
    const Entry *entries;
};

class MYOFuncTableList : public TableList<MYOFuncTable> {
public:
    MYOFuncTableList() : TableList<Table>()
    {}

    // add table to the list
    void add_table(Node *node) {
        // add table
        TableList<Table>::add_table(node);
    }

    // debug dump
    void dump(void);

    // check if any shared functions
    bool is_empty();

    // process the table contents
    void process();
};

// List of MYO shared variable initialization routine tables
struct MYOInitTable {
    typedef InitTableEntry Entry;
    const Entry *entries;
};

class MYOInitTableList : public TableList<MYOInitTable> {
public:
    MYOInitTableList() : TableList<Table>()
    {}

    // add table to the list
    void add_table(Node *node) {
        // add table
        TableList<Table>::add_table(node);
    }

    // debug dump
    void dump(void);

    // check if any init routines
    bool is_empty();

    // process the table contents
    void process();
};

extern MYOVarTableList  __offload_myo_var_tables;
extern MYOVarTableList  __offload_myo_vtable_tables;
extern MYOFuncTableList __offload_myo_func_tables;
extern MYOInitTableList __offload_myo_init_tables;

extern "C" void __offload_myoRegisterTables1(
    MYOInitTableList::Node *init_table,
    MYOVarTableList::Node  *shared_table,
    MYOVarTableList::Node  *shared_vtable,
    MYOFuncTableList::Node *fptr_table
); 

extern "C" void __offload_myoRemoveTables(
    MYOInitTableList::Node *init_table,
    MYOVarTableList::Node  *shared_table,
    MYOVarTableList::Node  *shared_vtable,
    MYOFuncTableList::Node *fptr_table
);

#endif // MYO_SUPPORT

#endif  // OFFLOAD_TABLE_H_INCLUDED
