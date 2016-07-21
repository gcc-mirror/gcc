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


#if HOST_LIBRARY
#include "offload_table.h"
#ifdef MYO_SUPPORT
#include "offload_myo_host.h"
#endif // MYO_SUPPORT
#else
#include "compiler_if_target.h"
#include "offload_target.h"
#ifdef MYO_SUPPORT
#include "offload_myo_target.h"
#endif // MYO_SUPPORT
#endif // HOST_LIBRARY

// Initializes library and registers specified offload image.
// Don't use this declarations from offload_host.h as offload_table.h
// is used instead of it. Using offload_host.h contradicts with
// STL library compiled with VS2010.
extern "C" bool __offload_register_image(const void* image);
extern "C" void __offload_unregister_image(const void* image);
extern "C" bool __offload_target_image_is_executable(const void *image);

#ifdef TARGET_WINNT
#define ALLOCATE(name) __declspec(allocate(name))
#define DLL_LOCAL
#else // TARGET_WINNT
#define ALLOCATE(name) __attribute__((section(name)))
#define DLL_LOCAL  __attribute__((visibility("hidden")))
#endif // TARGET_WINNT

#if HOST_LIBRARY
// the host program/shared library should always have __offload_target_image
// symbol defined. This symbol specifies the beginning of the target program
// image.
extern "C" DLL_LOCAL const void* __offload_target_image;
#else // HOST_LIBRARY
// Define a weak main which would be used on target side in case usere's
// source file containing main does not have offload code.
#pragma weak main
int main(void)
{
    OFFLOAD_TARGET_MAIN();
    return 0;
}

#pragma weak MAIN__
extern "C" int MAIN__(void)
{
    OFFLOAD_TARGET_MAIN();
    return 0;
}
#endif // HOST_LIBRARY

// offload section prolog
ALLOCATE(OFFLOAD_ENTRY_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FuncTable::Entry)))
#endif // TARGET_WINNT
static FuncTable::Entry __offload_entry_table_start = { 0 };

// list element for the current module
static FuncList::Node __offload_entry_node = {
    { &__offload_entry_table_start + 1, -1 },
    0, 0
};

// offload fp section prolog
ALLOCATE(OFFLOAD_FUNC_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FuncTable::Entry)))
#endif // TARGET_WINNT
static FuncTable::Entry __offload_func_table_start = { 0 };

// list element for the current module
static FuncList::Node __offload_func_node = {
    { &__offload_func_table_start + 1, -1 },
    0, 0
};

// offload fp section prolog
ALLOCATE(OFFLOAD_VAR_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(VarTable::Entry)))
#endif // TARGET_WINNT
static VarTable::Entry __offload_var_table_start = { 0 };

// list element for the current module
static VarList::Node __offload_var_node = {
    { &__offload_var_table_start + 1 },
    0, 0
};

#ifdef MYO_SUPPORT

// offload myo shared var section prolog
// first element is empty
ALLOCATE(OFFLOAD_MYO_SHARED_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(SharedTableEntry)))
#endif // TARGET_WINNT
static MYOVarTable::Entry __offload_myo_shared_var_start = { 0 };

// list element for the current module
// table entry pointer skips the empty first entry
static MYOVarTableList::Node __offload_myo_shared_var_node = {
    { &__offload_myo_shared_var_start + 1 },
    0, 0
};

// offload myo shared vtable section prolog
// first element is empty
ALLOCATE(OFFLOAD_MYO_SHARED_VTABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(SharedTableEntry)))
#endif // TARGET_WINNT
static MYOVarTable::Entry __offload_myo_shared_vtable_start = { 0 };

// list element for the current module
// table entry pointer skips the empty first entry
static MYOVarTableList::Node __offload_myo_shared_vtable_node = {
    { &__offload_myo_shared_vtable_start + 1 },
    0, 0
};

// offload myo shared var init section prolog
// first element is empty
ALLOCATE(OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(InitTableEntry)))
#endif // TARGET_WINNT
static MYOInitTable::Entry __offload_myo_init_table_start = { 0 };

// list element for the current module
// table entry pointer skips the empty first entry
static MYOInitTableList::Node __offload_myo_init_table_node = {
    { &__offload_myo_init_table_start + 1 },
    0, 0
};

// The functions and variables needed for a built-in
// remote function entry for vtable initialization on MIC

#if !HOST_LIBRARY
MyoError __offload_init_vtables(void)
{
    SharedTableEntry *t_start;

    //OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);
    t_start = &__offload_myo_shared_vtable_start + 1;
    //OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, t_start);
    while (t_start->varName != 0) {
        //OFFLOAD_DEBUG_TRACE(4,
        //    "myo shared vtable \"%s\" &myo_ptr = %p myo_ptr = %p\n",
        //    t_start->varName,
        //    (void *)(t_start->sharedAddr),
        //    ((void **)(t_start->sharedAddr))[0]);
        t_start++;
    }

    __offload_myo_shared_init_table_process(
        &__offload_myo_init_table_start + 1);
    return MYO_SUCCESS;
}
#endif  // !HOST_LIBRARY

static void vtable_initializer()
{
}

#if !HOST_LIBRARY
static MyoError vtable_initializer_wrapper()
{
    __offload_myoAcquire();
    __offload_init_vtables();
    __offload_myoRelease();
    return MYO_SUCCESS;
}
#endif

static void* __offload_vtable_initializer_thunk_ptr = 0;

// offload myo fptr section prolog
// first element is pre-initialized to the MIC vtable initializer
ALLOCATE(OFFLOAD_MYO_FPTR_TABLE_SECTION_START)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FptrTableEntry)))
#endif // TARGET_WINNT
static MYOFuncTable::Entry __offload_myo_fptr_table_start = {
#if HOST_LIBRARY
    "--vtable_initializer--",
    (void*)&vtable_initializer,
    (void*)&__offload_vtable_initializer_thunk_ptr,
#ifdef TARGET_WINNT
    // Dummy to pad up to 32 bytes
    0
#endif // TARGET_WINNT
#else  // HOST_LIBRARY
    "--vtable_initializer--",
    (void*)&vtable_initializer,
    (void*)&vtable_initializer_wrapper,
    &__offload_vtable_initializer_thunk_ptr,
#endif // HOST_LIBRARY
};

// list element for the current module
static MYOFuncTableList::Node __offload_myo_fptr_table_node = {
    { &__offload_myo_fptr_table_start },
    0, 0
};

#endif // MYO_SUPPORT

// init/fini code which adds/removes local lookup data to/from the global list

static void offload_fini();
static void offload_fini_so();

#ifndef TARGET_WINNT
static void offload_init() __attribute__((constructor(101)));
#else // TARGET_WINNT
static void offload_init();

// Place offload initialization before user constructors
ALLOCATE(OFFLOAD_CRTINIT_SECTION_START)
static void (*addressof_offload_init)() = offload_init;
#endif // TARGET_WINNT

static void offload_init()
{
    bool success;

    // Set offload version
    __offload_set_version(OFFLOAD_VERSION_17);

    // register offload tables
    __offload_register_tables(&__offload_entry_node,
                              &__offload_func_node,
                              &__offload_var_node);

#if HOST_LIBRARY
    success = __offload_register_image(&__offload_target_image);
    if (!success)
    {
        return;
    }
#endif // HOST_LIBRARY
#ifdef MYO_SUPPORT
#if HOST_LIBRARY
    // If this was the main program register main atexit routine
    if (__offload_myoProcessTables(
            &__offload_target_image,
            &__offload_myo_init_table_node,
            &__offload_myo_shared_var_node,
            &__offload_myo_shared_vtable_node,
            &__offload_myo_fptr_table_node))
    {
        atexit(offload_fini);
#ifdef TARGET_WINNT
    } else {
        atexit(offload_fini_so);
#endif
    }
#else // HOST_LIBRARY
    __offload_myoProcessTables(
        &__offload_myo_init_table_start + 1,
        &__offload_myo_shared_var_start + 1,
        &__offload_myo_shared_vtable_start + 1,
        &__offload_myo_fptr_table_start
    );
#endif // HOST_LIBRARY
#endif // MYO_SUPPORT
}

#ifndef TARGET_WINNT
static void offload_fini_so() __attribute__((destructor(101)));
#endif // TARGET_WINNT

static void offload_fini()
{
#if HOST_LIBRARY
    __offload_unregister_image(&__offload_target_image);
#endif // HOST_LIBRARY
}

static void offload_fini_so()
{
    // Offload and MYO tables need to be removed from list
    // to prevent invalid accesses after dlclose
    // Remove offload tables
    __offload_unregister_tables(&__offload_entry_node,
                                &__offload_func_node,
                                &__offload_var_node);
#if HOST_LIBRARY
   if(!__offload_target_image_is_executable(&__offload_target_image)) {
      __offload_unregister_image(&__offload_target_image);
   }
#endif
#ifdef MYO_SUPPORT
#if HOST_LIBRARY
    // Remove MYO tables
    __offload_myoRemoveTables(
        &__offload_myo_init_table_node,
        &__offload_myo_shared_var_node,
        &__offload_myo_shared_vtable_node,
        &__offload_myo_fptr_table_node);
#endif // HOST_LIBRARY
#endif // MYO_SUPPORT
}
