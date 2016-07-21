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
#include "offload_target.h"
#ifdef MYO_SUPPORT
#include "offload_myo_target.h"
#endif // MYO_SUPPORT
#endif // HOST_LIBRARY

#ifdef TARGET_WINNT
#define ALLOCATE(name) __declspec(allocate(name))
#else // TARGET_WINNT
#define ALLOCATE(name) __attribute__((section(name)))
#endif // TARGET_WINNT

// offload entry table
ALLOCATE(OFFLOAD_ENTRY_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FuncTable::Entry)))
#endif // TARGET_WINNT
static FuncTable::Entry __offload_entry_table_end = { (const char*)-1 };

// offload function table
ALLOCATE(OFFLOAD_FUNC_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FuncTable::Entry)))
#endif // TARGET_WINNT
static FuncTable::Entry __offload_func_table_end = { (const char*)-1 };

// data table
ALLOCATE(OFFLOAD_VAR_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(VarTable::Entry)))
#endif // TARGET_WINNT
static VarTable::Entry __offload_var_table_end = { (const char*)-1 };

#ifdef MYO_SUPPORT

// offload myo shared var section epilog
ALLOCATE(OFFLOAD_MYO_SHARED_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(SharedTableEntry)))
static MYOVarTable::Entry __offload_myo_shared_var_end =
    { (const char*)-1, 0 };
#else // TARGET_WINNT
static MYOVarTable::Entry __offload_myo_shared_var_end = { 0 };
#endif // TARGET_WINNT

// offload myo shared vtable section epilog
ALLOCATE(OFFLOAD_MYO_SHARED_VTABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(SharedTableEntry)))
static MYOVarTable::Entry __offload_myo_shared_vtable_end =
    { (const char*)-1, 0 };
#else // TARGET_WINNT
static MYOVarTable::Entry __offload_myo_shared_vtable_end = { 0 };
#endif // TARGET_WINNT

//#if HOST_LIBRARY
// offload myo shared var init section epilog
ALLOCATE(OFFLOAD_MYO_SHARED_INIT_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(InitTableEntry)))
static MYOInitTable::Entry __offload_myo_init_table_end =
    { (const char*)-1, 0 };
#else // TARGET_WINNT
static MYOInitTable::Entry __offload_myo_init_table_end = { 0 };
#endif // TARGET_WINNT
//#endif // HOST_LIBRARY

// offload myo fptr section epilog
ALLOCATE(OFFLOAD_MYO_FPTR_TABLE_SECTION_END)
#ifdef TARGET_WINNT
__declspec(align(sizeof(FptrTableEntry)))
static MYOFuncTable::Entry __offload_myo_fptr_table_end =
    { (const char*)-1, 0, 0 };
#else // TARGET_WINNT
static MYOFuncTable::Entry __offload_myo_fptr_table_end = { 0 };
#endif // TARGET_WINNT

#endif // MYO_SUPPORT
