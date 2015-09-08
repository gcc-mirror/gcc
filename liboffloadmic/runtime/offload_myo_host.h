/*
    Copyright (c) 2014-2015 Intel Corporation.  All Rights Reserved.

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


#ifndef OFFLOAD_MYO_HOST_H_INCLUDED
#define OFFLOAD_MYO_HOST_H_INCLUDED

#include <myotypes.h>
#include <myoimpl.h>
#include <myo.h>

#include "offload.h"
// undefine the following since offload.h defines them to malloc and free if __INTEL_OFFLOAD 
// is not defined which is the case when building the offload library
#undef _Offload_shared_malloc
#undef _Offload_shared_free
#undef _Offload_shared_aligned_malloc
#undef _Offload_shared_aligned_free
#include "offload_table.h"

// This function retained for compatibility with 15.0
extern "C" void __offload_myoRegisterTables(
    InitTableEntry *init_table,
    SharedTableEntry *shared_table,
    FptrTableEntry *fptr_table
);

// Process shared variable, shared vtable and function and init routine tables.
// In .dlls/.sos these will be collected together.
// In the main program, all collected tables will be processed.
extern "C" bool __offload_myoProcessTables(
    const void* image,
    MYOInitTableList::Node *init_table,
    MYOVarTableList::Node  *shared_table,
    MYOVarTableList::Node  *shared_vtable,
    MYOFuncTableList::Node *fptr_table
);

extern void __offload_myoFini(void);
extern bool __offload_myo_init_is_deferred(const void *image);

#endif // OFFLOAD_MYO_HOST_H_INCLUDED
