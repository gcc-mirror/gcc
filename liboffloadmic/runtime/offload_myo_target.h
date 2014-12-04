/*
    Copyright (c) 2014 Intel Corporation.  All Rights Reserved.

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


#ifndef OFFLOAD_MYO_TARGET_H_INCLUDED
#define OFFLOAD_MYO_TARGET_H_INCLUDED

#include <myotypes.h>
#include <myoimpl.h>
#include <myo.h>
#include "offload.h"

typedef MyoiSharedVarEntry          SharedTableEntry;
typedef MyoiTargetSharedFptrEntry   FptrTableEntry;

#ifdef TARGET_WINNT
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_START          ".MyoSharedTable$a"
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_END            ".MyoSharedTable$z"

#define OFFLOAD_MYO_FPTR_TABLE_SECTION_START            ".MyoFptrTable$a"
#define OFFLOAD_MYO_FPTR_TABLE_SECTION_END              ".MyoFptrTable$z"
#else  // TARGET_WINNT
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_START          ".MyoSharedTable."
#define OFFLOAD_MYO_SHARED_TABLE_SECTION_END            ".MyoSharedTable."

#define OFFLOAD_MYO_FPTR_TABLE_SECTION_START            ".MyoFptrTable."
#define OFFLOAD_MYO_FPTR_TABLE_SECTION_END              ".MyoFptrTable."
#endif // TARGET_WINNT

#pragma section(OFFLOAD_MYO_SHARED_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_SHARED_TABLE_SECTION_END, read, write)

#pragma section(OFFLOAD_MYO_FPTR_TABLE_SECTION_START, read, write)
#pragma section(OFFLOAD_MYO_FPTR_TABLE_SECTION_END, read, write)

extern "C" void __offload_myoRegisterTables(
    SharedTableEntry *shared_table,
    FptrTableEntry *fptr_table
);

extern "C" void __offload_myoAcquire(void);
extern "C" void __offload_myoRelease(void);

// temporary workaround for blocking behavior for myoiLibInit/Fini calls
extern "C" void __offload_myoLibInit();
extern "C" void __offload_myoLibFini();

#endif // OFFLOAD_MYO_TARGET_H_INCLUDED
