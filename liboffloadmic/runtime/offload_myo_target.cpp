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


#include "offload_myo_target.h"
#include "offload_target.h"

extern "C" void __cilkrts_cilk_for_32(void*, void*, uint32_t, int32_t);
extern "C" void __cilkrts_cilk_for_64(void*, void*, uint64_t, int32_t);

#pragma weak __cilkrts_cilk_for_32
#pragma weak __cilkrts_cilk_for_64

static void CheckResult(const char *func, MyoError error) {
    if (error != MYO_SUCCESS) {
       LIBOFFLOAD_ERROR(c_myotarget_checkresult, func, error);
        exit(1);
    }
}

static void __offload_myo_shared_table_register(SharedTableEntry *entry)
{
    int entries = 0;
    SharedTableEntry *t_start;

    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    t_start = entry;
    while (t_start->varName != 0) {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_mic_myo_shared,
                              "myo shared entry name = \"%s\" addr = %p\n",
                              t_start->varName, t_start->sharedAddr);
        t_start++;
        entries++;
    }

    if (entries > 0) {
        OFFLOAD_DEBUG_TRACE(3, "myoiMicVarTableRegister(%p, %d)\n", entry,
                            entries);
        CheckResult("myoiMicVarTableRegister",
                    myoiMicVarTableRegister(entry, entries));
    }
}

static void __offload_myo_fptr_table_register(
    FptrTableEntry *entry
)
{
    int entries = 0;
    FptrTableEntry *t_start;

    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, entry);

    t_start = entry;
    while (t_start->funcName != 0) {
        OFFLOAD_DEBUG_TRACE_1(4, 0, c_offload_mic_myo_fptr,
                              "myo fptr entry name = \"%s\" addr = %p\n",
                              t_start->funcName, t_start->funcAddr);
        t_start++;
        entries++;
    }

    if (entries > 0) {
        OFFLOAD_DEBUG_TRACE(3, "myoiTargetFptrTableRegister(%p, %d, 0)\n",
                            entry, entries);
        CheckResult("myoiTargetFptrTableRegister",
                    myoiTargetFptrTableRegister(entry, entries, 0));
    }
}

extern "C" void __offload_myoAcquire(void)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);
    CheckResult("myoAcquire", myoAcquire());
}

extern "C" void __offload_myoRelease(void)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);
    CheckResult("myoRelease", myoRelease());
}

extern "C" void __intel_cilk_for_32_offload_wrapper(void *args_)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    struct S {
        void *M1;
        unsigned int M2;
        unsigned int M3;
        char closure[];
    } *args = (struct S*) args_;

    __cilkrts_cilk_for_32(args->M1, args->closure, args->M2, args->M3);
}

extern "C" void __intel_cilk_for_64_offload_wrapper(void *args_)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    struct S {
        void *M1;
        uint64_t M2;
        uint64_t M3;
        char closure[];
    } *args = (struct S*) args_;

    __cilkrts_cilk_for_64(args->M1, args->closure, args->M2, args->M3);
}

static void __offload_myo_once_init(void)
{
    CheckResult("myoiRemoteFuncRegister",
                myoiRemoteFuncRegister(
                    (MyoiRemoteFuncType) __intel_cilk_for_32_offload_wrapper,
                    "__intel_cilk_for_32_offload"));
    CheckResult("myoiRemoteFuncRegister",
                myoiRemoteFuncRegister(
                    (MyoiRemoteFuncType) __intel_cilk_for_64_offload_wrapper,
                    "__intel_cilk_for_64_offload"));
}

extern "C" void __offload_myoRegisterTables(
    SharedTableEntry *shared_table,
    FptrTableEntry *fptr_table
)
{
    OFFLOAD_DEBUG_TRACE(3, "%s\n", __func__);

    // one time registration of Intel(R) Cilk(TM) language entries
    static pthread_once_t once_control = PTHREAD_ONCE_INIT;
    pthread_once(&once_control, __offload_myo_once_init);

    // register module's tables
    if (shared_table->varName == 0 && fptr_table->funcName == 0) {
        return;
    }

    __offload_myo_shared_table_register(shared_table);
    __offload_myo_fptr_table_register(fptr_table);
}

extern "C" void* _Offload_shared_malloc(size_t size)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%lld)\n", __func__, size);
    return myoSharedMalloc(size);
}

extern "C" void _Offload_shared_free(void *ptr)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, ptr);
    myoSharedFree(ptr);
}

extern "C" void* _Offload_shared_aligned_malloc(size_t size, size_t align)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%lld, %lld)\n", __func__, size, align);
    return myoSharedAlignedMalloc(size, align);
}

extern "C" void _Offload_shared_aligned_free(void *ptr)
{
    OFFLOAD_DEBUG_TRACE(3, "%s(%p)\n", __func__, ptr);
    myoSharedAlignedFree(ptr);
}

// temporary workaround for blocking behavior of myoiLibInit/Fini calls
extern "C" void __offload_myoLibInit()
{
    OFFLOAD_DEBUG_TRACE(3, "%s()\n", __func__);
    CheckResult("myoiLibInit", myoiLibInit(0, 0));
}

extern "C" void __offload_myoLibFini()
{
    OFFLOAD_DEBUG_TRACE(3, "%s()\n", __func__);
    myoiLibFini();
}
