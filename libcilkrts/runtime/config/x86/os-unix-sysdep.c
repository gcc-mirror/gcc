/* os-unix-sysdep.c                  -*-C-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2009-2016, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *  
 *  *********************************************************************
 *  
 *  PLEASE NOTE: This file is a downstream copy of a file mainitained in
 *  a repository at cilkplus.org. Changes made to this file that are not
 *  submitted through the contribution process detailed at
 *  http://www.cilkplus.org/submit-cilk-contribution will be lost the next
 *  time that a new version is released. Changes only submitted to the
 *  GNU compiler collection or posted to the git repository at
 *  https://bitbucket.org/intelcilkruntime/intel-cilk-runtime.git are
 *  not tracked.
 *  
 *  We welcome your contributions to this open source project. Thank you
 *  for your assistance in helping us improve Cilk Plus.
 *************************************************************************
 *
 * This file contains system-specific code for Unix systems
 */

#include "os.h"
#include "sysdep.h"
#include <internal/abi.h>

// On x86 processors (but not MIC processors), the compiler generated code to
// save the FP state (rounding mode and the like) before calling setjmp.  We
// will need to restore that state when we resume.
#ifndef __MIC__
# if defined(__i386__) || defined(__x86_64)
#   define RESTORE_X86_FP_STATE
# endif // defined(__i386__) || defined(__x86_64)
#endif  // __MIC__

/* timer support */
COMMON_SYSDEP unsigned long long __cilkrts_getticks(void)
{
#if defined __i386__ || defined __x86_64
    unsigned a, d; 
    __asm__ volatile("rdtsc" : "=a" (a), "=d" (d)); 
    return ((unsigned long long)a) | (((unsigned long long)d) << 32); 
#else
#   warning "unimplemented cycle counter"
    return 0;
#endif
}

COMMON_SYSDEP void __cilkrts_short_pause(void)
{
#if __ICC >= 1110
#   if __MIC__ || __MIC2__
    _mm_delay_32(16); // stall for 16 cycles
#   else
    _mm_pause();
#   endif
#elif defined __i386__ || defined __x86_64
    __asm__("pause");
#else
#   warning __cilkrts_short_pause empty
#endif
}

COMMON_SYSDEP int __cilkrts_xchg(volatile int *ptr, int x)
{
#if defined __i386__ || defined __x86_64
    /* asm statement here works around icc bugs */
    __asm__("xchgl %0,%a1" :"=r" (x) : "r" (ptr), "0" (x) :"memory");
#else
    x = __sync_lock_test_and_set(ptr, x);
#endif
    return x;
}

/*
 * The Intel compiler distribution assumes newer CPUs and doesn't yet support
 * the __builtin_cpu_supports intrinsic added by GCC 4.8, so just return 1 in
 * that environment.
 *
 * This declaration should generate an error when the Intel compiler adds
 * supprt for the intrinsic.
 */
#if defined(__INTEL_COMPILER) || defined(__clang__)
static inline int __builtin_cpu_supports(const char *feature)
{
    return 1;
}
#endif

/*
 * Restore the floating point state that is stored in a stack frame at each
 * spawn.  This should be called each time a frame is resumed.
 *
 * Only valid for IA32 and Intel64 processors.
 */
void restore_x86_fp_state (__cilkrts_stack_frame *sf) {
#ifdef RESTORE_X86_FP_STATE
    if (__builtin_cpu_supports("sse"))
    {
        __asm__ ("ldmxcsr %0"
                 :
                 : "m" (sf->mxcsr));
    }
    __asm__ ("fnclex\n\t"
             "fldcw %0"
             :
             : "m" (sf->fpcsr));
#endif
}

void sysdep_save_fp_ctrl_state(__cilkrts_stack_frame *sf)
{
// If we're not going to restore, don't bother saving it
#ifdef RESTORE_X86_FP_STATE
    if (CILK_FRAME_VERSION_VALUE(sf->flags) >= 1)
    {
        if (__builtin_cpu_supports("sse"))
        {
            __asm__ ("stmxcsr %0" : "=m" (sf->mxcsr));
        }
        __asm__ ("fnstcw %0" : "=m" (sf->fpcsr));
    }
#endif
}

