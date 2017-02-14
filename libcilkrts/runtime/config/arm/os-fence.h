/* os.h                  -*-C++-*-
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
 **************************************************************************/

// __atomic_* intrinsics are available since GCC 4.7.
#define HAVE_ATOMIC_INTRINSICS defined(__GNUC__) && \
                               (__GNUC__ * 10 + __GNUC_MINOR__ >= 47)

// GCC before 4.4 does not implement __sync_synchronize properly
#define HAVE_SYNC_INTRINSICS defined(__GNUC__) && \
                             (__GNUC__ * 10 + __GNUC_MINOR__ >= 44)

/*
 * void __cilkrts_fence(void)
 *
 * Executes an MFENCE instruction to serialize all load and store instructions
 * that were issued prior the MFENCE instruction. This serializing operation
 * guarantees that every load and store instruction that precedes the MFENCE
 * instruction is globally visible before any load or store instruction that
 * follows the MFENCE instruction. The MFENCE instruction is ordered with
 * respect to all load and store instructions, other MFENCE instructions, any
 * SFENCE and LFENCE instructions, and any serializing instructions (such as
 * the CPUID instruction).
 */

#if HAVE_ATOMIC_INTRINSICS
#   define __cilkrts_fence() __atomic_thread_fence(__ATOMIC_SEQ_CST)
#elif HAVE_SYNC_INTRINSICS
#   define __cilkrts_fence() __sync_synchronize()
#else
#   define __cilkrts_fence()
// Leaving this code just in case.
//# define __cilkrts_fence() __asm__ __volatile__ ("mcr   p15,0,%[t],c7,c10,4\n" :: [t] "r" (0) : "memory");
#endif
