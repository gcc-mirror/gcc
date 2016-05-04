/* rts-common.h                  -*-C++-*-
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

#ifndef INCLUDED_RTS_COMMON_DOT_H
#define INCLUDED_RTS_COMMON_DOT_H

/* Abbreviations API functions returning different types.  By using these
 * abbreviations instead of using CILK_API(ret) directly, etags and other
 * tools can more easily recognize function signatures.
 */
#define CILK_API_VOID          CILK_API(void)
#define CILK_API_VOID_PTR      CILK_API(void*)
#define CILK_API_INT           CILK_API(int)
#define CILK_API_SIZET         CILK_API(size_t)
#define CILK_API_TBB_RETCODE   CILK_API(__cilk_tbb_retcode)
#define CILK_API_PEDIGREE      CILK_API(__cilkrts_pedigree) 

/* Abbreviations ABI functions returning different types.  By using these
 * abbreviations instead of using CILK_ABI(ret) directly, etags and other
 * tools can more easily recognize function signatures.
 */
#define CILK_ABI_VOID        CILK_ABI(void)
#define CILK_ABI_WORKER_PTR  CILK_ABI(__cilkrts_worker_ptr)
#define CILK_ABI_THROWS_VOID CILK_ABI_THROWS(void)

/* documentation aid to identify portable vs. nonportable
   parts of the runtime.  See README for definitions. */
#define COMMON_PORTABLE
#define COMMON_SYSDEP
#define NON_COMMON

#if !(defined __GNUC__ || defined __ICC) || defined(_WRS_KERNEL)
#   define __builtin_expect(a_, b_) (a_)
#endif

#ifdef __cplusplus
#   define cilk_nothrow throw()
#else
#   define cilk_nothrow /*empty in C*/
#endif

#ifdef __GNUC__
#   define NORETURN void __attribute__((noreturn))
#else
#   define NORETURN void __declspec(noreturn)
#endif

#ifdef __GNUC__
#   define NOINLINE __attribute__((noinline))
#else
#   define NOINLINE __declspec(noinline)
#endif

#ifndef __GNUC__
#   define __attribute__(X)
#endif

/* Microsoft CL accepts "inline" for C++, but not for C.  It accepts 
 * __inline for both.  Intel ICL accepts inline for C of /Qstd=c99
 * is set.  The Cilk runtime is assumed to be compiled with /Qstd=c99
 */
#if defined(_MSC_VER) && ! defined(__INTEL_COMPILER)
#   error define inline
#   define inline __inline
#endif

/* Compilers that build the Cilk runtime are assumed to know about zero-cost
 * intrinsics (__notify_intrinsic()).  For those that don't, #undef the
 * following definition:
 */
#define ENABLE_NOTIFY_ZC_INTRINSIC 1

#if defined(__INTEL_COMPILER)
/* The notify intrinsic was introduced in ICC 12.0. */
#   if __INTEL_COMPILER <= 1200
#       undef ENABLE_NOTIFY_ZC_INTRINSIC
#   endif
#elif defined(__VXWORKS__)
#   undef ENABLE_NOTIFY_ZC_INTRINSIC
#elif defined(__GNUC__)
#   // GCC doesn't support the notify intrinsic as of 4.9
#   undef ENABLE_NOTIFY_ZC_INTRINSIC
#elif defined(__clang__)
#   if !defined(__has_extension) || !__has_extension(notify_zc_intrinsic)
#      undef ENABLE_NOTIFY_ZC_INTRINSIC
#   endif
#elif ! (defined(__x86_64__) || defined(__i386) \
         || defined(_M_X64) || defined(_M_IX86))
// __notify_zc_intrinsic currently supported only for intel architecture
#   undef ENABLE_NOTIFY_ZC_INTRINSIC
#endif

// If ENABLE_NOTIFY_ZC_INTRINSIC is defined, use __notify_zc_intrisic
#ifdef ENABLE_NOTIFY_ZC_INTRINSIC
#   define NOTIFY_ZC_INTRINSIC(annotation, data) \
    __notify_zc_intrinsic(annotation, data)
#else
#   define NOTIFY_ZC_INTRINSIC(annotation, data)
#endif

#endif // ! defined(INCLUDED_RTS_COMMON_DOT_H)
