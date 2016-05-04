/* cilk_malloc.h                  -*-C++-*-
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

/**
 * @file cilk_malloc.h
 *
 * @brief Provides replacement memory allocation functions to allocate
 * (and free) memory on cache line boundaries, if supported by the OS.
 *
 * If aligned memory functions are not provided by the OS, the calls just
 * pass through to the standard memory allocation functions.
 */

#ifndef INCLUDED_CILK_MALLOC_DOT_H
#define INCLUDED_CILK_MALLOC_DOT_H

#include <cilk/common.h>
#include <stddef.h>

#include "rts-common.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * malloc replacement function to allocate memory aligned on a cache line
 * boundary if aligned memory allocations are supported by the OS.
 *
 * @param size Number of bytes to allocate.
 *
 * @return pointer to memory block allocated, or NULL if unsuccessful.
 */
COMMON_PORTABLE void *__cilkrts_malloc(size_t size);

/**
 * realloc replacement function to allocate memory aligned on a cache line
 * boundary if aligned memory allocations are supported by the OS.
 *
 * @param ptr Block to be reallocated.
 * @param size Number of bytes to allocate.
 *
 * @return pointer to memory block allocated, or NULL if unsuccessful.
 */
COMMON_PORTABLE void *__cilkrts_realloc(void *ptr, size_t size);

/**
 * free replacement function to deallocate memory aligned on a cache line
 * boundary if aligned memory allocations are supported by the OS.
 *
 * @param ptr Block to be freed.
 */
COMMON_PORTABLE void __cilkrts_free(void *ptr);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_CILK_MALLOC_DOT_H)
