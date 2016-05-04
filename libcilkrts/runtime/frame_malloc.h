/* frame_malloc.h                  -*-C++-*-
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
 * @file frame_malloc.h
 *
 * @brief The frame allocation routines manage memory in a per-worker pool.
 *
 * The name "frame malloc" refers to an earlier implementation of Cilk which
 * allocated frames from the heap using this allocator.
 */

#ifndef INCLUDED_FRAME_MALLOC_DOT_H
#define INCLUDED_FRAME_MALLOC_DOT_H

#include "worker_mutex.h"
#include "rts-common.h"
#include <internal/abi.h>  // __cilkrts_worker

#ifdef __cplusplus
#   include <cstddef>
#else
#   include <stddef.h>
#endif

__CILKRTS_BEGIN_EXTERN_C

/**
 * Number of buckets.  Gives us buckets to hold  64, 128, 256, 512, 1024
 * and 2048 bytes
 */
#define FRAME_MALLOC_NBUCKETS 6

/** Layout of frames when unallocated */
struct free_list {
     /** Pointer to next free frame */
     struct free_list *cdr;
};

/** per-worker memory cache */
struct __cilkrts_frame_cache
{
    /** Mutex to serialize access */
    struct mutex lock;

    /** Linked list of frames */
    struct pool_cons *pool_list;

    /** Low bound of memory in pool */
    char *pool_begin;

    /** High bound of memory in pool */
    char *pool_end;

    /** Global free-list buckets */
    struct free_list *global_free_list[FRAME_MALLOC_NBUCKETS];

    /**
     * How many bytes to obtain at once from the global pool
     * (approximately)
     */
    size_t batch_size;

    /** Garbage-collect a bucket when its potential exceeds the limit */
    size_t potential_limit;

    /** If TRUE, check for memory leaks at the end of execution */
    int check_for_leaks;

    /** Bytes of memory allocated from the OS by the global cache */
    size_t allocated_from_os;

    /** Tracks memory allocated by a chunk that isn't a full bucket size */
    size_t wasted;

    /** Bytes of memory allocated from the global cache */
    size_t allocated_from_global_pool;
};

/**
 * Allocate memory from the per-worker pool. If the size is too large, or
 * if we're given a NULL worker, the memory is allocated using
 * __cilkrts_malloc().
 *
 * @param w The worker to allocate the memory from.
 * @param size The number of bytes to allocate.
 *
 * @return pointer to allocated memory block.
 */
COMMON_PORTABLE
void *__cilkrts_frame_malloc(__cilkrts_worker *w,
                             size_t size) cilk_nothrow;

/**
 * Return memory to the per-worker pool. If the size is too large, or
 * if we're given a NULL worker, the memory is freed using
 * __cilkrts_free().
 *
 * @param w The worker to allocate the memory from.
 * @param p The memory block to be released.
 * @param size The size of the block, in bytes.
 */
COMMON_PORTABLE
void __cilkrts_frame_free(__cilkrts_worker *w,
                          void*  p,
                          size_t size) cilk_nothrow;

/**
 * Destroy the global cache stored in the global state, freeing all memory
 * to the global heap.  Checks whether any memory has been allocated but
 * not freed.
 *
 * @param g The global state.
 */
COMMON_PORTABLE
void __cilkrts_frame_malloc_global_cleanup(global_state_t *g);

/**
 * Initialize a worker's memory cache.  Initially it is empty.
 *
 * @param w The worker who's memory cache is to be initialized.
 */
COMMON_PORTABLE
void __cilkrts_frame_malloc_per_worker_init(__cilkrts_worker *w);

/**
 * If check_for_leaks is set in the global state's memory cache, free any
 * memory in the worker's memory cache.
 *
 * If check_for_leask is not set, nothing happens.
 *
 * @param w The worker who's memory cache is to be cleaned up.
 */
COMMON_PORTABLE
void __cilkrts_frame_malloc_per_worker_cleanup(__cilkrts_worker *w);

/**
 * Round a number of bytes to the size of the smallest bucket that will
 * hold it.  If the size is bigger than the largest bucket, the value is
 * unchanged.
 *
 * @param size Number of bytes to be rounded up to the nearest bucket size.
 *
 * @return The size of the smallest bucket that will hold the specified bytes.
 */
COMMON_PORTABLE
size_t __cilkrts_frame_malloc_roundup(size_t size) cilk_nothrow;

/**
 * Return the number of bytes that can fit into a bucket.
 *
 * Preconditions:
 *  - The index must be in the range 0 - FRAME_MALLOC_NBUCKETS
 *
 * @param bucket Index of the bucket to be sized.
 */
COMMON_PORTABLE
size_t __cilkrts_size_of_bucket(int bucket) cilk_nothrow;

/**
 * Initialize the global memory cache.
 *
 * @param g The global state.
 */
COMMON_PORTABLE
void __cilkrts_frame_malloc_global_init(global_state_t *g);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_FRAME_MALLOC_DOT_H)
