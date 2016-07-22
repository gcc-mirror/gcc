/* frame_malloc.c                  -*-C-*-
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

#include "frame_malloc.h"
#include "bug.h"
#include "local_state.h"
#include "cilk_malloc.h"

#ifndef __VXWORKS__
#include <memory.h>
#endif

/* #define USE_MMAP 1 */ 
#if USE_MMAP
#define __USE_MISC 1
#include <sys/mman.h>
#include <errno.h>
#endif

// Define to fill the stack frame header with the fill character when pushing
// it on a free list.  Note that this should be #ifdef'd out when checked in!

#ifdef _DEBUG
#define HEADER_FILL_CHAR 0xbf
#endif

// HEADER_FILL_CHAR should not be defined when checked in, so put out a warning
// message if this is a release build

#if defined(NDEBUG) && defined (HEADER_FILL_CHAR)
#pragma message ("Warning: HEADER_FILL_CHAR defined for a release build")
#endif

static void allocate_batch(__cilkrts_worker *w, int bucket, size_t size);

#ifndef _WIN32

const unsigned short __cilkrts_bucket_sizes[FRAME_MALLOC_NBUCKETS] =
{
    64, 128, 256, 512, 1024, 2048
};

#define FRAME_MALLOC_BUCKET_TO_SIZE(bucket) __cilkrts_bucket_sizes[bucket]

/* threshold above which we use slow malloc */
#define FRAME_MALLOC_MAX_SIZE 2048

#else // _WIN32

/* Note that this must match the implementation of framesz_to_bucket in
 * asmilator/layout.ml! */
#define FRAME_MALLOC_BUCKET_TO_SIZE(bucket) ((size_t)(64 << (bucket)))

/* threshold above which we use slow malloc */
#define FRAME_MALLOC_MAX_SIZE                                   \
    FRAME_MALLOC_BUCKET_TO_SIZE(FRAME_MALLOC_NBUCKETS - 1)

#endif // _WIN32

/* utility procedures */
static void push(struct free_list **b, struct free_list *p)
{
#ifdef HEADER_FILL_CHAR
    memset (p, HEADER_FILL_CHAR, FRAME_MALLOC_BUCKET_TO_SIZE(0));
#endif
    /* cons! onto free list */
    p->cdr = *b;
    *b = p;
}

static struct free_list *pop(struct free_list **b)
{
    struct free_list *p = *b;
    if (p) 
        *b = p->cdr;
    return p;
}

/*************************************************************
  global allocator:
*************************************************************/
/* request slightly less than 2^K from the OS, which after malloc
   overhead and alignment should end up filling each VM page almost
   completely.  128 is a guess of the total malloc overhead and cache
   line alignment */
#define FRAME_MALLOC_CHUNK (32 * 1024 - 128)

/** Implements linked list of frames */
struct pool_cons {
    char *p;                /**< This element of the list */
    struct pool_cons *cdr;  /**< Remainder of the list */
};

static void extend_global_pool(global_state_t *g)
{
    /* FIXME: memalign to a cache line? */
    struct pool_cons *c = (struct pool_cons *)__cilkrts_malloc(sizeof(*c));
    g->frame_malloc.pool_begin = 
        (char *)__cilkrts_malloc((size_t)FRAME_MALLOC_CHUNK);
    g->frame_malloc.pool_end = 
        g->frame_malloc.pool_begin + FRAME_MALLOC_CHUNK;
    g->frame_malloc.allocated_from_os += FRAME_MALLOC_CHUNK;
    c->p = g->frame_malloc.pool_begin;
    c->cdr = g->frame_malloc.pool_list;
    g->frame_malloc.pool_list = c;
}

/* the size is already canonicalized at this point */
static struct free_list *global_alloc(global_state_t *g, int bucket)
{
    struct free_list *mem;
    size_t size;

    CILK_ASSERT(bucket < FRAME_MALLOC_NBUCKETS);
    size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
    g->frame_malloc.allocated_from_global_pool += size;

    if (!(mem = pop(&g->frame_malloc.global_free_list[bucket]))) {

        CILK_ASSERT(g->frame_malloc.pool_begin <= g->frame_malloc.pool_end);
        if (g->frame_malloc.pool_begin + size > g->frame_malloc.pool_end) {
            /* We waste the fragment of pool. */
            g->frame_malloc.wasted +=
                g->frame_malloc.pool_end - g->frame_malloc.pool_begin;
            extend_global_pool(g);
        }
        mem = (struct free_list *)g->frame_malloc.pool_begin;
        g->frame_malloc.pool_begin += size;
    }

    return mem;
}

static void global_free(global_state_t *g, void *mem, int bucket)
{
    size_t size;

    CILK_ASSERT(bucket < FRAME_MALLOC_NBUCKETS);
    size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
    g->frame_malloc.allocated_from_global_pool -= size;

    push(&g->frame_malloc.global_free_list[bucket], mem);
}

void __cilkrts_frame_malloc_global_init(global_state_t *g)
{
    int i;

    __cilkrts_mutex_init(&g->frame_malloc.lock); 
    g->frame_malloc.check_for_leaks = 1;
    g->frame_malloc.pool_list = 0;
    g->frame_malloc.pool_begin = 0;
    g->frame_malloc.pool_end = 0;
    g->frame_malloc.batch_size = 8000;
    g->frame_malloc.potential_limit = 4 * g->frame_malloc.batch_size;
    g->frame_malloc.allocated_from_os = 0;
    g->frame_malloc.allocated_from_global_pool = 0;
    g->frame_malloc.wasted = 0;
    for (i = 0; i < FRAME_MALLOC_NBUCKETS; ++i) 
        g->frame_malloc.global_free_list[i] = 0;
}

// Counts how many bytes are in the global free list.
static size_t count_memory_in_global_list(global_state_t *g)
{

    // Count the memory remaining in the global free list.
    size_t size_remaining_in_global_list = 0;
    int i;
    for (i = 0; i < FRAME_MALLOC_NBUCKETS; ++i) {
        struct free_list *p;
        size_t size_in_bucket = 0;
        p = g->frame_malloc.global_free_list[i];

        while (p) {
            size_in_bucket += FRAME_MALLOC_BUCKET_TO_SIZE(i);
            p = p->cdr;
        }
        size_remaining_in_global_list += size_in_bucket;
    }
    return size_remaining_in_global_list;
}


void __cilkrts_frame_malloc_global_cleanup(global_state_t *g)
{
    struct pool_cons *c;

    if (g->frame_malloc.check_for_leaks) {
        size_t memory_in_global_list = count_memory_in_global_list(g);
        // TBD: This check is weak.  Short of memory corruption,
        // I don't see how we have more memory in the free list
        // than allocated from the os.
        // Ideally, we should count the memory in the global free list
        // and check that we have it all.  But I believe the runtime
        // itself also uses some memory, which is not being tracked.
        if (memory_in_global_list > g->frame_malloc.allocated_from_os) {
            __cilkrts_bug("\nError. The Cilk runtime data structures may have been corrupted.\n");
        }
    }
    
    while ((c = g->frame_malloc.pool_list)) {
        g->frame_malloc.pool_list = c->cdr;
        __cilkrts_free(c->p);
        __cilkrts_free(c);
    }

    __cilkrts_mutex_destroy(0, &g->frame_malloc.lock);

    // Check that all the memory moved from the global pool into
    // workers has been returned to the global pool.
    if (g->frame_malloc.check_for_leaks
        && (g->frame_malloc.allocated_from_global_pool != 0))
    {
        __cilkrts_bug("\n"
                      "---------------------------" "\n"
                      "  MEMORY LEAK DETECTED!!!  " "\n"
                      "---------------------------" "\n"
                      "\n"
            );
    }
}

/*************************************************************
  per-worker allocator
*************************************************************/
/* allocate a batch of frames of size SIZE from the global pool and
   store them in the worker's free list */
static void allocate_batch(__cilkrts_worker *w, int bucket, size_t size)
{
    global_state_t *g = w->g;

    __cilkrts_mutex_lock(w, &g->frame_malloc.lock); {
#if USE_MMAP
        char *p = mmap(0, 12288, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
        if (p == MAP_FAILED)
            __cilkrts_bug("mmap failed %d", errno);
        assert(size < 4096);
        assert(p != MAP_FAILED);
        mprotect(p, 4096, PROT_NONE);
        mprotect(p + 8192, 4096, PROT_NONE);
        w->l->bucket_potential[bucket] += size;
        push(&w->l->free_list[bucket], (struct free_list *)(p + 8192 - size));
#else
        size_t bytes_allocated = 0;
        do {
            w->l->bucket_potential[bucket] += size;
            bytes_allocated += size;
            push(&w->l->free_list[bucket], global_alloc(g, bucket));
        } while (bytes_allocated < g->frame_malloc.batch_size);
#endif
    } __cilkrts_mutex_unlock(w, &g->frame_malloc.lock);

}

static void gc_bucket(__cilkrts_worker *w, int bucket, size_t size)
{
    struct free_list *p, *q;
    global_state_t *g = w->g;
    size_t pot = w->l->bucket_potential[bucket];
    size_t newpot;

    /* Keep up to POT/2 elements in the free list.  The cost of
       counting up to POT/2 is amortized against POT. */
    newpot = 0;
    for (newpot = 0, p = w->l->free_list[bucket]; p && 2 * newpot < pot; 
         p = p->cdr, newpot += size)
        ;
    w->l->bucket_potential[bucket] = newpot;

    if (p) {
        /* free the rest of the list.  The cost of grabbing the lock
           is amortized against POT/2; the cost of traversing the rest
           of the list is amortized against the free operation that
           puts the element on the list. */
        __cilkrts_mutex_lock(w, &g->frame_malloc.lock); {
            while ((q = pop(&p->cdr)))
#if USE_MMAP
                munmap((char *)q + size - 8192, 12288);
#else
                global_free(g, q, bucket);
#endif
        } __cilkrts_mutex_unlock(w, &g->frame_malloc.lock);
    }
}

// Free all the memory in this bucket for the specified worker,
// returning it to the global pool's free list.
static void move_bucket_to_global_free_list(__cilkrts_worker *w,
                                            int bucket)
{
    struct free_list *p, *q;
    global_state_t *g = w->g;
    p = w->l->free_list[bucket];
    
    if (p) {
        __cilkrts_mutex_lock(w, &g->frame_malloc.lock); {
            while ((q = pop(&p))) {
#if USE_MMAP
                size_t size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
                munmap((char *)q + size - 8192, 12288);
#else
                global_free(g, q, bucket);
#endif
            }
        } __cilkrts_mutex_unlock(w, &g->frame_malloc.lock);
    }

    // I'm not sure this does anything useful now, since
    // the worker is about to be destroyed. But why not?
    w->l->bucket_potential[bucket] = 0;
}

static int bucket_of_size(size_t size)
{
    int i;

    for (i = 0; i < FRAME_MALLOC_NBUCKETS; ++i)
        if (size <= FRAME_MALLOC_BUCKET_TO_SIZE(i))
            return i;

    CILK_ASSERT(0 /* can't happen */);
    return -1;
}

size_t __cilkrts_frame_malloc_roundup(size_t size)
{
    if (size > FRAME_MALLOC_MAX_SIZE) {
        /* nothing, leave it alone */
    } else {
        int bucket = bucket_of_size(size);
        size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
    }
    return size;
}

size_t __cilkrts_size_of_bucket(int bucket)
{
    CILK_ASSERT(bucket >= 0 && bucket < FRAME_MALLOC_NBUCKETS);
    return FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
}

void *__cilkrts_frame_malloc(__cilkrts_worker *w, size_t size)
{
    int bucket;
    void *mem;

    /* if too large, or if no worker, fall back to __cilkrts_malloc()  */
    if (!w || size > FRAME_MALLOC_MAX_SIZE) {
        NOTE_INTERVAL(w, INTERVAL_FRAME_ALLOC_LARGE);
        return __cilkrts_malloc(size);
    }

    START_INTERVAL(w, INTERVAL_FRAME_ALLOC); {
        bucket = bucket_of_size(size);
        size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);

        while (!(mem = pop(&w->l->free_list[bucket]))) {
            /* get a batch of frames from the global pool */
            START_INTERVAL(w, INTERVAL_FRAME_ALLOC_GLOBAL) {
                allocate_batch(w, bucket, size);
            } STOP_INTERVAL(w, INTERVAL_FRAME_ALLOC_GLOBAL);
        }
    } STOP_INTERVAL(w, INTERVAL_FRAME_ALLOC);

    return mem;
}

void __cilkrts_frame_free(__cilkrts_worker *w, void *p0, size_t size)
{
    int bucket;
    struct free_list *p = (struct free_list *)p0;

    /* if too large, or if no worker, fall back to __cilkrts_free()  */
    if (!w || size > FRAME_MALLOC_MAX_SIZE) {
        NOTE_INTERVAL(w, INTERVAL_FRAME_FREE_LARGE);
        __cilkrts_free(p);
        return;
    }

#if CILK_LIB_DEBUG
    *(volatile long *)w;
#endif

    START_INTERVAL(w, INTERVAL_FRAME_FREE); {
        bucket = bucket_of_size(size);
        size = FRAME_MALLOC_BUCKET_TO_SIZE(bucket);
        w->l->bucket_potential[bucket] += size;
        push(&w->l->free_list[bucket], p);
        if (w->l->bucket_potential[bucket] >
            w->g->frame_malloc.potential_limit) {
            START_INTERVAL(w, INTERVAL_FRAME_FREE_GLOBAL) {
                gc_bucket(w, bucket, size);
            } STOP_INTERVAL(w, INTERVAL_FRAME_FREE_GLOBAL);
        }
    } STOP_INTERVAL(w, INTERVAL_FRAME_FREE);
}

void __cilkrts_frame_malloc_per_worker_init(__cilkrts_worker *w)
{
    int i;
    local_state *l = w->l;

    for (i = 0; i < FRAME_MALLOC_NBUCKETS; ++i) {
        l->free_list[i] = 0;
        l->bucket_potential[i] = 0;
    }
}

void __cilkrts_frame_malloc_per_worker_cleanup(__cilkrts_worker *w)
{
    int i;
    // Move memory to the global pool.  This operation
    // ensures the memory does not become unreachable / leak
    // when the worker is destroyed.
    for (i = 0; i < FRAME_MALLOC_NBUCKETS; ++i) {
        move_bucket_to_global_free_list(w, i);
    }
}

/*
  Local Variables: **
  c-file-style:"bsd" **
  c-basic-offset:4 **
  indent-tabs-mode:nil **
  End: **
*/
