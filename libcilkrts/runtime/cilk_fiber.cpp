/* cilk_fiber.cpp                  -*-C++-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2012-2016, Intel Corporation
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

/* Implementations of non-platform-specific aspects of cilk_fiber, especially
 * the cilk_fiber_pool interface.
 */
#include "cilk_fiber.h"
#ifdef _WIN32
#   include "cilk_fiber-win.h"
#else
#   include "cilk_fiber-unix.h"
#endif
#include "cilk_malloc.h"
#include "bug.h"
#include <new>

#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "sysdep.h"


extern "C" {

inline int cilk_fiber_pool_sanity_check(cilk_fiber_pool *pool, const char* desc)
{
    int errors = 0;
#if FIBER_DEBUG >= 1    
    if ((NULL != pool) && pool->total > 0) {

        // Root pool should not allocate more fibers than alloc_max
        errors += ((pool->parent == NULL) &&
                   (pool->total > pool->alloc_max));
        errors += (pool->total > pool->high_water);

        if (errors) {
            fprintf(stderr, "ERROR at %s: pool=%p has max_size=%u, total=%d, high_water=%d\n",
                    desc,
                    pool, pool->max_size, pool->total, pool->high_water);
        }
    }
#endif
    return (errors == 0);
}

inline void increment_pool_total(cilk_fiber_pool* pool)
{
    ++pool->total;
    if (pool->high_water < pool->total)
        pool->high_water = pool->total;
}

inline void decrement_pool_total(cilk_fiber_pool* pool, int fibers_freed)
{
    pool->total -= fibers_freed;
}


/**
 * @brief Free fibers from this pool until we have at most @c
 * num_to_keep fibers remaining, and then put a fiber back.
 *
 * @pre   We do not hold @c pool->lock 
 * @post  After completion, we do not hold @c pool->lock
 */
static void cilk_fiber_pool_free_fibers_from_pool(cilk_fiber_pool* pool,
                                                  unsigned num_to_keep,
                                                  cilk_fiber* fiber_to_return)
{
    // Free our own fibers, until we fall below our desired threshold.
    // Each iteration of this loop proceeds in the following stages:
    //   1.  Acquire the pool lock,
    //   2.  Grabs up to B fibers from the pool, stores them into a buffer.
    //   3.  Check if pool is empty enough.  If yes, put the last fiber back,
    //       and remember that we should quit.
    //   4.  Release the pool lock, and actually free any buffered fibers.
    //   5.  Check if we are done and should exit the loop.  Otherwise, try again.
    // 
    const bool need_lock = pool->lock;
    bool last_fiber_returned = false;
    
    do {
        const int B = 10;   // Pull at most this many fibers from the
                            // parent for one lock acquisition.  Make
                            // this value large enough to amortize
                            // against the cost of acquiring and
                            // releasing the lock.
        int num_to_free = 0;
        cilk_fiber* fibers_to_free[B];

        // Stage 1: Grab the lock.
        if (need_lock) {
            spin_mutex_lock(pool->lock);
        }
        
        // Stage 2: Grab up to B fibers to free.
        int fibers_freed = 0;
        while ((pool->size > num_to_keep) && (num_to_free < B)) {
            fibers_to_free[num_to_free++] = pool->fibers[--pool->size];
            fibers_freed++;
        }
        decrement_pool_total(pool, fibers_freed);

        // Stage 3.  Pool is below threshold.  Put extra fiber back.
        if (pool->size <= num_to_keep) {
            // Put the last fiber back into the pool.
            if (fiber_to_return) {
                CILK_ASSERT(pool->size < pool->max_size);
                pool->fibers[pool->size] = fiber_to_return;
                pool->size++;
            }
            last_fiber_returned = true;
        }
        
        // Stage 4: Release the lock, and actually free any fibers
        // buffered.
        if (need_lock) {
            spin_mutex_unlock(pool->lock);
        }

        for (int i = 0; i < num_to_free; ++i) {
            fibers_to_free[i]->deallocate_to_heap();
        }
        
    } while (!last_fiber_returned);
}


/******************************************************************
 * TBD: We want to simplify / rework the logic for allocating and
 * deallocating fibers, so that they are hopefully simpler and work
 * more elegantly for more than two levels.
 ******************************************************************/

/**
 * @brief Transfer fibers from @c pool to @c pool->parent.
 *
 * @pre   Must hold @c pool->lock if it exists.
 * @post  After completion, some number of fibers
 *        have been moved from this pool to the parent.
 *        The lock @c pool->lock is still held.
 *
 * TBD: Do we wish to guarantee that the lock has never been
 * released?  It may depend on the implementation...
 */
static void cilk_fiber_pool_move_fibers_to_parent_pool(cilk_fiber_pool* pool,
                                                       unsigned num_to_keep)
{
    // ASSERT: We should hold the lock on pool (if it has one).
    CILK_ASSERT(pool->parent);
    cilk_fiber_pool* parent_pool = pool->parent;

    // Move fibers from our pool to the parent until we either run out
    // of space in the parent, or hit our threshold.
    //
    // This operation must be done while holding the parent lock.

    // If the parent pool appears to be full, just return early.
    if (parent_pool->size >= parent_pool->max_size)
        return;

    spin_mutex_lock(pool->parent->lock);
    while ((parent_pool->size < parent_pool->max_size) &&
           (pool->size > num_to_keep)) {
        parent_pool->fibers[parent_pool->size++] =
            pool->fibers[--pool->size];
    }

    // If the child pool has deallocated more than fibers to the heap
    // than it has allocated, then transfer this "surplus" to the
    // parent, so that the parent is free to allocate more from the
    // heap.
    // 
    // This transfer means that the total in the parent can
    // temporarily go negative.
    if (pool->total < 0) {
        // Reduce parent total by the surplus we have in the local
        // pool.
        parent_pool->total += pool->total;
        pool->total = 0;
    }

    spin_mutex_unlock(pool->parent->lock);
}
    
void cilk_fiber_pool_init(cilk_fiber_pool* pool,
                          cilk_fiber_pool* parent,
                          size_t           stack_size,
                          unsigned         buffer_size,
                          int              alloc_max,
                          int              is_shared)
{
#if FIBER_DEBUG >= 1    
    fprintf(stderr, "fiber_pool_init, pool=%p, parent=%p, alloc_max=%u\n",
            pool, parent, alloc_max);
#endif

    pool->lock       = (is_shared ? spin_mutex_create() : NULL);
    pool->parent     = parent;
    pool->stack_size = stack_size;
    pool->max_size   = buffer_size;
    pool->size       = 0;
    pool->total      = 0;
    pool->high_water = 0;
    pool->alloc_max  = alloc_max;
    pool->fibers     =
        (cilk_fiber**) __cilkrts_malloc(buffer_size * sizeof(cilk_fiber*));
    CILK_ASSERT(NULL != pool->fibers);

#ifdef __MIC__
#define PREALLOCATE_FIBERS
#endif
    
#ifdef PREALLOCATE_FIBERS
    // Pre-allocate 1/4 of fibers in the pools ahead of time.  This
    // value is somewhat arbitrary.  It was chosen to be less than the
    // threshold (of about 3/4) of fibers to keep in the pool when
    // transferring fibers to the parent.
    
    int pre_allocate_count = buffer_size/4;
    for (pool->size = 0; pool->size < pre_allocate_count; pool->size++) {
        pool->fibers[pool->size] = cilk_fiber::allocate_from_heap(pool->stack_size);
    }
#endif
}


void cilk_fiber_pool_set_fiber_limit(cilk_fiber_pool* root_pool,
                                     unsigned max_fibers_to_allocate)
{
    // Should only set limit on root pool, not children.
    CILK_ASSERT(NULL == root_pool->parent);
    root_pool->alloc_max = max_fibers_to_allocate;
}
                                   
void cilk_fiber_pool_destroy(cilk_fiber_pool* pool)
{
    CILK_ASSERT(cilk_fiber_pool_sanity_check(pool, "pool_destroy"));

    // Lock my own pool, if I need to.
    if (pool->lock) {
        spin_mutex_lock(pool->lock);
    }

    // Give any remaining fibers to parent pool.
    if (pool->parent) {
        cilk_fiber_pool_move_fibers_to_parent_pool(pool, 0);
    }

    // Unlock pool.
    if (pool->lock) {
        spin_mutex_unlock(pool->lock);
    }

    // If I have any left in my pool, just free them myself.
    // This method may acquire the pool lock.
    cilk_fiber_pool_free_fibers_from_pool(pool, 0, NULL);

    // Destroy the lock if there is one.
    if (pool->lock) {
        spin_mutex_destroy(pool->lock);
    }
    __cilkrts_free(pool->fibers);
}


cilk_fiber* cilk_fiber_allocate(cilk_fiber_pool* pool)
{
    CILK_ASSERT(cilk_fiber_pool_sanity_check(pool, "allocate"));
    return cilk_fiber::allocate(pool);
}

cilk_fiber* cilk_fiber_allocate_from_heap(size_t stack_size)
{
    return cilk_fiber::allocate_from_heap(stack_size);
}

void cilk_fiber_reset_state(cilk_fiber* fiber, cilk_fiber_proc start_proc) 
{
    fiber->reset_state(start_proc);
}

int cilk_fiber_remove_reference(cilk_fiber *fiber, cilk_fiber_pool *pool)
{
    return fiber->remove_reference(pool);
}

cilk_fiber* cilk_fiber_allocate_from_thread()
{
    return cilk_fiber::allocate_from_thread();
}

int cilk_fiber_deallocate_from_thread(cilk_fiber *fiber)
{
    return fiber->deallocate_from_thread();
}

int cilk_fiber_remove_reference_from_thread(cilk_fiber *fiber)
{
    return fiber->remove_reference_from_thread();
}

int cilk_fiber_is_allocated_from_thread(cilk_fiber *fiber)
{
    return fiber->is_allocated_from_thread();
}

#if SUPPORT_GET_CURRENT_FIBER
cilk_fiber* cilk_fiber_get_current_fiber(void)
{
    return cilk_fiber::get_current_fiber();
}
#endif

void cilk_fiber_suspend_self_and_resume_other(cilk_fiber* self,
                                              cilk_fiber* other)
{
    self->suspend_self_and_resume_other(other);
}


void cilk_fiber::reset_state(cilk_fiber_proc start_proc)
{
    // Setup the fiber and return.
    this->m_start_proc = start_proc;
    
    CILK_ASSERT(!this->is_resumable());
    CILK_ASSERT(NULL == this->m_pending_remove_ref);
    CILK_ASSERT(NULL == this->m_pending_pool);
}

NORETURN
cilk_fiber_remove_reference_from_self_and_resume_other(cilk_fiber*      self,
                                                       cilk_fiber_pool* self_pool,
                                                       cilk_fiber*      other)
{
#if FIBER_DEBUG >= 3
    __cilkrts_worker* w = __cilkrts_get_tls_worker();
    fprintf(stderr, "W=%d: cilk_fiber_deactivate_self_and_resume_other: self=%p, other=%p\n",
            w->self,
            self, other);
#endif
    CILK_ASSERT(cilk_fiber_pool_sanity_check(self_pool, "remove_reference_from_self_resume_other"));
    self->remove_reference_from_self_and_resume_other(self_pool, other);
    
    // We should never return here. 
}

void cilk_fiber_set_post_switch_proc(cilk_fiber *self,
                                     cilk_fiber_proc post_switch_proc)
{
    self->set_post_switch_proc(post_switch_proc);
}

void cilk_fiber_invoke_tbb_stack_op(cilk_fiber* fiber,
                                    __cilk_tbb_stack_op op)
{
    fiber->invoke_tbb_stack_op(op);
}

cilk_fiber_data* cilk_fiber_get_data(cilk_fiber* fiber)
{
    return fiber->get_data();

    /// TBD: Change this code to "return (cilk_fiber_data*)fiber;"
    //       plus a static assert, so that this function is 
    //       more easily inlined by the compiler.
}

int cilk_fiber_is_resumable(cilk_fiber *fiber)
{
    return fiber->is_resumable();
}

char* cilk_fiber_get_stack_base(cilk_fiber *fiber)
{
    return fiber->get_stack_base();
}


#if defined(_WIN32) && 0 // Only works on Windows.  Disable debugging for now.
#define DBG_STACK_OPS(_fmt, ...) __cilkrts_dbgprintf(_fmt, __VA_ARGS__)
#else
#define DBG_STACK_OPS(_fmt, ...)
#endif

void cilk_fiber_set_stack_op(cilk_fiber *fiber,
                             __cilk_tbb_stack_op_thunk o)
{
    cilk_fiber_data *fdata = cilk_fiber_get_data(fiber);
    DBG_STACK_OPS ("cilk_fiber_set_stack_op - cilk_fiber %p, routine: %p, data: %p\n",
                   fiber,
                   o.routine,
                   o.data);
    fdata->stack_op_routine = o.routine;
    fdata->stack_op_data = o.data;
}

#if 0    // Debugging function
static
const char *NameStackOp (enum __cilk_tbb_stack_op op)
{
    switch(op)
    {
        case CILK_TBB_STACK_ORPHAN: return "CILK_TBB_STACK_ORPHAN";
        case CILK_TBB_STACK_ADOPT: return "CILK_TBB_STACK_ADOPT";
        case CILK_TBB_STACK_RELEASE: return "CILK_TBB_STACK_RELEASE";
        default: return "Unknown";
    }
}
#endif

/*
 * Save TBB interop information for an unbound thread.  It will get picked
 * up when the thread is bound to the runtime.
 */
void cilk_fiber_tbb_interop_save_stack_op_info(__cilk_tbb_stack_op_thunk o)
{
    __cilk_tbb_stack_op_thunk *saved_thunk =
        __cilkrts_get_tls_tbb_interop();

    DBG_STACK_OPS("Calling save_stack_op; o.routine=%p, o.data=%p, saved_thunk=%p\n",
                  o.routine, o.data, saved_thunk);

    // If there is not already space allocated, allocate some.
    if (NULL == saved_thunk) {
        saved_thunk = (__cilk_tbb_stack_op_thunk*)
            __cilkrts_malloc(sizeof(__cilk_tbb_stack_op_thunk));
        __cilkrts_set_tls_tbb_interop(saved_thunk);
    }

    *saved_thunk = o;

    DBG_STACK_OPS ("Unbound Thread %04x: tbb_interop_save_stack_op_info - saved info\n",
                   cilkos_get_current_thread_id());
}

/*
 * Save TBB interop information from the cilk_fiber.  It will get picked
 * up when the thread is bound to the runtime next time.
 */
void cilk_fiber_tbb_interop_save_info_from_stack(cilk_fiber *fiber)
{
    __cilk_tbb_stack_op_thunk *saved_thunk;
    cilk_fiber_data* fdata;

    if (NULL == fiber)
        return;

    fdata = cilk_fiber_get_data(fiber);
    // If there is no TBB interop data, just return
    if (NULL == fdata->stack_op_routine)
        return;
    
    saved_thunk = __cilkrts_get_tls_tbb_interop();

    // If there is not already space allocated, allocate some.
    if (NULL == saved_thunk) {
        saved_thunk = (__cilk_tbb_stack_op_thunk*)
            __cilkrts_malloc(sizeof(__cilk_tbb_stack_op_thunk));
        __cilkrts_set_tls_tbb_interop(saved_thunk);
    }

    saved_thunk->routine = fdata->stack_op_routine;
    saved_thunk->data = fdata->stack_op_data;
}

/*
 * If there's TBB interop information that was saved before the thread was
 * bound, apply it now
 */
void cilk_fiber_tbb_interop_use_saved_stack_op_info(cilk_fiber* fiber)
{
    __cilk_tbb_stack_op_thunk *saved_thunk =
        __cilkrts_get_tls_tbb_interop();

    CILK_ASSERT(fiber);
    // If we haven't allocated a TBB interop index, we don't have any saved info
    if (NULL == saved_thunk) {
        DBG_STACK_OPS ("cilk_fiber %p: tbb_interop_use_saved_stack_op_info - no saved info\n",
                       fiber);
        return;
    }

    DBG_STACK_OPS ("cilk_fiber %p: tbb_interop_use_saved_stack_op_info - using saved info\n",
                   fiber);

     // Associate the saved info with the __cilkrts_stack
    cilk_fiber_set_stack_op(fiber, *saved_thunk);
    
    // Free the saved data.  We'll save it again if needed when the code
    // returns from the initial function
    cilk_fiber_tbb_interop_free_stack_op_info();
}

/*
 * Free saved TBB interop memory.  Should only be called when the thread is
 * not bound.
 */
void cilk_fiber_tbb_interop_free_stack_op_info(void)
{
    __cilk_tbb_stack_op_thunk *saved_thunk =
        __cilkrts_get_tls_tbb_interop();

    // If we haven't allocated a TBB interop index, we don't have any saved info
    if (NULL == saved_thunk)
        return;

    DBG_STACK_OPS ("tbb_interop_free_stack_op_info - freeing saved info\n");

    // Free the memory and wipe out the TLS value
    __cilkrts_free(saved_thunk);
    __cilkrts_set_tls_tbb_interop(NULL);
}



#if NEED_FIBER_REF_COUNTS
int cilk_fiber_has_references(cilk_fiber *fiber) 
{
    return (fiber->get_ref_count() > 0);
}

int cilk_fiber_get_ref_count(cilk_fiber *fiber)
{
    return fiber->get_ref_count();
}

void cilk_fiber_add_reference(cilk_fiber *fiber)
{
    fiber->inc_ref_count();
}
#endif // NEED_FIBER_REF_COUNTS


} // End extern "C"


cilk_fiber_sysdep* cilk_fiber::sysdep()
{
    return static_cast<cilk_fiber_sysdep*>(this);
}


cilk_fiber::cilk_fiber()
    : m_start_proc(NULL)
    , m_post_switch_proc(NULL)
    , m_pending_remove_ref(NULL)
    , m_pending_pool(NULL)
    , m_flags(0)
{
    // Clear cilk_fiber_data base-class data members
    std::memset((cilk_fiber_data*) this, 0, sizeof(cilk_fiber_data));

    // cilk_fiber data members
    init_ref_count(0);
}

cilk_fiber::cilk_fiber(std::size_t stack_size)
{
    *this = cilk_fiber();  // A delegating constructor would be nice here
    this->stack_size = stack_size;
}

cilk_fiber::~cilk_fiber() 
{
    // Empty destructor.
}


char* cilk_fiber::get_stack_base()
{
    return this->sysdep()->get_stack_base_sysdep();
}

cilk_fiber* cilk_fiber::allocate_from_heap(std::size_t stack_size)
{
    // Case 1: pool is NULL. create a new fiber from the heap
    // No need for locks here.
    cilk_fiber_sysdep* ret =
        (cilk_fiber_sysdep*) __cilkrts_malloc(sizeof(cilk_fiber_sysdep));

    // Error condition. If we failed to allocate a fiber from the
    // heap, we are in trouble though...
    if (!ret)
        return NULL;

    ::new(ret) cilk_fiber_sysdep(stack_size);

    CILK_ASSERT(0 == ret->m_flags);
    CILK_ASSERT(NULL == ret->m_pending_remove_ref);
    CILK_ASSERT(NULL == ret->m_pending_pool);
    ret->init_ref_count(1);
    return ret;
}


#if USE_FIBER_TRY_ALLOCATE_FROM_POOL
/**
 * Helper method: try to allocate a fiber from this pool or its
 * ancestors without going to the OS / heap.
 *
 * Returns allocated pool, or NULL if no pool is found.
 *
 * If pool contains a suitable fiber. Return it.  Otherwise, try to
 * recursively grab a fiber from the parent pool, if there is one.
 *
 * This method will not allocate a fiber from the heap.
 *
 * This method could be written either recursively or iteratively.
 * It probably does not matter which one we do.
 *
 * @note This method is compiled, but may not be used unless the
 * USE_FIBER_TRY_ALLOCATE_FROM_POOL switch is set.
 */
cilk_fiber* cilk_fiber::try_allocate_from_pool_recursive(cilk_fiber_pool* pool)
{
    cilk_fiber* ret = NULL;

    if (pool->size > 0) {
        // Try to get the lock.
        if (pool->lock) {
            // For some reason, it seems to be better to just block on the parent
            // pool lock, instead of using a try-lock?
#define USE_TRY_LOCK_IN_FAST_ALLOCATE 0
#if USE_TRY_LOCK_IN_FAST_ALLOCATE
            int got_lock = spin_mutex_trylock(pool->lock);
            if (!got_lock) {
                // If we fail, skip to the parent.
                if (pool->parent) {
                    return try_allocate_from_pool_recursive(pool->parent);
                }
            }
#else
            spin_mutex_lock(pool->lock);
#endif
        }

        // Check in the pool if we have the lock.
        if (pool->size > 0) {
            ret = pool->fibers[--pool->size];
        }

        // Release the lock once we are done updating pool fields.
        if (pool->lock) {
            spin_mutex_unlock(pool->lock);
        }
    }

    if ((!ret) && (pool->parent)) {
        return try_allocate_from_pool_recursive(pool->parent);
    }

    if (ret) {
        // When we pull a fiber out of the pool, set its reference
        // count before we return it.
        ret->init_ref_count(1);
    }
    return ret;
}
#endif // USE_FIBER_TRY_ALLOCATE_FROM_POOL


cilk_fiber* cilk_fiber::allocate(cilk_fiber_pool* pool)
{
    // Pool should not be NULL in this method.  But I'm not going to
    // actually assert it, because we are likely to seg fault anyway
    // if it is.
    // CILK_ASSERT(NULL != pool);

    cilk_fiber *ret = NULL;

#if USE_FIBER_TRY_ALLOCATE_FROM_POOL
    // "Fast" path, which doesn't go to the heap or OS until checking
    // the ancestors first.
    ret = try_allocate_from_pool_recursive(pool);
    if (ret)
        return ret;
#endif

    // If we don't get anything from the "fast path", then go through
    // a slower path to look for a fiber.
    //
    //  1. Lock the pool if it is shared.
    //  2. Look in our local pool.  If we find one, release the lock
    //     and quit searching.
    //  3. Otherwise, check whether we can allocate from heap.
    //  4. Release the lock if it was acquired.
    //  5. Try to allocate from the heap, if step 3 said we could.
    //     If we find a fiber, then quit searching.
    //  6. If none of these steps work, just recursively try again
    //     from the parent.

    // 1. Lock the pool if it is shared.
    if (pool->lock) {
        spin_mutex_lock(pool->lock);
    }

    // 2. Look in local pool.
    if (pool->size > 0) {
        ret = pool->fibers[--pool->size];
        if (ret) {
            // If we found one, release the lock once we are
            // done updating pool fields, and break out of the
            // loop.
            if (pool->lock) {
                spin_mutex_unlock(pool->lock);
            }

            // When we pull a fiber out of the pool, set its reference
            // count just in case.
            ret->init_ref_count(1);
            return ret;
        }
    }

    // 3. Check whether we can allocate from the heap.
    bool can_allocate_from_heap = false;
    if (pool->total < pool->alloc_max) {
        // Track that we are allocating a new fiber from the
        // heap, originating from this pool.
        // This increment may be undone if we happen to fail to
        // allocate from the heap.
        increment_pool_total(pool);
        can_allocate_from_heap = true;
    }

    // 4. Unlock the pool, and then allocate from the heap.
    if (pool->lock) {
        spin_mutex_unlock(pool->lock);
    }

    // 5. Actually try to allocate from the heap / OS.
    if (can_allocate_from_heap) {
        ret = allocate_from_heap(pool->stack_size);
        // If we got something from the heap, just return it.
        if (ret) {
            return ret;
        }

        // Otherwise, we failed in our attempt to allocate a
        // fiber from the heap.  Grab the lock and decrement
        // the total again.
        if (pool->lock) {
            spin_mutex_lock(pool->lock);
        }
        decrement_pool_total(pool, 1);
        if (pool->lock) {
            spin_mutex_unlock(pool->lock);
        }
    }

    // 6. If we get here, then searching this pool failed.  Go search
    // the parent instead if we have one.
    if (pool->parent) {
        return allocate(pool->parent);
    }
    
    return ret;
}

int cilk_fiber::remove_reference(cilk_fiber_pool* pool)
{
    int ref_count = this->dec_ref_count();
    if (ref_count == 0) {
        if (pool) {
            deallocate_self(pool);
        }
        else {
            deallocate_to_heap();
        }
    }
    return ref_count;
}

cilk_fiber* cilk_fiber::allocate_from_thread()
{
    void* retmem = __cilkrts_malloc(sizeof(cilk_fiber_sysdep));
    CILK_ASSERT(retmem);
    cilk_fiber_sysdep* ret = ::new(retmem) cilk_fiber_sysdep(from_thread);

    // A fiber allocated from a thread begins with a reference count
    // of 2.  The first is for being created, and the second is for
    // being running.
    //
    // Suspending this fiber will decrement the count down to 1.
    ret->init_ref_count(2);

#if SUPPORT_GET_CURRENT_FIBER    
    // We're creating the main fiber for this thread. Set this fiber as the
    // current fiber.
    cilkos_set_tls_cilk_fiber(ret);
#endif
    return ret;
}

int cilk_fiber::deallocate_from_thread()
{
    CILK_ASSERT(this->is_allocated_from_thread());
#if SUPPORT_GET_CURRENT_FIBER
    CILK_ASSERT(this == cilkos_get_tls_cilk_fiber());
    // Reverse of "allocate_from_thread".
    cilkos_set_tls_cilk_fiber(NULL);
#endif

    this->assert_ref_count_at_least(2);

    // Suspending the fiber should conceptually decrement the ref
    // count by 1.
    cilk_fiber_sysdep* self = this->sysdep();
    self->convert_fiber_back_to_thread();

    // Then, freeing the fiber itself decrements the ref count again.
    int ref_count = this->sub_from_ref_count(2);
    if (ref_count == 0) {
        self->~cilk_fiber_sysdep();
        __cilkrts_free(self);
    }
    return ref_count;
}

int cilk_fiber::remove_reference_from_thread()
{
    int ref_count = dec_ref_count();
    if (ref_count == 0) {
        cilk_fiber_sysdep* self = this->sysdep();
        self->~cilk_fiber_sysdep();
        __cilkrts_free(self);
    }
    return ref_count;
}


#if SUPPORT_GET_CURRENT_FIBER
cilk_fiber* cilk_fiber::get_current_fiber()
{
    return cilk_fiber_sysdep::get_current_fiber_sysdep();
}
#endif

void cilk_fiber::do_post_switch_actions()
{
    if (m_post_switch_proc) 
    {
        cilk_fiber_proc proc = m_post_switch_proc;
        m_post_switch_proc = NULL;
        proc(this);
    }

    if (m_pending_remove_ref)
    {
        m_pending_remove_ref->remove_reference(m_pending_pool);

        // Even if we don't free it, 
        m_pending_remove_ref = NULL;
        m_pending_pool   = NULL;
    }
}

void cilk_fiber::suspend_self_and_resume_other(cilk_fiber* other)
{
#if FIBER_DEBUG >=1
    fprintf(stderr, "suspend_self_and_resume_other: self =%p, other=%p [owner=%p, resume_sf=%p]\n",
            this, other, other->owner, other->resume_sf);
#endif

    // Decrement my reference count (to suspend)
    // Increment other's count (to resume)
    // Suspended fiber should have a reference count of at least 1.  (It is not in a pool).
    this->dec_ref_count();
    other->inc_ref_count();
    this->assert_ref_count_at_least(1);

    // Pass along my owner.
    other->owner = this->owner;
    this->owner  = NULL;

    // Change this fiber to resumable.
    CILK_ASSERT(!this->is_resumable());
    this->set_resumable(true);

    // Normally, I'd assert other->is_resumable().  But this flag may
    // be false the first time we try to "resume" a fiber.
    cilk_fiber_sysdep* self = this->sysdep();
    self->suspend_self_and_resume_other_sysdep(other->sysdep());

    // HAVE RESUMED EXECUTION
    // When we come back here, we should have at least two references:
    // one for the fiber being allocated / out of a pool, and one for it being active.
    this->assert_ref_count_at_least(2);
}

NORETURN
cilk_fiber::remove_reference_from_self_and_resume_other(cilk_fiber_pool* self_pool,
                                                        cilk_fiber*      other)
{
    // Decrement my reference count once (to suspend)
    // Increment other's count (to resume)
    // Suspended fiber should have a reference count of at least 1.  (It is not in a pool).
    this->dec_ref_count();
    other->inc_ref_count();

    // Set a pending remove reference for this fiber, once we have
    // actually switched off.
    other->m_pending_remove_ref = this;
    other->m_pending_pool   = self_pool;

    // Pass along my owner.
    other->owner = this->owner;
    this->owner  = NULL;

    // Since we are deallocating self, this fiber does not become
    // resumable.
    CILK_ASSERT(!this->is_resumable());

    cilk_fiber_sysdep* self = this->sysdep();
    self->jump_to_resume_other_sysdep(other->sysdep());

    __cilkrts_bug("Deallocating fiber.  We should never come back here.");
    std::abort();
}


void cilk_fiber::deallocate_to_heap()
{
    cilk_fiber_sysdep* self = this->sysdep();
    self->~cilk_fiber_sysdep();
    __cilkrts_free(self);
}

void cilk_fiber::deallocate_self(cilk_fiber_pool* pool)
{
    this->set_resumable(false);

    CILK_ASSERT(NULL != pool);
    CILK_ASSERT(!this->is_allocated_from_thread());
    this->assert_ref_count_equals(0);
    
    // Cases: 
    //
    // 1. pool has space:  Add to this pool.
    // 2. pool is full:    Give some fibers to parent, and then free
    //                     enough to make space for the fiber we are deallocating.
    //                     Then put the fiber back into the pool.
    
    const bool need_lock = pool->lock;
    // Grab the lock for the remaining cases.
    if (need_lock) {
        spin_mutex_lock(pool->lock);
    }

    // Case 1: this pool has space.  Return the fiber.
    if (pool->size < pool->max_size)
    {
        // Add this fiber to pool
        pool->fibers[pool->size++] = this;
        if (need_lock) {
            spin_mutex_unlock(pool->lock);
        }
        return;
    }

    // Case 2: Pool is full.
    //
    // First free up some space by giving fibers to the parent.
    if (pool->parent)
    {
        // Pool is full.  Move all but "num_to_keep" fibers to parent,
        // if we can.
        unsigned num_to_keep = pool->max_size/2 + pool->max_size/4;
        cilk_fiber_pool_move_fibers_to_parent_pool(pool, num_to_keep);
    }

    if (need_lock) {
        spin_mutex_unlock(pool->lock);
    }

    // Now, free a fiber to make room for the one we need to put back,
    // and then put this fiber back.  This step may actually return
    // fibers to the heap.
    cilk_fiber_pool_free_fibers_from_pool(pool, pool->max_size -1, this);
}


// NOTE: Except for print-debug, this code is the same as in Windows. 
void cilk_fiber::invoke_tbb_stack_op(__cilk_tbb_stack_op op)
{
    cilk_fiber_data *fdata = this->get_data();

    if (0 == fdata->stack_op_routine)
    {
        if  (CILK_TBB_STACK_RELEASE != op)
            DBG_STACK_OPS ("Wkr %p: invoke_tbb_stack_op - %s (%d) for cilk_fiber %p, fiber %p, thread id %04x - No stack op routine\n",
                           fdata->owner, 
                           NameStackOp(op),
                           op,
                           fdata,
                           this,
                           cilkos_get_current_thread_id());
        return;
    }

    // Call TBB to do it's thing
    DBG_STACK_OPS ("Wkr %p: invoke_tbb_stack_op - op %s data %p for cilk_fiber %p, fiber %p, thread id %04x\n",
                   fdata->owner, 
                   NameStackOp(op),
                   fdata->stack_op_data,
                   fdata,
                   this, 
                   cilkos_get_current_thread_id());

    (*fdata->stack_op_routine)(op, fdata->stack_op_data);
    if (op == CILK_TBB_STACK_RELEASE)
    {
        fdata->stack_op_routine = 0;
        fdata->stack_op_data = 0;
    }
}



#if NEED_FIBER_REF_COUNTS

void cilk_fiber::atomic_inc_ref_count()
{
    cilkos_atomic_add(&m_outstanding_references, 1);
}

long cilk_fiber::atomic_dec_ref_count()
{
    return cilkos_atomic_add(&m_outstanding_references, -1);
}

long cilk_fiber::atomic_sub_from_ref_count(long v)
{
    return cilkos_atomic_add(&m_outstanding_references, -v);
}

#endif // NEED_FIBER_REF_COUNTS

/* End cilk_fibers.cpp */
