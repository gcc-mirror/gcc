/* cilk_fiber.h                  -*-C++-*-
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

/**
 * @file cilk_fiber.h
 *
 * @brief Abstraction of a "fiber": A coprocess-like stack and auxiliary data
 */

#ifndef INCLUDED_CILK_FIBER_DOT_H
#define INCLUDED_CILK_FIBER_DOT_H

#include <cilk/common.h>
#ifdef __cplusplus
#   include <cstddef>
#else
#   include <stddef.h>
#endif

#include "bug.h"
#include "cilk-tbb-interop.h"
#include "spin_mutex.h"
#include "internal/abi.h"       // Define __cilkrts_stack_frame

/**
 * @brief Debugging level for Cilk fiber code.
 *
 * A value of 0 means no debugging.
 * Higher values generate more debugging output.
 */
#define FIBER_DEBUG 0

/**
 * @brief Flag for validating reference counts.
 * 
 * Set to 1 to assert that fiber reference counts are reasonable.
 */
#define FIBER_CHECK_REF_COUNTS 1

/**
 * @brief Flag to determine whether fibers support reference counting.
 * We require reference counting only on Windows, for exception
 * processing.  Unix does not need reference counting.
 */
#if defined(_WIN32)
#   define NEED_FIBER_REF_COUNTS 1
#endif

/**
 * @brief Flag to enable support for the
 * cilk_fiber_get_current_fiber() method.
 *
 * I'd like this flag to be 0.  However, the cilk_fiber test depends
 * on being able to call this method.
 */
#if !defined(SUPPORT_GET_CURRENT_FIBER)
#   define SUPPORT_GET_CURRENT_FIBER 0
#endif

/**
 * @brief Switch for enabling "fast path" check for fibers, which
 * doesn't go to the heap or OS until checking the ancestors first.
 *
 * Doing this check seems to make the stress test in
 * cilk_fiber_pool.t.cpp run faster.  But it doesn't seem to make much
 * difference in other benchmarks, so it is disabled by default.
 */
#define USE_FIBER_TRY_ALLOCATE_FROM_POOL 0


__CILKRTS_BEGIN_EXTERN_C

/// @brief Forward reference to fiber pool.
typedef struct cilk_fiber_pool cilk_fiber_pool;

/** @brief Opaque data structure representing a fiber */
typedef struct cilk_fiber cilk_fiber;

/** @brief Function pointer type for use as a fiber's "main" procedure */
typedef void (*cilk_fiber_proc)(cilk_fiber*);

/** @brief Data structure associated with each fiber. */
typedef struct cilk_fiber_data
{
    __STDNS size_t          stack_size;       /**< Size of stack for fiber    */
    __cilkrts_worker*       owner;            /**< Worker using this fiber    */
    __cilkrts_stack_frame*  resume_sf;        /**< Stack frame to resume      */
    __cilk_tbb_pfn_stack_op stack_op_routine; /**< Cilk/TBB interop callback  */
    void*                   stack_op_data;    /**< Data for Cilk/TBB callback */
    void*                   client_data;      /**< Data managed by client     */

#ifdef _WIN32
    char *initial_sp;       /**<  Initalized in fiber_stub */
# ifdef _WIN64
    char *steal_frame_sp;   /**< RSP for frame stealing work */
                            // Needed for exception handling so we can
                            // identify when about to unwind off stack
# endif
#endif

} cilk_fiber_data;

/** @brief Pool of cilk_fiber for fiber reuse
 *
 * Pools form a hierarchy, with each pool pointing to its parent.  When the
 * pool undeflows, it gets a fiber from its parent.  When a pool overflows,
 * it returns some fibers to its parent.  If the root pool underflows, it
 * allocates and initializes a new fiber from the heap but only if the total
 * is less than max_size; otherwise, fiber creation fails.
 */
struct cilk_fiber_pool
{
    spin_mutex*      lock;       ///< Mutual exclusion for pool operations 
    __STDNS size_t   stack_size; ///< Size of stacks for fibers in this pool.
    cilk_fiber_pool* parent;     ///< @brief Parent pool.
                                 ///< If this pool is empty, get from parent 

    // Describes inactive fibers stored in the pool.
    cilk_fiber**     fibers;     ///< Array of max_size fiber pointers 
    unsigned         max_size;   ///< Limit on number of fibers in pool 
    unsigned         size;       ///< Number of fibers currently in the pool

    // Statistics on active fibers that were allocated from this pool,
    // but no longer in the pool.
    int              total;      ///< @brief Fibers allocated - fiber deallocated from pool
                                 ///< total may be negative for non-root pools.
    int              high_water; ///< High water mark of total fibers
    int              alloc_max;  ///< Limit on number of fibers allocated from the heap/OS
};

/** @brief Initializes a cilk_fiber_pool structure
 *
 * @param pool         - The address of the pool that is to be initialized
 * @param parent       - The address of this pool's parent, or NULL for root pool
 * @param stack_size   - Size of stacks for fibers allocated from this pool.
 * @param buffer_size  - The maximum number of fibers that may be pooled.
 * @param alloc_max    - Limit on # of fibers this pool can allocate from the heap.
 * @param is_shared    - True if accessing this pool needs a lock, false otherwise.
 */
void cilk_fiber_pool_init(cilk_fiber_pool* pool,
                          cilk_fiber_pool* parent,
                          size_t           stack_size,
                          unsigned         buffer_size,
                          int              alloc_max,
                          int              is_shared);

/** @brief Sets the maximum number of fibers to allocate from a root pool.
 *
 * @param root_pool              - A root fiber pool
 * @param max_fibers_to_allocate - The limit on # of fibers to allocate.
 *
 * Sets the maximum number of fibers that can be allocated from this
 * pool and all its descendants.  This pool must be a root pool.
 */
void cilk_fiber_pool_set_fiber_limit(cilk_fiber_pool* root_pool,
                                     unsigned max_fibers_to_allocate);

/** @brief De-initalizes a cilk_fiber_pool
 *
 * @param pool - The address of the pool that is to be destroyed
 */
void cilk_fiber_pool_destroy(cilk_fiber_pool* pool);

/** @brief Allocates a new cilk_fiber.
 *
 * If the specified pool is empty, this method may choose to either
 * allocate a fiber from the heap (if pool->total < pool->alloc_max),
 * or retrieve a fiber from the parent pool.
 *
 * @note If a non-null fiber is returned, @c cilk_fiber_reset_state
 * should be called on this fiber before using it.
 *
 * An allocated fiber begins with a reference count of 1.
 * This method may lock @c pool or one of its ancestors.
 *
 * @pre pool should not be NULL.
 *
 * @param pool         The fiber pool from which to retrieve a fiber.
 * @return             An allocated fiber, or NULL if failed to allocate.
 */
cilk_fiber* cilk_fiber_allocate(cilk_fiber_pool* pool);

/** @brief Allocate and initialize a new cilk_fiber using memory from
 * the heap and/or OS.
 *
 * The allocated fiber begins with a reference count of 1.
 *
 * @param stack_size   The size (in bytes) to be allocated for the fiber's
 *                     stack.
 * @return             An initialized fiber.  This method should not return NULL
 *                     unless some exceptional condition has occurred.
 */
cilk_fiber* cilk_fiber_allocate_from_heap(size_t stack_size);


/** @brief Resets an fiber object just allocated from a pool with the
 * specified proc.
 *
 * After this call, cilk_fiber_data object associated with this fiber
 * is filled with zeros.
 *
 * This function can be called only on a fiber that has been allocated
 * from a pool, but never used.
 *
 * @param fiber        The fiber to reset and initialize. 
 * @param start_proc   The function to run when switching to the fiber.  If
 *                     null, the fiber can be used with cilk_fiber_run_proc()
 *                     but not with cilk_fiber_resume().
 */
void cilk_fiber_reset_state(cilk_fiber* fiber,
                            cilk_fiber_proc start_proc);

/** @brief Remove a reference from this fiber, possibly deallocating it.
 *
 * This fiber is deallocated only when there are no other references
 * to it.  Deallocation happens either by returning the fiber to the
 * specified pool, or returning it to the heap.
 *
 * A fiber that is currently executing should not remove the last
 * reference to itself.
 *
 * When a fiber is deallocated, destructors are not called for the
 * objects (if any) still on its stack.  The fiber's stack and fiber
 * data is returned to the stack pool but the client fiber data is not
 * deallocated.
 *
 * If the pool overflows because of a deallocation, then some fibers
 * will be returned to the parent pool.  If the root pool overflows,
 * then the fiber is returned to the heap.
 *
 * @param fiber   The Cilk fiber to remove a reference to.
 * @param pool    The fiber pool to which the fiber should be returned.  The
 *                caller is assumed to have exclusive access to the pool
 *                either because there is no contention for it or because
 *                its lock has been acquired.  If pool is NULL, any
 *                deallocated fiber is destroyed and returned to the
 *                heap.
 *
 * @return        Final reference count.  If the count is 0, the fiber was
 *                returned to a pool or the heap.
 */
int cilk_fiber_remove_reference(cilk_fiber *fiber, cilk_fiber_pool *pool);

/** @brief Allocates and intializes this thread's main fiber
 *
 * Each thread has an "implicit" main fiber that control's the
 * thread's initial stack.  This function makes this fiber visible to
 * the client and allocates the Cilk-specific aspects of the implicit
 * fiber.  A call to this function must be paired with a call to
 *   cilk_fiber_deallocate_fiber_from_thread() 
 * or a memory leak (or worse) will result.
 *
 * A fiber allocated from a thread begins with a reference count of 2.
 * One is for being allocated, and one is for being active.
 * (A fiber created from a thread is automatically currently executing.)
 * The matching calls above each decrement the reference count by 1.
 *
 * @return  A fiber for the currently executing thread.
 */
cilk_fiber* cilk_fiber_allocate_from_thread(void);

/** @brief Remove  a fiber created from a thread,
 * possibly deallocating it.
 *
 * Same as cilk_fiber_remove_reference, except that it works on fibers
 * created via cilk_fiber_allocate_from_thread().
 *
 * Fibers created from a thread are never returned to a pool.
 *
 * @param fiber   The Cilk fiber to remove a reference from.
 * @return        Final reference count.  If the count is 0, the fiber was
 *                returned to the heap.
 */
int cilk_fiber_remove_reference_from_thread(cilk_fiber *fiber);

/** @brief Deallocate a fiber created from a thread,
 * possibly destroying it.
 *
 * This method decrements the reference count of the fiber by 2, and
 * destroys the fiber struct if the reference count is 0.
 *
 * OS-specific cleanup for the fiber executes unconditionally with 
 * this method.  The destruction of the actual object, however, does
 * not occur unless the reference count is 0.
 *
 * @param fiber   The cilk_fiber to deallocate from a thread.
 * @return        Final reference count.  If the count is 0, the fiber was
 *                returned to the heap.
 */
int cilk_fiber_deallocate_from_thread(cilk_fiber *fiber);

/** @brief Returns true if this fiber is allocated from a thread.
 */
int cilk_fiber_is_allocated_from_thread(cilk_fiber *fiber);


/** @brief Suspend execution on current fiber resumes other fiber.
 *
 * Suspends the current fiber and transfers control to a new fiber.  Execution
 * on the new fiber resumes from the point at which fiber suspended itself to
 * run a different fiber.  If fiber was freshly allocated, then runs the
 * start_proc function specified at allocation.  This function returns when
 * another fiber resumes the self fiber.  Note that the state of the
 * floating-point control register (i.e., the register that controls rounding
 * mode, etc.) is valid but indeterminate on return -- different
 * implementations will have different results.
 *
 * When the @c self fiber is resumed, execution proceeds as though
 * this function call returns.
 *
 * This operation increments the reference count of @p other.
 * This operation decrements the reference count of @p self.
 *
 * @param self               Fiber to switch from.  Must equal current fiber.
 * @param other              Fiber to switch to.
 */
void cilk_fiber_suspend_self_and_resume_other(cilk_fiber* self,
                                              cilk_fiber* other);

/** @brief Removes a reference from the currently executing fiber and
 * resumes other fiber.
 *
 * Removes a reference from @p self and transfer control to @p other
 * fiber.  Execution on @p other resumes from the point at which @p
 * other suspended itself to run a different fiber.  If @p other fiber
 * was freshly allocated, then runs the function specified at
 * creation.
 *
 *
 * This operation increments the reference count of @p other.
 * 
 * This operation conceptually decrements the reference count of
 * @p self twice, once to suspend it, and once to remove a reference to
 * it.  Then, if the count is 0, it is returned to the specified pool
 * or destroyed.
 *
 * @pre @p self is the currently executing fiber.
 *
 * @param self               Fiber to remove reference switch from. 
 * @param self_pool          Pool to which the current fiber should be returned
 * @param other              Fiber to switch to.
 */
NORETURN
cilk_fiber_remove_reference_from_self_and_resume_other(cilk_fiber*      self,
                                                       cilk_fiber_pool* self_pool,
                                                       cilk_fiber*      other);

/** @brief Set the proc method to execute immediately after a switch
 * to this fiber.
 *
 * The @c post_switch_proc method executes immediately after switching
 * away form @p self fiber to some other fiber, but before @c self
 * gets cleaned up.
 * 
 * @note A fiber can have only one post_switch_proc method at a time.
 * If this method is called multiple times before switching to the
 * fiber, only the last proc method will execute.
 *
 * @param self              Fiber.
 * @param post_switch_proc  Proc method to execute immediately after switching to this fiber.
 */
void cilk_fiber_set_post_switch_proc(cilk_fiber* self, cilk_fiber_proc post_switch_proc);

/** @brief Invoke TBB stack op for this fiber.
 *
 * @param fiber Fiber to invoke stack op for.
 * @param op    The stack op to invoke
 */
void cilk_fiber_invoke_tbb_stack_op(cilk_fiber* fiber, __cilk_tbb_stack_op op);

/** @brief Returns the fiber data associated with the specified fiber.
 *
 * The returned struct is owned by the fiber and is deallocated automatically
 * when the fiber is destroyed.  However, the client_data field is owned by
 * the client and must be deallocated separately.  When called for a
 * newly-allocated fiber, the returned data is zero-filled.
 *
 * @param fiber   The fiber for which data is being requested.
 * @return        The fiber data for the specified fiber
 */
cilk_fiber_data* cilk_fiber_get_data(cilk_fiber* fiber);

/** @brief Retrieve the owner field from the fiber.
 *
 *  This method is provided for convenience.  One can also get the
 *  fiber data, and then get the owner field.
 */
__CILKRTS_INLINE
__cilkrts_worker* cilk_fiber_get_owner(cilk_fiber* fiber)
{
    // TBD: We really want a static assert here, that this cast is
    // doing the right thing.
    cilk_fiber_data* fdata = (cilk_fiber_data*)fiber;
    return fdata->owner;
}

/** @brief Sets the owner field of a fiber.
 *
 *  This method is provided for convenience.  One can also get the
 *  fiber data, and then get the owner field.
 */
__CILKRTS_INLINE
void cilk_fiber_set_owner(cilk_fiber* fiber, __cilkrts_worker* owner) 
{
    // TBD: We really want a static assert here, that this cast is
    // doing the right thing.
    cilk_fiber_data* fdata = (cilk_fiber_data*)fiber;
    fdata->owner = owner;
}
    
/** @brief Returns true if this fiber is resumable.
 *
 * A fiber is considered resumable when it is not currently being
 * executed.
 *
 * This function is used by Windows exception code.
 * @param fiber   The fiber to check.
 * @return        Nonzero value if fiber is resumable.
 */
int cilk_fiber_is_resumable(cilk_fiber* fiber);

/**
 * @brief Returns the base of this fiber's stack.
 *
 * On some platforms (e.g., Windows), the fiber must have started
 * running before we can get this information.
 *
 * @param fiber   The fiber to get the stack pointer from.
 * @return        The base of the stack, or NULL if this
 *                information is not available yet.
 */
char* cilk_fiber_get_stack_base(cilk_fiber* fiber);


/****************************************************************************
 * TBB interop functions
 * **************************************************************************/
/**
 * @brief Set the TBB callback information for a stack
 *
 * @param fiber The fiber to set the TBB callback information for
 * @param o     The TBB callback thunk.  Specifies the callback address and
 *              context value.
 */
void cilk_fiber_set_stack_op(cilk_fiber *fiber,
                             __cilk_tbb_stack_op_thunk o);
                     
/**
 * @brief Save the TBB callback address and context value in
 * thread-local storage.
 *
 * We'll use it later when the thread binds to a worker.
 *
 * @param o The TBB callback thunk which is to be saved.
 */
void cilk_fiber_tbb_interop_save_stack_op_info(__cilk_tbb_stack_op_thunk o);

/**
 * @brief Move TBB stack-op info from thread-local storage and store
 * it into the fiber.
 *
 * Called when we bind a thread to the runtime.  If there is any TBB
 * interop information in thread-local storage, bind it to the stack
 * now.
 *
 * @pre \c fiber should not be NULL.
 * @param fiber The fiber that should take over the TBB interop information.
 */
void cilk_fiber_tbb_interop_use_saved_stack_op_info(cilk_fiber *fiber);

/**
 * @brief Free any TBB interop information saved in thread-local storage
 */
void cilk_fiber_tbb_interop_free_stack_op_info(void);

/**
 * @brief Migrate any TBB interop information from a cilk_fiber to
 * thread-local storage.
 *
 * Returns immediately if no TBB interop information has been
 * associated with the stack.
 *
 * @param fiber The cilk_fiber who's TBB interop information should be
 * saved in thread-local storage.
 */
void cilk_fiber_tbb_interop_save_info_from_stack(cilk_fiber* fiber);


#if SUPPORT_GET_CURRENT_FIBER
/** @brief Returns the fiber associated with the currently executing thread
 *
 * @note This function is currently used only for testing the Cilk
 * runtime.
 *
 * @return Fiber associated with the currently executing thread or NULL if no
 *         fiber was associated with this thread.
 */
cilk_fiber* cilk_fiber_get_current_fiber(void);
#endif


#if NEED_FIBER_REF_COUNTS 
/** @brief Returns true if this fiber has reference count > 0.
 * 
 * @param fiber   The fiber to check for references.
 * @return        Nonzero value if the fiber has references.
 */
int cilk_fiber_has_references(cilk_fiber *fiber);

/** @brief Returns the value of the reference count.
 *
 * @param fiber   The fiber to check for references.
 * @return        The value of the reference count of fiber.
 */
int cilk_fiber_get_ref_count(cilk_fiber *fiber);

/** @brief Adds a reference to this fiber.
 *
 *  Increments the reference count of a current fiber.  Fibers with
 *  nonzero reference count will not be freed or returned to a fiber
 *  pool.
 *
 * @param fiber   The fiber to add a reference to.
 */
void cilk_fiber_add_reference(cilk_fiber *fiber);

#endif // NEED_FIBER_REF_COUNTS

__CILKRTS_END_EXTERN_C

#ifdef __cplusplus
// Some C++ implementation details

/// Opaque declaration of a cilk_fiber_sysdep object.
struct cilk_fiber_sysdep;

/**
 * cilk_fiber is a base-class for system-dependent fiber implementations.
 */
struct cilk_fiber : protected cilk_fiber_data
{
  protected:
    // This is a rare acceptable use of protected inheritence and protected
    // variable access: when the base class and derived class collaborate
    // tightly to comprise a single component.

    /// For overloading constructor of cilk_fiber. 
    enum from_thread_t { from_thread = 1 };

    // Boolean flags capturing the status of the fiber.
    // Each one can be set independently.
    // A default fiber is constructed with a flag value of 0.
    static const int RESUMABLE             = 0x01;  ///< True if the fiber is in a suspended state and can be resumed.
    static const int ALLOCATED_FROM_THREAD = 0x02;  ///< True if fiber was allocated from a thread.

    cilk_fiber_proc  m_start_proc;        ///< Function to run on start up/reset
    cilk_fiber_proc  m_post_switch_proc;  ///< Function that executes when we first switch to a new fiber from a different one.

    cilk_fiber*      m_pending_remove_ref;///< Fiber to possibly delete on start up or resume
    cilk_fiber_pool* m_pending_pool;      ///< Pool where m_pending_remove_ref should go if it is deleted.
    unsigned         m_flags;             ///< Captures the status of this fiber. 

#if NEED_FIBER_REF_COUNTS
    volatile long    m_outstanding_references;  ///< Counts references to this fiber.
#endif

    /// Creates a fiber with NULL data.
    cilk_fiber();

    /**
     * @brief Creates a fiber with user-specified arguments.
     *
     * @param stack_size   Size of stack to use for this fiber.
     */
    cilk_fiber(std::size_t stack_size);

    /// Empty destructor.
    ~cilk_fiber();

    /**
     * @brief Performs any actions that happen after switching from
     * one fiber to another.
     *
     * These actions are:
     *   1. Execute m_post_switch_proc on a fiber.
     *   2. Do any pending deallocations from the previous fiber.
     */
    void do_post_switch_actions();

    /**
     *@brief Helper method that converts a @c cilk_fiber object into a
     * @c cilk_fiber_sysdep object.
     *
     * The @c cilk_fiber_sysdep object contains the system-dependent parts
     * of the implementation of a @\c cilk_fiber.
     *
     * We could have @c cilk_fiber_sysdep inherit from @c cilk_fiber and
     * then use virtual functions.  But since a given platform only uses
     * one definition of @c cilk_fiber_sysdep at a time, we statically
     * cast between them.
     */
    inline cilk_fiber_sysdep* sysdep();

    /**
     * @brief Set resumable flag to specified state.
     */
    inline void set_resumable(bool state) {
        m_flags = state ?  (m_flags | RESUMABLE) : (m_flags & (~RESUMABLE));
    }

    /**
     *@brief Set the allocated_from_thread flag. 
     */
    inline void set_allocated_from_thread(bool state) {
        m_flags = state ?  (m_flags | ALLOCATED_FROM_THREAD) : (m_flags & (~ALLOCATED_FROM_THREAD));
    }

  public:

    /**
     * @brief Allocates and initializes a new cilk_fiber, either from
     * the specified pool or from the heap.
     *
     * @pre pool should not be NULL.
     */
    static cilk_fiber* allocate(cilk_fiber_pool* pool);

    /**
     * @brief Allocates a fiber from the heap.
     */
    static cilk_fiber* allocate_from_heap(size_t stack_size);

    /**
     * @brief Return a fiber to the heap.
     */
    void deallocate_to_heap();

    /**
     * @brief Reset the state of a fiber just allocated from a pool.
     */
    void reset_state(cilk_fiber_proc start_proc);

    /**
     * @brief Remove a reference from this fiber, possibly
     * deallocating it if the reference count becomes 0.
     *
     * @param pool The fiber pool to which this fiber should be returned.
     * @return     The final reference count.
     */
    int remove_reference(cilk_fiber_pool* pool);

    /**
     * @brief Deallocate the fiber by returning it to the pool.
     * @pre This method should only be called if the reference count
     * is 0.
     *
     * @param pool The fiber pool to return this fiber to. If NULL,
     *   fiber is returned to the heap.
     */
    void deallocate_self(cilk_fiber_pool *pool);

    /** @brief Allocates and intializes this thread's main fiber. */
    static cilk_fiber* allocate_from_thread();

    /** @brief Deallocate a fiber created from a thread,
     * possibly destroying it.
     *
     * This method decrements the reference count of this fiber by 2,
     * and destroys the fiber if the reference count is 0.
     *
     * OS-specific cleanup for the fiber executes unconditionally with for
     * this method.  The destruction of the actual object, however, does
     * not occur unless the reference count is 0.
     *
     * @return        Final reference count.  If the count is 0, the fiber was
     *                returned to the heap.
     */
    int deallocate_from_thread();

    /** @brief Removes a reference from this fiber.
     *
     * This method deallocates this fiber if the reference count
     * becomes 0.
     *
     * @pre     This fiber must be allocated from a thread.
     * @return  The final reference count of this fiber.
     */
    int remove_reference_from_thread();

#if SUPPORT_GET_CURRENT_FIBER
    /** @brief Get the current fiber from TLS.
     *
     * @note This function is only used for testing the runtime.
     */
    static cilk_fiber* get_current_fiber();
#endif

    /** @brief Suspend execution on current fiber resumes other fiber.
     * 
     * Control returns after resuming execution of the self fiber.
     */ 
    void suspend_self_and_resume_other(cilk_fiber* other);


    /** @brief Removes a reference from the currently executing fiber
     * and resumes other fiber.
     *
     *  This fiber may be returned to a pool or deallocated.
     */
    NORETURN remove_reference_from_self_and_resume_other(cilk_fiber_pool* self_pool,
                                                         cilk_fiber*      other);

    /** @brief Set the proc method to execute immediately after a switch
     * to this fiber.
     *
     * @param post_switch_proc Proc method to execute immediately
     * after switching to this fiber.
     */
    inline void set_post_switch_proc(cilk_fiber_proc post_switch_proc) {
        m_post_switch_proc = post_switch_proc;
    }

    /** @brief Returns true if this fiber is resumable.
     *
     * A fiber is considered resumable when it is not currently being
     * executed.
     */
    inline bool is_resumable(void) {
        return (m_flags & RESUMABLE);
    }
    
    /** @brief Returns true if fiber was allocated from a thread. */   
    inline bool is_allocated_from_thread(void) {
        return (m_flags & ALLOCATED_FROM_THREAD);
    }

    /**
     *@brief Get the address at the base of the stack for this fiber.
     */
    inline char* get_stack_base();
    
    /** @brief Return the data for this fiber. */ 
    cilk_fiber_data*       get_data()       { return this; }

    /** @brief Return the data for this fiber. */ 
    cilk_fiber_data const* get_data() const { return this; }

    
#if NEED_FIBER_REF_COUNTS
    /** @brief Verifies that this fiber's reference count equals v. */
    inline void assert_ref_count_equals(long v) {
    #if FIBER_CHECK_REF_COUNTS
        CILK_ASSERT(m_outstanding_references >= v);
    #endif
    }

    /** @brief Verifies that this fiber's reference count is at least v. */
    inline void assert_ref_count_at_least(long v) {
    #if FIBER_CHECK_REF_COUNTS
        CILK_ASSERT(m_outstanding_references >= v);
    #endif
    }

    /** @brief Get reference count. */
    inline long get_ref_count()        { return m_outstanding_references; }

    /** @brief Initialize reference count.
     *  Operation is not atomic.
     */
    inline void init_ref_count(long v) { m_outstanding_references = v; }

    // For Windows, updates to the fiber reference count need to be
    // atomic, because exceptions can live on a stack that we are not
    // currently executing on.  Thus, we can update the reference
    // count of a fiber we are not currently executing on.

    /** @brief Increment reference count for this fiber [Windows]. */
    inline void inc_ref_count()            { atomic_inc_ref_count(); }

    /** @brief Decrement reference count for this fiber [Windows]. */
    inline long dec_ref_count()            { return atomic_dec_ref_count(); }

    /** @brief Subtract v from the reference count for this fiber [Windows]. */
    inline long sub_from_ref_count(long v) { return atomic_sub_from_ref_count(v); }
#else  // NEED_FIBER_REF_COUNTS

    // Without reference counting, we have placeholder methods.
    inline void init_ref_count(long v) { }

    inline void inc_ref_count() { }
    
    // With no reference counting, dec_ref_count always return 0.
    // Thus, anyone checking is always the "last" one.
    inline long dec_ref_count() { return 0; }
    inline long sub_from_ref_count(long v) { return 0; }

    // The assert methods do nothing.
    inline void assert_ref_count_equals(long v) { }
    inline void assert_ref_count_at_least(long v) { }
#endif    

    /**
     * @brief Call TBB to tell it about an "interesting" event.
     *
     * @param op    Value specifying the event to track.
     */
    void invoke_tbb_stack_op(__cilk_tbb_stack_op op);

private:

    /**
     * @brief Helper method: try to allocate a fiber from this pool or
     * its ancestors without going to the OS / heap.
     *
     * Returns allocated pool, or NULL if no pool is found.
     *
     * If pool contains a suitable fiber. Return it.  Otherwise, try to
     * recursively grab a fiber from the parent pool, if there is one.
     *
     * This method will not allocate a fiber from the heap.
     */
    static cilk_fiber* try_allocate_from_pool_recursive(cilk_fiber_pool* pool);
    
    
#if NEED_FIBER_REF_COUNTS
    /**
     * @brief Atomic increment of reference count. 
     */
    void atomic_inc_ref_count();

    /**
     * @brief Atomic decrement of reference count.
     */
    long atomic_dec_ref_count();

    /**
     * @brief Atomic subtract of v from reference count.
     * @param v Value to subtract.
     */    
    long atomic_sub_from_ref_count(long v);
#endif // NEED_FIBER_REF_COUNTS
    
};

#endif // __cplusplus

#endif // ! defined(INCLUDED_CILK_FIBER_DOT_H)
