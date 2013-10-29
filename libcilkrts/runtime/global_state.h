/* global_state.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2009-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
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
 *  @copyright
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
 **************************************************************************/

/**
 * @file global_state.h
 *
 * @brief The global_state_t structure contains most of the global context
 * maintained by the Intel Cilk runtime.
 */

#ifndef INCLUDED_GLOBAL_STATE_DOT_H
#define INCLUDED_GLOBAL_STATE_DOT_H

#include <cilk/common.h>

#include "frame_malloc.h"
#include "stats.h"
#include "bug.h"
#include "cilk_fiber.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * Non-null place-holder for a stack handle that has no meaningful value.
 */
#define PLACEHOLDER_FIBER  ((cilk_fiber *) -2)

/**
 * States for record_or_replay
 */
enum record_replay_t {
    RECORD_REPLAY_NONE,
    RECORD_LOG,
    REPLAY_LOG
};

/**
 * @brief The global state is a structure that is shared by all workers in
 * Cilk.
 *
 * Make the structure ready for use by calling
 * cilkg_init_global_state() and then cilkg_publish_global_state().
 *
 * The same global lock should be held while both of these methods are
 * called.  These methods are split because it is useful to execute
 * other runtime initialization code in between.
 *
 * After cilkg_publish_global_state() has completed, Cilk runtime
 * methods may call cilkg_get_global_state() to look at the published
 * value without holding the global lock.
 *
 * Finally, clean up the global state by calling
 * cilkg_deinit_global_state().  This method should be called only
 * after all calls to cilkg_get_global_state() have completed, and
 * while holding the global lock.
 *
 * Before initialization and after deinitialization, the fields in the
 * global state have unspecified values, except for a few special
 * fields labeled "USER SETTING", which can be read and written before
 * initialization and after deinitialization.
 */

struct global_state_t { /* COMMON_PORTABLE */

    /* Fields described as "(fixed)" should not be changed after
     * initialization.
     */

    /*************************************************************************
     * Note that debugger integration must reach into the
     * global state!  The debugger integration is depending on the
     * offsets of the addr_size, system_workers, total_workers,
     * stealing_disabled, sysdep, and workers.  If these offsets change, the
     * debugger integration library will need to be changed to match!!!
     *************************************************************************/

    int addr_size; ///< Number of bytes for an address, used by debugger (fixed)

    int system_workers; ///< Number of system workers (fixed)

    /**
     * @brief USER SETTING: Maximum number of user workers that can be
     * bound to cilk workers.
     *
     * 0 unless set by user.  Call cilkg_calc_max_user_workers to get
     * the value.
     */
    int max_user_workers; 

    int total_workers;  ///< Total number of worker threads allocated (fixed)

    int workers_running; ///< True when system workers have beens started */

    /// Set by debugger to disable stealing (fixed)
    int stealing_disabled;

    /// System-dependent part of the global state
    struct global_sysdep_state *sysdep;

    /// Array of worker structures.
    __cilkrts_worker **workers;

    /******* END OF DEBUGGER-INTEGRATION FIELDS ***************/

    /// Number of frames in each worker's lazy task queue
    __STDNS size_t ltqsize;

    /**
     * @brief USER SETTING: Force all possible reductions.
     *
     * TRUE if running a p-tool that requires reducers to call the reduce()
     * method even if no actual stealing occurs.
     *
     * When set to TRUE, runtime will simulate steals, forcing calls to the 
     * the reduce() methods of reducers.
     *
     */
    int force_reduce;    

    /// USER SETTING: Per-worker fiber pool size
    int fiber_pool_size; 

    /// USER SETTING: Global fiber pool size
    int global_fiber_pool_size;

    /**
     * @brief TRUE when workers should exit scheduling loop so we can
     * shut down the runtime and free the global state.
     *
     * @note @c work_done will be checked *FREQUENTLY* in the scheduling loop
     * by idle workers.  We need to ensure that it's not in a cache line which
     * may be invalidated by other cores.  The surrounding fields are either
     * constant after initialization or not used until shutdown (stats) so we
     * should be OK.
     */
    volatile int work_done;

    int under_ptool;     ///< True when running under a serial PIN tool

    statistics stats;    ///< Statistics on use of runtime

    /**
     * @brief USER SETTING: Maximum number of stacks the runtime will
     * allocate (apart from those created by the OS when worker
     * threads are created).
     *
     * If max_stacks == 0,there is no pre-defined maximum.
     */
    unsigned max_stacks; 

    /// Size of each stack
    size_t stack_size;

    /// Global cache for per-worker memory
    struct __cilkrts_frame_cache frame_malloc;

    /// Global fiber pool
    cilk_fiber_pool fiber_pool;


    /**
     * @brief Track whether the runtime has failed to allocate a
     * stack.
     * 
     * Setting this flag prevents multiple warnings from being
     * issued.
     */
    int failure_to_allocate_stack;

    /**
     * @brief USER SETTING: indicate record or replay log.
     * Set to NULL if not used in this run.
     */
    char *record_replay_file_name;

    /**
     * @brief Record/replay state.
     * Valid states are:
     *   RECORD_REPLAY_NONE - Not recording or replaying a log
     *   RECORD_LOG - Recording a log for replay later
     *   REPLAY_LOG - Replay a log recorded earlier
     */
    enum record_replay_t record_or_replay;

    /**
     * @brief Buffer to force max_steal_failures to appear on a
     * different cache line from the previous member variables.
     *
     * This padding is needed because max_steal_failures is read
     * constantly and other modified values in the global state will
     * cause thrashing.
     */
    char cache_buf[64];

    /**
     * @brief Maximum number of times a thread should fail to steal
     * before checking if Cilk is shutting down.
     */
    unsigned int max_steal_failures;

    /// Pointer to scheduler entry point
    void (*scheduler)(__cilkrts_worker *w);

    /**
     * @brief Buffer to force P and Q to appear on a different cache
     * line from the previous member variables.
     */
    char cache_buf_2[64];

    int P;         ///< USER SETTING: number of system workers + 1 (fixed)
    int Q;         ///< Number of user threads currently bound to workers 
};

/**
 * @brief Initialize the global state object.  This method must both
 * complete before referencing any fields in the global state, except
 * those specified as "user-settable values".
 */
global_state_t* cilkg_init_global_state();

/**
 * @brief Publish the global state object, so that
 * cilkg_is_published can return true.
 *
 * @param g - the global state created by cilkg_init_global_state() to
 * publish.
 *
 * After the global state object has been published, a thread should
 * not modify this state unless it has exclusive access (i.e., holds
 * the global lock).
 */
void cilkg_publish_global_state(global_state_t* g);

/**
 * @brief Return true if the global state has been fully initialized
 * and published, and has not been deinitialized.
 */
int cilkg_is_published(void);

/**
 * @brief De-initializes the global state object.  Must be called to free
 * resources when the global state is no longer needed.
 */
void cilkg_deinit_global_state(void);

/**
 * @brief Returns the global state object.  Result is valid only if the
 * global state has been published (see cilkg_publish_global_state()).
 */
static inline
global_state_t* cilkg_get_global_state(void)
{
    // "private" extern declaration:
    extern global_state_t *cilkg_singleton_ptr;

    __CILKRTS_ASSERT(cilkg_singleton_ptr); // Debug only
    return cilkg_singleton_ptr;
}


/**
 * @brief Implementation of __cilkrts_set_params.
 *
 * Set user controllable parameters
 * @param param - string specifying parameter to be set
 * @param value - string specifying new value
 * @returns One of: CILKG_SET_PARAM_SUCCESS ( = 0),
 *    CILKG_SET_PARAM_UNIMP, CILKG_SET_PARAM_XRANGE,
 *    CILKG_SET_PARAM_INVALID, or CILKG_SET_PARAM_LATE.
 *
 * @attention The wide character version __cilkrts_set_param_w() is available
 * only on Windows.
 *
 * Allowable parameter names:
 *
 * - "nworkers" - number of processors that should run Cilk code.
 *   The value is a string of digits to be parsed by strtol.
 *
 * - "force reduce" - test reducer callbacks by allocating new views
 *   for every spawn within which a reducer is accessed.  This can
 *   significantly reduce performance.  The value is "1" or "true"
 *   to enable, "0" or "false" to disable.
 *   @warning Enabling "force reduce" when running with more than a single
 *   worker is currently broken.
 *
 * - "max user workers" - (Not publicly documented) Sets the number of slots
 *   allocated for user worker threads
 *
 * - "local stacks" - (Not publicly documented) Number of stacks we'll hold in
 *   the per-worker stack cache.  Range 1 .. 42.  See
 *   cilkg_init_global_state for details.
 *
 * - "shared stacks" - (Not publicly documented) Maximum number of stacks
 *   we'll hold in the global stack cache. Maximum value is 42.  See
 *   __cilkrts_make_global_state for details
 *
 * - "nstacks" - (Not publicly documented at this time, though it may be
 *   exposed in the future) Sets the maximum number of stacks permitted at one
 *   time.  If the runtime reaches this maximum, it will cease to allocate
 *   stacks and the app will lose parallelism.  0 means unlimited.  Default is
 *   unlimited.  Minimum is twice the number of worker threads, though that
 *   cannot be tested at this time.
 */
int cilkg_set_param(const char* param, const char* value);
#ifdef _WIN32
/**
 * @brief Implementation of __cilkrts_set_params for Unicode characters on
 * Windows.  See the documentation on @ref cilkg_set_param for more details.
 *
 * Set user controllable parameters
 * @param param - string specifying parameter to be set
 * @param value - string specifying new value
 * @returns One of: CILKG_SET_PARAM_SUCCESS ( = 0),
 *    CILKG_SET_PARAM_UNIMP, CILKG_SET_PARAM_XRANGE,
 *    CILKG_SET_PARAM_INVALID, or CILKG_SET_PARAM_LATE.
 */
int cilkg_set_param_w(const wchar_t* param, const wchar_t* value);
#endif

/**
 * @brief implementation of __cilkrts_get_nworkers()
 */
static inline
int cilkg_get_nworkers(void)
{
    // "private" extern declaration
    extern global_state_t* cilkg_get_user_settable_values(void);
    return cilkg_get_user_settable_values()->P;
}

/**
 * @brief implementation of __cilkrts_get_total_workers()
 */
static inline
int cilkg_get_total_workers(void)
{
    // "private" extern declaration
    extern int cilkg_calc_total_workers(void);

    // This number can fluctate until initialization so we
    // compute it from scratch
    return cilkg_calc_total_workers();
}

/**
 * @brief implementation of __cilkrts_get_force_reduce()
 */
static inline
int cilkg_get_force_reduce(void)
{
    // "private" extern declaration
    extern global_state_t* cilkg_get_user_settable_values(void);
    return cilkg_get_user_settable_values()->force_reduce;
}

/**
 * @brief implementation of __cilkrts_get_stack_size()
 */
static inline
size_t cilkg_get_stack_size(void)
{
    // "private" extern declaration
    extern global_state_t* cilkg_get_user_settable_values(void);
    return cilkg_get_user_settable_values()->stack_size;
}

/**
 * @brief Run the scheduler function stored in the global_state
 *
 * Look up the scheduler function in global_state and run it.  Report a fatal
 * error if an exception escapes the scheduler function.
 * 
 * @param w - Worker structure to associate with the current thread.
 *
 * @attention The scheduler field of the global state must be set before this
 * function is called.
 */
void __cilkrts_run_scheduler_with_exceptions(__cilkrts_worker *w);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_GLOBAL_STATE_DOT_H)
