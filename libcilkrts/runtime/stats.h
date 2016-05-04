/* stats.h                  -*-C++-*-
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
 * @file stats.h
 *
 * @brief Support for gathering and reporting statistics on Cilk applications.
 *
 * Note that stats are normally NOT compiled in because it increases the
 * overhead of stealing.  To compile in profiling support, define CILK_PROFILE.
 */

#ifndef INCLUDED_STATS_DOT_H
#define INCLUDED_STATS_DOT_H

/* #define CILK_PROFILE 1 */
// @note  The CILK_PROFILE flag and intervals is known to be broken
//        in at least programs with Windows exceptions. 
//        Enable this flag at your own peril. :)

#include <cilk/common.h>
#include "rts-common.h"
#include "internal/abi.h"

#ifdef CILK_PROFILE
#include <stdio.h>     // Define FILE *
#endif

__CILKRTS_BEGIN_EXTERN_C

// Some notes on the stats we are counting for the Cilk Plus runtime. 
//
// INTERVAL_IN_SCHEDULER:
//
//   For a user worker, it starts counting when the worker thread is
//   bound, and stops when the worker thread is unbound.
//
//   For a system worker, it starts counting when the worker thread is
//   created, and stops after it returns from the scheduling loop
//   (which usually won't happen unless __cilkrts_end_cilk() is
//   called.)
// 
// INTERVAL_WORKING + INTERVAL_IN_RUNTIME should be equal to
// INTERVAL_IN_SCHEDULER (modulo overheads in instrumentation).
//
// INTERVAL_IN_RUNTIME tries to count all the time spent inside
// runtime code.  (leave_frame and reudctions blur the accounting a
// bit).  We switch from WORKING to IN_RUNTIME from:
// 
//    1. __cilkrts_leave_frame, when we start doing an exception check
//       and might jump into the runtime, either when returning from a
//       spawn or have an exception to process.
//
//    2. __cilkrts_c_sync, when we are at a sync and the frame has
//       been stolen from.
//
// We switch back to WORKING when we are returning to user code.
// 
// We also switch back to WORKING when we need to do a slow merge of
// reducer maps.  Otherwise, parallel reductions can mess up the stat
// collection.

/** @brief Events that we measure. */
enum interval
{
    INTERVAL_IN_SCHEDULER,                  ///< Time threads spend "bound" to Cilk
    INTERVAL_WORKING,                       ///< Time spent working
    INTERVAL_IN_RUNTIME,                    ///< Time spent executing runtime code
    INTERVAL_SCHED_LOOP,                    ///< Time spent in scheduling loop
    INTERVAL_STEALING,                      ///< Time spent stealing work
    INTERVAL_STEAL_SUCCESS,                 ///< Time to do a successful steal
    INTERVAL_STEAL_FAIL_EMPTYQ,             ///< Count of steal failures due to lack of stealable work
    INTERVAL_STEAL_FAIL_LOCK,               ///< Count of steal failures due to failure to lock worker
    INTERVAL_STEAL_FAIL_USER_WORKER,        ///< Count of steal failures by user workers which attempt to steal from another team
    INTERVAL_STEAL_FAIL_DEKKER,             ///< Count of steal failures due to Dekker protocol failure
    INTERVAL_SYNC_CHECK,                    ///< Time spent processing syncs
    INTERVAL_THE_EXCEPTION_CHECK,           ///< Time spent performing THE exception checks
    INTERVAL_THE_EXCEPTION_CHECK_USELESS,   ///< Count of useless THE exception checks
    INTERVAL_RETURNING,                     ///< Time spent returning from calls
    INTERVAL_FINALIZE_CHILD,                ///< Time spent in finalize_child
    INTERVAL_PROVABLY_GOOD_STEAL,           ///< Time spent in provably_good_steal
    INTERVAL_UNCONDITIONAL_STEAL,           ///< Time spent in unconditional_steal
    INTERVAL_ALLOC_FULL_FRAME,              ///< Time spent in __cilkrts_make_full_frame
    INTERVAL_FRAME_ALLOC_LARGE,             ///< Count of calls to __cilkrts_frame_malloc for buffers bigger than FRAME_MALLOC_MAX_SIZE or with a NULL worker
    INTERVAL_FRAME_ALLOC,                   ///< Time spent allocating memory from worker buckets
    INTERVAL_FRAME_ALLOC_GLOBAL,            ///< Time spent calling memory allocator when buckets are empty
    INTERVAL_FRAME_FREE_LARGE,              ///< Count of calls to __cilkrts_frame_malloc for buffers bigger than FRAME_MALLOC_MAX_SIZE or with a NULL worker
    INTERVAL_FRAME_FREE,                    ///< Time spent freeing memory to worker buckets
    INTERVAL_FRAME_FREE_GLOBAL,             ///< Time spent calling memory deallocator when buckets are full
    INTERVAL_MUTEX_LOCK,                    ///< Count of calls to __cilkrts_mutex_lock for a worker
    INTERVAL_MUTEX_LOCK_SPINNING,           ///< Time spent spinning in __cilkrts_mutex_lock for a worker
    INTERVAL_MUTEX_LOCK_YIELDING,           ///< Time spent yielding in __cilkrts_mutex_lock for a worker
    INTERVAL_MUTEX_TRYLOCK,                 ///< Count of calls to __cilkrts_mutex_trylock
    INTERVAL_FIBER_ALLOCATE,                ///< Time spent calling cilk_fiber_allocate
    INTERVAL_FIBER_DEALLOCATE,              ///< Time spent calling cilk_fiber_deallocate (not from thread)
    INTERVAL_FIBER_ALLOCATE_FROM_THREAD,    ///< Time spent calling cilk_fiber_allocate_from_thread
    INTERVAL_FIBER_DEALLOCATE_FROM_THREAD,  ///< Time spent calling cilk_fiber_deallocate (from thread)
    INTERVAL_SUSPEND_RESUME_OTHER,          ///< Count of fiber suspend_self_and_resume_other
    INTERVAL_DEALLOCATE_RESUME_OTHER,       ///< Count of fiber deallocate_self_and_resume_other
    INTERVAL_INIT_WORKER,                   ///< Time spent on initialization of worker thread
    INTERVAL_SCHEDULE_WAIT,                 ///< Time worker spends in SCHEDULE_WAIT state
    INTERVAL_N                              ///< Number of intervals, must be last
};

/**
 * @brief Struct that collects of all runtime statistics.
 * 
 * There is an instance of this structure in each worker's
 * local_state, as well as one in the @c global_state_t which will be
 * used to accumulate the per-worker stats.
 */
typedef struct statistics
{
    /** Number of times each interval is entered */
    unsigned long long count[INTERVAL_N];

    /**
     * Time when the system entered each interval, in system-dependent
     * "ticks"
     */
    unsigned long long start[INTERVAL_N];

    /** Total time spent in each interval, in system-dependent "ticks" */
    unsigned long long accum[INTERVAL_N];

    /**
     * Largest global number of stacks seen by this worker.
     * The true maximum at end of execution is the max of the
     * worker maxima.
     */
    long stack_hwm;
} statistics;

/**
 * Initializes a statistics structure
 *
 * @param s The statistics structure to be initialized.
 */
COMMON_PORTABLE void __cilkrts_init_stats(statistics *s);

/**
 * @brief Sums statistics from worker to the global struct
 *
 * @param to   The statistics structure that will accumulate the information.
 *             This structure is usually @c g->stats.
 * @param from The statistics structure that will be accumulated.
 *             This structure is usually statistics kept per worker.
 */
COMMON_PORTABLE
void __cilkrts_accum_stats(statistics *to, statistics *from);

/**
 * @brief Mark the start of an interval by saving the current tick count.
 *
 * @pre Start time == INVALID_START
 *
 * @param w The worker we're accumulating stats for.
 * @param i The interval we're accumulating stats for.
 */
COMMON_PORTABLE
void __cilkrts_start_interval(__cilkrts_worker *w, enum interval i);

/**
 * @brief Mark the end of an interval by adding the ticks since the
 * start to the accumulated time.
 *
 * @pre Start time != INVALID_START
 *
 * @param w The worker we're accumulating stats for.
 * @param i The interval we're accumulating stats for.
 */
COMMON_PORTABLE
void __cilkrts_stop_interval(__cilkrts_worker *w, enum interval i);

/**
 * @brief Start and stop interval I, charging zero time against it
 *
 * Precondition:
 * - Start time == INVALID_START
 *
 * @param w The worker we're accumulating stats for.
 * @param i The interval we're accumulating stats for.
 */
COMMON_PORTABLE
void __cilkrts_note_interval(__cilkrts_worker *w, enum interval i);

#ifdef CILK_PROFILE
COMMON_PORTABLE
void dump_stats_to_file(FILE *stat_file, statistics *s);
#endif


#ifdef CILK_PROFILE
# define START_INTERVAL(w, i) __cilkrts_start_interval(w, i);
# define STOP_INTERVAL(w, i) __cilkrts_stop_interval(w, i);
# define NOTE_INTERVAL(w, i) __cilkrts_note_interval(w, i);
#else
/** Start an interval.  No effect unless CILK_PROFILE is defined. */
# define START_INTERVAL(w, i)
/** End an interval.  No effect unless CILK_PROFILE is defined. */
# define STOP_INTERVAL(w, i)
/** Increment a counter.  No effect unless CILK_PROFILE is defined. */
# define NOTE_INTERVAL(w, i)
#endif

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_STATS_DOT_H)
