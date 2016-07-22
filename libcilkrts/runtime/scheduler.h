/* scheduler.h                  -*-C++-*-
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
 * @file scheduler.h
 *
 * @brief scheduler.h declares routines for the Intel Cilk Plus scheduler,
 * making it the heart of the Intel Cilk Plus implementation.
 */

#ifndef INCLUDED_SCHEDULER_DOT_H
#define INCLUDED_SCHEDULER_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>

#include "rts-common.h"
#include "full_frame.h"
#include "reducer_impl.h"
#include "global_state.h"

#ifdef CILK_RECORD_REPLAY
#include "record-replay.h"
#endif

__CILKRTS_BEGIN_EXTERN_C


/**
 * @brief Flag to disable parallel reductions.
 *
 * Set to 0 to allow parallel reductions.
 */
#define DISABLE_PARALLEL_REDUCERS 0

/**
 * @brief Debugging level for parallel reductions.
 *
 * Print debugging messages and assertions for parallel reducers. 0 is
 * no debugging.  A higher value generates more output.
 */
#define REDPAR_DEBUG 0

/**
 * @brief Lock the worker mutex to allow exclusive access to the
 * values in the @c __cilkrts_worker and local_state structures.
 *
 * @pre @c w->l->do_not_steal must not be set.  Essentially this
 * condition asserts that the worker is not locked recursively.
 *
 * @param w The worker to lock.
 */
COMMON_PORTABLE
void __cilkrts_worker_lock(__cilkrts_worker *w);

/**
 * @brief Unlock the worker mutex.
 *
 * @pre @c w->l->do_not_steal must be set.  Essentially this condition
 * asserts that the worker has been previously locked.
 *
 * @param w The worker to unlock.
 */
COMMON_PORTABLE
void __cilkrts_worker_unlock(__cilkrts_worker *w);

/**
 * @brief Push the next full frame to be made active in this worker
 * and increment its join counter.
 *
 * __cilkrts_push_next_frame and pop_next_frame work on a one-element queue.
 * This queue is used to communicate across the runtime from the code that
 * wants to activate a frame to the code that can actually begin execution
 * on that frame.  They are asymetrical in that push increments the join
 * counter but pop does not decrement it.  Rather, a single push/pop
 * combination makes a frame active and increments its join counter once.
 *
 * @note A system worker may chose to push work onto a user worker if
 * the work is the continuation from a sync which only the user worker
 * may complete.
 *
 * @param w The worker which the frame is to be pushed onto.
 * @param ff The full_frame which is to be continued by the worker.
 */
COMMON_PORTABLE
void __cilkrts_push_next_frame(__cilkrts_worker *w,
                               full_frame *ff);

/**
 * @brief Sync on this worker.
 *
 * If this worker is the last to reach the sync, execution may resume
 * on this worker after the sync.
 *
 * If this worker is not the last spawned child to reach the sync,
 * then execution is suspended and the worker will re-enter the
 * scheduling loop, looking for work it can steal.
 *
 * This function will jump into the runtime to switch to the scheduling
 * stack to implement most of its logic.
 *
 * @param w The worker which is executing the sync.
 * @param sf The __cilkrts_stack_frame containing the sync.
 */
COMMON_PORTABLE
NORETURN __cilkrts_c_sync(__cilkrts_worker *w,
                          __cilkrts_stack_frame *sf);

/**
 * @brief Worker @c w completely promotes its own deque, simulating the case
 * where the whole deque is stolen.
 *
 * We use this mechanism to force the allocation of new storage for
 * reducers for race-detection purposes.
 *
 * This method is called from the reducer lookup logic when
 * @c g->force_reduce is set.
 *
 * @warning Use of "force_reduce" is known to have bugs when run with
 * more than 1 worker.
 *
 * @param w The worker which is to have all entries in its deque
 * promoted to full frames.
 */
COMMON_PORTABLE
void __cilkrts_promote_own_deque(__cilkrts_worker *w);

/**
 * Called when a spawned function attempts to return and
 * __cilkrts_undo_detach() fails. This can happen for two reasons:
 *
 * @li If another worker is considering stealing our parent, it bumps the
 * exception pointer while it did so, which will cause __cilkrts_undo_detach()
 * to fail. If the other worker didn't complete the steal of our parent, we
 * still may be able to return to it, either because the steal attempt failed,
 * or we won the race for the tail pointer.
 *
 * @li If the function's parent has been stolen then we cannot return. Instead
 * we'll longjmp into the runtime to switch onto the scheduling stack to
 * execute do_return_from_spawn() and determine what to do.  Either this
 * worker is the last one to the sync, in which case we need to jump to the
 * sync, or this worker is not the last one to the sync, in which case we'll
 * abandon this work and jump to the scheduling loop to search for more work
 * we can steal.
 *
 * @param w The worker which attempting to return from a spawn to
 * a stolen parent.
 * @param returning_sf The stack frame which is returning. 
 */
COMMON_PORTABLE
void __cilkrts_c_THE_exception_check(__cilkrts_worker *w,
				     __cilkrts_stack_frame *returning_sf);

/**
 * @brief Return an exception to an stolen parent.
 *
 * Used by the gcc implementation of exceptions to return an exception
 * to a stolen parent
 *
 * @param w The worker which attempting to return from a spawn with an
 * exception to a stolen parent.
 * @param returning_sf The stack frame which is returning.
 */
COMMON_PORTABLE
NORETURN __cilkrts_exception_from_spawn(__cilkrts_worker *w,
					__cilkrts_stack_frame *returning_sf);

/**
 * @brief Used by the Windows implementations of exceptions to migrate an exception
 * across fibers.
 *
 * Call this function when an exception has been thrown and has to
 * traverse across a steal.  The exception has already been wrapped
 * up, so all that remains is to longjmp() into the continuation,
 * sync, and re-raise it.
 *
 * @param sf The __cilkrts_stack_frame for the frame that is attempting to
 * return an exception to a stolen parent.
 */
void __cilkrts_migrate_exception (__cilkrts_stack_frame *sf);

/**
 * @brief Return from a call, not a spawn, where this frame has ever
 * been stolen.
 *
 * @param w The worker that is returning from a frame which was ever stolen.
 */
COMMON_PORTABLE
void __cilkrts_return(__cilkrts_worker *w);

/**
 * @brief Special return from the initial frame.
 *
 * This method will be called from @c __cilkrts_leave_frame if
 * @c CILK_FRAME_LAST is set.
 *
 * This function will do the things necessary to cleanup, and unbind the
 * thread from the Intel Cilk Plus runtime.  If this is the last user
 * worker unbinding from the runtime, all system worker threads will be
 * suspended.
 *
 * @pre @c w must be the currently executing worker, and must be a user
 * worker.
 *
 * @param w The worker that's returning from the initial frame.
 */
COMMON_PORTABLE
void __cilkrts_c_return_from_initial(__cilkrts_worker *w);

/**
 * @brief Used by exception handling code to pop an entry from the
 * worker's deque.
 *
 * @param w Worker to pop the entry from
 *
 * @return __cilkrts_stack_frame of parent call
 * @return NULL if the deque is empty
 */
COMMON_PORTABLE
__cilkrts_stack_frame *__cilkrts_pop_tail(__cilkrts_worker *w);

/**
 * @brief Modifies the worker's protected_tail to prevent frames from
 * being stolen.
 *
 * The Dekker protocol has been extended to only steal if head+1 is also
 * less than protected_tail.
 *
 * @param w The worker to be modified.
 * @param new_protected_tail The new setting for protected_tail, or NULL if the
 * entire deque is to be protected
 *
 * @return Previous value of protected tail.
 */
COMMON_PORTABLE
__cilkrts_stack_frame *volatile *__cilkrts_disallow_stealing(
    __cilkrts_worker *w,
    __cilkrts_stack_frame *volatile *new_protected_tail);

/**
 * @brief Restores the protected tail to a previous state, possibly
 * allowing frames to be stolen.
 *
 * @param w The worker to be modified.
 * @param saved_protected_tail A previous setting for protected_tail that is
 * to be restored
 */
COMMON_PORTABLE
void __cilkrts_restore_stealing(
    __cilkrts_worker *w,
    __cilkrts_stack_frame *volatile *saved_protected_tail);

/**
 * @brief Initialize a @c __cilkrts_worker.
 *
 * @note The memory for the worker must have been allocated outside
 * this call.
 *
 * @param g The global_state_t.
 * @param self The index into the global_state's array of workers for this
 * worker, or -1 if this worker was allocated from the heap and cannot be
 * stolen from.
 * @param w The worker to be initialized.
 *
 * @return The initialized __cilkrts_worker.
 */
COMMON_PORTABLE
__cilkrts_worker *make_worker(global_state_t *g,
                              int self,
                              __cilkrts_worker *w);

/**
 * @brief Free up any resources allocated for a worker.
 *
 * @note The memory for the @c __cilkrts_worker itself must be
 * deallocated outside this call.
 *
 * @param w The worker to be destroyed.
 */
COMMON_PORTABLE
void destroy_worker (__cilkrts_worker *w);

/**
 * @brief Initialize the runtime.
 * 
 * If necessary, allocates and initializes the global state.  If
 * necessary, unsuspends the system workers.
 *
 * @param start Specifies whether the workers are to be unsuspended if
 * they are suspended.  Allows __cilkrts_init() to start up the runtime without
 * releasing the system threads.
 */
COMMON_PORTABLE
void __cilkrts_init_internal(int start);

/**
 * @brief Part of the sequence to shutdown the runtime.
 *
 * Specifically, this call frees the @c global_state_t for the runtime.
 *
 * @param g The global_state_t.
 */
COMMON_PORTABLE
void __cilkrts_deinit_internal(global_state_t *g);

/**
 * Obsolete.  We no longer need to import or export reducer maps.
 */
COMMON_PORTABLE
cilkred_map *__cilkrts_xchg_reducer(
    __cilkrts_worker *w, cilkred_map *newmap) cilk_nothrow;

/**
 * @brief Called when a user thread is bound to the runtime.
 *
 * If this action increments the count of bound user threads from 0 to
 * 1, the system worker threads are unsuspended.
 *
 * If this action increments the count of bound user threads from 0 to
 * 1, the system worker threads are unsuspended.
 *
 * @pre Global lock must be held.
 * @param g The runtime global state.
 */
COMMON_PORTABLE
void __cilkrts_enter_cilk(global_state_t *g);

/**
 * @brief Called when a user thread is unbound from the runtime.
 *
 * If this action decrements the count of bound user threads to 0, the
 * system worker threads are suspended.
 *
 *
 * @pre  Global lock must be held.
 *
 * @param g The runtime global state.
 */
COMMON_PORTABLE
void __cilkrts_leave_cilk(global_state_t *g);


/**
 * @brief cilk_fiber_proc that runs the main scheduler loop on a
 * user worker.
 *
 * @pre  fiber's owner field should be set to the correct __cilkrts_worker
 * @pre  fiber must be a user worker.
 *
 * @param fiber    The scheduling fiber object.
 */
void scheduler_fiber_proc_for_user_worker(cilk_fiber *fiber);


/**
 * @brief Prints out Cilk runtime statistics.
 *
 * @param g The runtime global state.
 *
 * This method is useful only for debugging purposes.  No guarantees
 * are made as to the validity of this data. :)
 */
COMMON_PORTABLE
void __cilkrts_dump_stats_to_stderr(global_state_t *g);

#ifdef CILK_RECORD_REPLAY
COMMON_PORTABLE
char * walk_pedigree_nodes(char *p, const __cilkrts_pedigree *pnode);

/**
 * @brief Used by exception handling code to simulate the popping of
 * an entry from the worker's deque.
 *
 * @param w Worker whose deque we want to check
 *
 * @return @c __cilkrts_stack_frame of parent call
 * @return NULL if the deque is empty
 */
COMMON_PORTABLE
__cilkrts_stack_frame *simulate_pop_tail(__cilkrts_worker *w);

#endif

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_SCHEDULER_DOT_H)
