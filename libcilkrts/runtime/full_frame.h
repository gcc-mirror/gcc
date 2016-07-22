/* full_frame.h                  -*-C++-*-
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

#ifndef INCLUDED_FULL_FRAME_DOT_H
#define INCLUDED_FULL_FRAME_DOT_H


#include "rts-common.h"
#include "worker_mutex.h"

#include <cilk/common.h>
#include <internal/abi.h>
#include <stddef.h>
#include "cilk_fiber.h"

__CILKRTS_BEGIN_EXTERN_C

/** Magic numbers for full_frame, used for debugging */
typedef unsigned long long ff_magic_t;

/* COMMON_SYSDEP */ struct pending_exception_info;  /* opaque */

/*************************************************************
  Full frames
*************************************************************/

/**
 * @file full_frame.h
 * @brief A full frame includes additional information such as a join
 * counter and parent frame.
 * @defgroup FullFrames Full Frames
 * A full frame includes additional information such as a join
 * counter and parent frame.
 * @{
 */

/**
 * Convenience typedef so we don't have to specify "struct full_frame"
 * all over the code.  Putting it before the structure definition allows
 * us to use the typedef within the structure itself
 */
typedef struct full_frame full_frame;

/**
 * @brief A full frame includes additional information such as a join
 * counter and parent frame.
 *
 * The frame at the top of a worker's stack is promoted into a "full"
 * frame, which carries additional information, such as join counter
 * and parent frame.  Full frames can be suspended at a sync, in which
 * case they lie somewhere in memory and do not belong to any
 * worker. 
 *
 * Full frames are in contrast to the entries in the worker's deque which
 * are only represented by a pointer to their __cilkrts_stack_frame.
 *
 * At any instant, we say that a full frame ff is either "suspended",
 * or "owned" by some worker w.
 *
 * More precisely, we say that a worker w owns a frame ff under one of
 * the following conditions:
 *
 *  1. Creation: Worker w has just created ff, but not yet linked ff
 *     into the tree of full frames.  This situation can occur when a
 *     worker is unrolling a call stack to promote a
 *     __cilkrts_stack_frame to a full_frame.
 *  2. Executing frame: We have w->l->frame_ff == ff, i.e,. ff is the
 *     currently executing frame for w.
 *  3. Next frame: We have w->l->next_frame_ff == ff, i.e,. ff is the
 *     next frame that w is about to execute.
 *  4. Resume execution: Worker w has popped ff from
 *     w->l->next_frame_ff, and is about to resume execution of ff.
 *  5. Dying leaf: Worker w has finished executing a frame ff
 *     that is a leaf the tree of full frames, and is in the process
 *     of unlinking "ff" from the tree.
 *
 * Otherwise, the frame ff is suspended, and has no owner.
 * Note that work-stealing changes the owner of a full frame from the
 * victim to the thief.  
 *
 * Using this notion of ownership, we classify the fields of a full
 * frame into one of several categories:
 *
 *  1. Local: 
 *     These fields are accessed only by the owner of the full frame.
 *     Because a frame can have only one owner at a time, these fields
 *     can be modified without any (additional) locking or
 *     synchronization, assuming the correct synchronization for
 *     changing the ownership of full frame (e.g., on a successful
 *     steal) is already in place.
 *
 *  2. Constant (i.e., read-only):
 *     This field is constant for the lifetime of the full frame.
 *     No locks are needed to access this field.
 *     Technically, a field could be read-only and local, but we assume
 *     it is shared.
 *  
 *  3. Self-locked:
 *     To access this field in the frame ff, a worker should acquire
 *     the lock on ff.  
 *     A self-locked field is conceptually "shared" between the worker
 *     that owns frame ff (which is a child) and the worker that
 *     owns the frame ff->parent (which is the parent of ff).
 *
 *  4. Parent-locked:
 *     To access this field in the frame ff, a worker should
 *     acquire the lock on ff->parent.
 *     A parent-locked field is conceptually "shared" between the worker
 *     that owns frame ff, and a worker that is either owns the
 *     parent frame (ff->parent) or owns a sibling frame of ff (i.e.,
 *     any child of ff->parent).
 *
 *  5. Synchronization
 *     A field used explicitly for synchronization (i.e., locks).
 */

/* COMMON_PORTABLE */ 
struct full_frame
{
    /**
     * Value to detect writes off the beginning of a full_frame.
     */
#   define FULL_FRAME_MAGIC_0 ((ff_magic_t)0x361e710b9597d553ULL)

    /**
     * Field to detect writes off the beginning of a full_frame.  Must be
     * FULL_FRAME_MAGIC_0.
     * [constant]
     */
    ff_magic_t full_frame_magic_0;

    /**
     * Used to serialize access to this full_frame
     * [synchronization]
     */
    struct mutex lock;

    /**
     * Count of outstanding children running in parallel
     * [self-locked]
     */
    int join_counter;

    /**
     * If TRUE: frame was called by the parent.
     * If FALSE: frame was spawned by parent.
     * [constant]
     */
    int is_call_child;

    /**
     * TRUE if this frame is the loot of a simulated steal.
     *
     * This situation never happens in normal execution.  However,
     * when running under cilkscreen, a worker may promote frames and
     * then immediately suspend them, in order to simulate an
     * execution on an infinite number of processors where all spawns
     * are stolen.  In this case, the frame is marked as the loot of a fake
     * steal.
     * [local]
     */
    int simulated_stolen;

    /**
     * Caller of this full_frame
     * [constant]
     */
    full_frame *parent;

    /**
     * Doubly-linked list of children.  The serial execution order is
     * by definition from left to right.  Because of how we do work
     * stealing, the parent is always to the right of all its
     * children.
     *
     * For a frame ff, we lock the ff->parent to follow the sibling
     * links for ff.
     *
     * [parent-locked]
     */
    full_frame *left_sibling;

    /**
     * @copydoc left_sibling
     */
    full_frame *right_sibling;

    /**
     * Pointer to rightmost child
     *
     * [self-locked]
     */
    full_frame *rightmost_child;

    /**
     * Call stack associated with this frame.
     * Set and reset in make_unrunnable and make_runnable
     *
     * [self-locked]
     */
    __cilkrts_stack_frame *call_stack;

    /**
     * Accumulated reducers of children
     *
     * [self-locked]
     */
    struct cilkred_map *children_reducer_map;

    /**
     * Accumulated reducers of right siblings that have already
     * terminated
     *
     * [parent-locked]
     */
    struct cilkred_map *right_reducer_map;

    /**
     * Exception that needs to be pass to our parent
     *
     * [local]
     *
     * TBD: verify that the exception code satisfies this requirement.
     */
    struct pending_exception_info *pending_exception;

    /**
     * Exception from one of our children
     *
     * [self-locked]
     */
    struct pending_exception_info *child_pending_exception;

    /**
     * Exception from any right siblings
     *
     * [parent-locked]
     */
    struct pending_exception_info *right_pending_exception;

    /**
     * Stack pointer to restore on sync.
     * [local]
     */
    char *sync_sp;

#ifdef _WIN32
    /**
     * Stack pointer to restore on exception.
     * [local]
     */
    char *exception_sp;

    /**
     * Exception trylevel at steal
     * [local]
     *
     * TBD: this field is set but not read?
     */
    unsigned long trylevel;

    /**
     * Exception registration head pointer to restore on sync.
     * [local]
     */
    unsigned long registration;
#endif

    /**
     * Size of frame to match sync sp
     * [local]
     * TBD: obsolete field only used in debugging?
     */
    ptrdiff_t frame_size;

    /**
     * Allocated fibers that need to be freed.  The fibers work
     * like a reducer.  The leftmost frame may have @c fiber_self
     * null and owner non-null.
     *
     * [local]
     * TBD: verify exception code satisfies this requirement.
     */
    cilk_fiber *fiber_self;

    /**
     * Allocated fibers that need to be freed.  The fibers work
     * like a reducer.  The leftmost frame may have @c fiber_self
     * null and owner non-null.
     *
     * [self-locked]
     */
    cilk_fiber *fiber_child;

    /**
     * If the sync_master is set, this function can only be sync'd by the team
     * leader, who first entered Cilk.  This is set by the first worker to steal
     * from the user worker.
     *
     * [self-locked]
     */
    __cilkrts_worker *sync_master;

    /**
     * Value to detect writes off the end of a full_frame.
     */
#   define FULL_FRAME_MAGIC_1 ((ff_magic_t)0x189986dcc7aee1caULL)

    /**
     * Field to detect writes off the end of a full_frame.  Must be
     * FULL_FRAME_MAGIC_1.
     *
     * [constant]
     */
    ff_magic_t full_frame_magic_1;
};

/* The functions __cilkrts_put_stack and __cilkrts_take_stack keep track of
 * changes in the stack's depth between when the point at which a frame is
 * stolen and when it is resumed at a sync.  A stolen frame typically goes
 * through the following phase changes:
 *
 *   1. Suspend frame while stealing it.
 *   2. Resume stolen frame at begining of continuation
 *   3. Suspend stolen frame at a sync
 *   4. Resume frame (no longer marked stolen) after the sync
 *
 * When the frame is suspended (steps 1 and 3), __cilkrts_put_stack is called to
 * establish the stack pointer for the sync.  When the frame is resumed (steps
 * 2 and 4), __cilkrts_take_stack is called to indicate the stack pointer
 * (which may be on a different stack) at
 * the point of resume.  If the stack pointer changes between steps 2 and 3,
 * e.g., as a result of pushing 4 bytes onto the stack,
 * the offset is reflected in the value of ff->sync_sp after step 3 relative to
 * its value after step 1 (e.g., the value of ff->sync_sp after step 3 would be
 * 4 less than its value after step 1, for a down-growing stack).
 *
 * Imp detail: The actual call chains for each of these phase-change events is:
 *
 *   1. unroll_call_stack -> make_unrunnable  -> __cilkrts_put_stack
 *   2. do_work           -> __cilkrts_resume -> __cilkrts_take_stack
 *   3. do_sync -> disown -> make_runnable    -> __cilkrts_put_stack
 *   4. __cilkrts_resume                      -> __cilkrts_take_stack
 *
 * (The above is a changeable implementation detail.  The resume, sequence, in
 * particular, is more complex on some operating systems.)
 */

/**
 * @brief Records the stack pointer within the @c sf stack frame as the
 * current stack pointer at the point of suspending full frame @c ff.
 *
 * @pre @c ff->sync_sp must be either null or contain the result of a prior call to
 *      @c __cilkrts_take_stack().
 * @pre If @c ff->sync_sp is not null, then @c SP(sf) must refer to the same stack as
 *      the @c sp argument to the prior call to @c __cilkrts_take_stack().
 * 

 * @post If @c ff->sync_sp was null before the call, then @c
 *       ff->sync_sp will be set to @c SP(sf).
 * @post Otherwise, @c ff->sync_sp will be restored to the value it had just prior
 *       to the last call to @c __cilkrts_take_stack(), except offset by any change
 *       in the stack pointer between the call to @c __cilkrts_take_stack() and
 *       this call to @c __cilkrts_put_stack().
 *
 * @param ff The full frame that is being suspended.
 * @param sf The @c __cilkrts_stack_frame that is being suspended.  The stack
 *   pointer will be taken from the jmpbuf contained within this
 *   @c __cilkrts_stack_frame.
 */
COMMON_PORTABLE void __cilkrts_put_stack(full_frame *ff,
                                         __cilkrts_stack_frame *sf);

/**
 * @brief Records the stack pointer @c sp as the stack pointer at the point of
 * resuming execution on full frame @c ff.
 *
 * The value of @c sp may be on a different stack than the original
 * value recorded for the stack pointer using __cilkrts_put_stack().
 *
 * @pre  @c ff->sync_sp must contain a value set by @c __cilkrts_put_stack().
 *
 * @post @c ff->sync_sp contains an *integer* value used to compute a change in the
 *       stack pointer upon the next call to @c __cilkrts_take_stack().
 * @post If @c sp equals @c ff->sync_sp, then @c ff->sync_sp is set to null.
 *
 * @param ff The full frame that is being resumed.
 * @param sp The stack pointer for the stack the function is being resumed on.
 */
COMMON_PORTABLE void __cilkrts_take_stack(full_frame *ff, void *sp);

/*
 * @brief Adjust the stack for to deallocate a Variable Length Array
 *
 * @param ff The full frame that is being adjusted.
 * @param size The size of the array being deallocated from the stack
 */
COMMON_PORTABLE void __cilkrts_adjust_stack(full_frame *ff, size_t size);

/**
 * @brief Allocates and initailizes a full_frame.
 *
 * @param w The memory for the full_frame will be allocated out of the
 * worker's pool.
 * @param sf The @c __cilkrts_stack_frame which will be saved as the call_stack
 * for this full_frame.
 *
 * @return The newly allocated and initialized full_frame.
 */
COMMON_PORTABLE
full_frame *__cilkrts_make_full_frame(__cilkrts_worker *w,
                                      __cilkrts_stack_frame *sf);

/**
 * @brief Deallocates a full_frame.
 *
 * @param w The memory for the full_frame will be returned to the worker's pool.
 * @param ff The full_frame to be deallocated.
 */
COMMON_PORTABLE
void __cilkrts_destroy_full_frame(__cilkrts_worker *w, full_frame *ff);

/**
 * @brief Performs sanity checks to check the integrity of a full_frame.
 *
 * @param ff The full_frame to be validated.
 */
COMMON_PORTABLE void validate_full_frame(full_frame *ff);

/**
 * @brief Locks the mutex contained in a full_frame.
 *
 * The full_frame is validated before the runtime attempts to lock it.
 *
 * @post @c ff->lock will be owned by @c w.
 *
 * @param w  The worker that will own the full_frame.  If the runtime is
 * collecting stats, the intervals will be attributed to the worker.
 * @param ff The full_frame containing the mutex to be locked.
 */
COMMON_PORTABLE void __cilkrts_frame_lock(__cilkrts_worker *w,
                                          full_frame *ff);

/**
 * @brief Unlocks the mutex contained in a full_frame.
 *
 * @pre @c ff->lock must must be owned by @c w.
 *
 * @param w  The worker that currently owns the full_frame.
 * @param ff The full_frame containing the mutex to be unlocked.
 */
COMMON_PORTABLE void __cilkrts_frame_unlock(__cilkrts_worker *w,
                                            full_frame *ff);
/** @} */

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_FULL_FRAME_DOT_H)
