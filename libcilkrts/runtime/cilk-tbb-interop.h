/* cilk-tbb-interop.h                  -*-C++-*-
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
 * @file cilk-tbb-interop.h
 *
 * @brief Interface between TBB and Cilk to allow TBB to associate it's
 * per-thread data with Cilk workers, and maintain the association as work
 * moves between worker threads.  This handles the case where TBB calls
 * into a Cilk function which may later call back to a function making
 * TBB calls.
 *
 * Each thunk structure has two pointers: \"routine\" and \"data\".
 * The caller of the thunk invokes *routine, passing \"data\" as the void*
 * parameter.
 */

#ifndef INCLUDED_CILK_TBB_INTEROP_DOT_H
#define INCLUDED_CILK_TBB_INTEROP_DOT_H

#include <cilk/common.h>  // for CILK_EXPORT

__CILKRTS_BEGIN_EXTERN_C

/** A return code.  0 indicates success. */
typedef int __cilk_tbb_retcode;

/**
 * Enumeration of reasons that Cilk will call the TBB stack operation
 * function.
 *
 * When a non-empty stack is transfered between threads, the first thread must
 * orphan it and the second thread must adopt it.
 *
 * An empty stack can be transfered similarly, or simply released by the first
 * thread.
 *
 * Here is a summary of the actions as transitions on a state machine.
@verbatim
                       watch                                    ORPHAN
                       -->-->                                   -->--
                      /      \                                 /     \
   (freed empty stack)       (TBB sees stack running on thread)      (stack in limbo)
                      \     /                                  \     / 
                       --<--                                    --<--
                       RELEASE or                               ADOPT
                       unwatch
@endverbatim
 */
typedef enum __cilk_tbb_stack_op {
   /**
    * Disconnecting stack from a thread.
    *
    * The thunk must be invoked on the thread disconnecting itself from the
    * stack.  Must \"happen before\" the stack is adopted elsewhere.
    */
    CILK_TBB_STACK_ORPHAN,

    /**
     * Reconnecting orphaned stack to a thread.
     *
     * The thunk must be invoked on the thread adopting the stack.
     */
    CILK_TBB_STACK_ADOPT,

   /**
    * Releasing stack.
    *
    * The thunk must be invoked on the thread doing the releasing, Must
    * \"happen before\" the stack is used elsewhere.
    */
    CILK_TBB_STACK_RELEASE
} __cilk_tbb_stack_op;

/**
 * Function that will be called by the Cilk runtime to inform TBB of a change
 * in the stack associated with the current thread.
 *
 * It does not matter what stack the thunk runs on.
 * The thread (not fiber) on which the thunk runs is important.
 *
 * @param op Enumerated value indicating what type of change is ocurring.
 * @param data Context value provided by TBB in the __cilkrts_watch_stack
 * call.  This data is opaque to Cilk.
 *
 * @return 0 indicates success.
 */
typedef __cilk_tbb_retcode (*__cilk_tbb_pfn_stack_op)(enum __cilk_tbb_stack_op op,
                                                      void* data);

/**
 * Function that will be called by TBB to inform the Cilk runtime that TBB
 * is no longer interested in watching the stack bound to the current thread.
 *
 * @param data Context value provided to TBB by the __cilkrts_watch_stack
 * call.  This data is opaque to TBB.
 *
 * @return 0 indicates success.
 */
typedef __cilk_tbb_retcode (*__cilk_tbb_pfn_unwatch_stacks)(void *data);

/**
 * Thunk invoked by Cilk to call back to TBB to tell it about a change in
 * the stack bound to the current thread.
 */
typedef struct __cilk_tbb_stack_op_thunk {
    /// Function in TBB the Cilk runtime should call when something
    // "interesting" happens involving a stack
    __cilk_tbb_pfn_stack_op routine;

    /// TBB context data to pass with the call to the stack_op routine
    void* data;
} __cilk_tbb_stack_op_thunk;

/**
 * Thunk invoked by TBB when it is no longer interested in watching the stack
 * bound to the current thread.
 */
typedef struct __cilk_tbb_unwatch_thunk {
    /// Function in Cilk runtime to call when TBB no longer wants to watch
    // stacks
    __cilk_tbb_pfn_unwatch_stacks routine;

    /// Cilk runtime context data to pass with the call to the unwatch_stacks
    /// routine
    void* data;
} __cilk_tbb_unwatch_thunk;

/**
 * Requests that Cilk invoke __cilk_tbb_orphan_thunk when it orphans a stack.
 * Cilk sets *u to a thunk that TBB should call when it is no longer
 * interested in watching the stack.
 *
 * If the thread is not yet bound to the Cilk runtime, the Cilk runtime should
 * save this data in thread-local storage until __cilkrts_bind_thread is called.
 *
 * Called by TBB, defined by Cilk.  This function is exported from the Cilk
 * runtime DLL/shared object.  This declaration also appears in
 * cilk/cilk_undocumented.h -- don't change one declaration without also
 * changing the other.
 *
 * @param u __cilk_tbb_unwatch_thunk.  This structure will be filled in by
 * the Cilk runtime to allow TBB to register that it is no longer interested
 * in watching the stack bound to the current thread.
 * @param o __cilk_tbb_stack_op_thunk.  This structure specifies the routine
 * that the Cilk runtime should call when an "interesting" change in the stack
 * associate with the current worker occurs.
 *
 * @return 0 indicates success.
 */
CILK_EXPORT
__cilk_tbb_retcode __cilkrts_watch_stack(__cilk_tbb_unwatch_thunk* u,
                                         __cilk_tbb_stack_op_thunk o);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_CILK_TBB_INTEROP_DOT_H)
