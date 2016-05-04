/* except.h                  -*-C++-*-
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
 * @file except.h
 *
 * @brief Common definitions for the various implementations of exception
 * handling.
 */

#ifndef INCLUDED_EXCEPT_DOT_H
#define INCLUDED_EXCEPT_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>
#include "full_frame.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * OS-dependent information about an exception that's being moved between
 * strands.
 */
typedef struct pending_exception_info pending_exception_info;

/**
 * Merge the right exception record into the left.  The left is logically
 * earlier.
 *
 * On entry the left state is synched and can not have an unresolved
 * exception.  The merge may result in an unresolved exception.
 *
 * If there is both a right and left exception, the right exception will
 * be disposed of in preference to the left exception, destructing the
 * exception object.
 *
 * @param w The worker that is preparing to resume execution.
 * @param left_exception The exception that would have happened earlier
 * if the code executed serially.  Can be NULL if the left strand has not
 * raised an exception.
 * @param right_exception The exception that would have happened later
 * if the code executed serially.  Can be NULL if the right strand has not
 * raised an exception.
 *
 * @return NULL if there both the right and left exception are NULL. This
 * indicates that there are no pending exceptions.
 * @return The pending exception that is to be raised to continue searching
 * for a catch block to handle the exception.
 */
COMMON_SYSDEP
struct pending_exception_info *__cilkrts_merge_pending_exceptions(
    __cilkrts_worker *w,
    pending_exception_info *left_exception,
    pending_exception_info *right_exception);

/**
 * Move the exception information from the worker to the full_frame.
 *
 * @param w The worker which is suspending work on a full_frame.
 * @param ff The full_frame which is being suspended.
 */
COMMON_SYSDEP
void __cilkrts_save_exception_state(__cilkrts_worker *w,
                                    full_frame *ff);

/**
 * Function to delete pending exception.  This will delete the
 * exception object and then free the stack/fiber.
 *
 * @param w The worker we're running on.
 * @param pei The pending exception to be delete
 * @param delete_object Unused.  Should always be 1.
 */
void delete_exception_obj (__cilkrts_worker *w,
                           struct pending_exception_info *pei,
                           int delete_object);

#ifndef _WIN32
/* gcc-style exception handling */
NON_COMMON NORETURN __cilkrts_c_sync_except(__cilkrts_worker *w,
                                            __cilkrts_stack_frame *sf);
NON_COMMON void __attribute__((nonnull))
__cilkrts_gcc_rethrow(__cilkrts_stack_frame *sf);
#endif

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_EXCEPT_DOT_H)
