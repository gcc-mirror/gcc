/* pedigrees.h                  -*-C++-*-
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

#ifndef INCLUDED_PEDIGREES_DOT_H
#define INCLUDED_PEDIGREES_DOT_H


#include <cilk/common.h>
#include <internal/abi.h>

#include "rts-common.h"
#include "global_state.h"
#include "os.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * @file pedigrees.h
 *
 * @brief pedigrees.h declares common routines related to pedigrees
 * and the pedigree API.
 */


/**
 * @brief Sets the leaf pedigree node for the current user thread.
 *
 * A typical implementation stores this pedigree node in thread-local
 * storage.
 *
 * Preconditions:
 *  - Current thread should be a user thread.
 *
 * @param leaf The pedigree node to store as a leaf.
 */
COMMON_PORTABLE
void __cilkrts_set_pedigree_leaf(__cilkrts_pedigree* leaf);


/**
 * Load the pedigree leaf node from thread-local storage into the
 * current user worker.  This method should execute as a part of
 * binding the user thread to a worker.
 *
 * Preconditions:
 *  
 *  - w should be the worker for the current thread 
 *  - w should be a user thread.
 */
COMMON_PORTABLE
void load_pedigree_leaf_into_user_worker(__cilkrts_worker *w);

/**
 * Save the pedigree leaf node from the worker into thread-local
 * storage.  This method should execute as part of unbinding a user
 * thread from a worker.
 *
 * Preconditions:
 *  
 *  - w should be the worker for the current thread 
 *  - w should be a user thread.
 */
COMMON_PORTABLE
void save_pedigree_leaf_from_user_worker(__cilkrts_worker *w);



/**
 * Update pedigree for a worker when leaving a frame.
 *
 * If this is the frame of a spawn helper (indicated by the
 *  CILK_FRAME_DETACHED flag) we must update the pedigree.  The
 *  pedigree points to nodes allocated on the stack.  Failing to
 *  update it will result in a accvio/segfault if the pedigree is
 *  walked.  This must happen for all spawn helper frames, even if
 *  we're processing an exception.
 */ 
COMMON_PORTABLE
inline void update_pedigree_on_leave_frame(__cilkrts_worker *w,
					   __cilkrts_stack_frame *sf) 
{
    // Update the worker's pedigree information if this is an ABI 1 or later
    // frame
    if (CILK_FRAME_VERSION_VALUE(sf->flags) >= 1)
    {
	w->pedigree.rank = sf->spawn_helper_pedigree.rank + 1;
	w->pedigree.parent = sf->spawn_helper_pedigree.parent;
    }
}



__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_PEDIGREES_DOT_H)
