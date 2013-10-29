/* reducer_impl.h                  -*-C++-*-
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
 * @file reducer_impl.h
 *
 * @brief Functions to implement reducers in the runtime.
 */

#ifndef INCLUDED_REDUCER_IMPL_DOT_H
#define INCLUDED_REDUCER_IMPL_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>
#include "rts-common.h"

__CILKRTS_BEGIN_EXTERN_C

/**
 * Construct an empty reducer map from the memory pool associated with the
 * given worker.  This reducer map must be destroyed before the worker's
 * associated global context is destroyed.
 *
 * @param w __cilkrts_worker the cilkred_map is being created for.
 *
 * @return Pointer to the initialized cilkred_map.
 */
COMMON_SYSDEP
cilkred_map *__cilkrts_make_reducer_map(__cilkrts_worker *w);

/**
 * Destroy a reducer map.  The map must have been allocated from the worker's
 * global context and should have been allocated from the same worker.
 *
 * @param w __cilkrts_worker the cilkred_map was created for.
 * @param h The cilkred_map to be deallocated.
 */
COMMON_SYSDEP
void __cilkrts_destroy_reducer_map(__cilkrts_worker *w,
                                   cilkred_map *h);

/**
 * Set the specified reducer map as the leftmost map if is_leftmost is true,
 * otherwise, set it to not be the leftmost map.
 *
 * @param h The cilkred_map to be modified.
 * @param is_leftmost true if the reducer map is leftmost.
 */
COMMON_SYSDEP
void __cilkrts_set_leftmost_reducer_map(cilkred_map *h,
                                        int is_leftmost);

/**
 * Merge reducer map RIGHT_MAP into LEFT_MAP and return the result of the
 * merge.  Both maps must be allocated from the global context associated
 * with the specified worker.  The returned reducer map must be destroyed
 * before the worker's associated global context is destroyed.
 *
 * If two cilkred_maps are specified, one will be destroyed and the other
 * one will be returned as the merged cilkred_map.
 *
 * When reducers can contain nested parallelism, execution can return
 * on a different worker than when it started (but still using the
 * same stack).
 *
 * Upon return, *w_ptr stores the pointer to the worker that execution
 * returns on.
 *
 * @param w_ptr      Pointer to the currently executing worker.
 * @param left_map   The left cilkred_map.
 * @param right_map  The right cilkred_map.
 *                  
 * @return pointer to merged cilkred_map.
 */
extern
cilkred_map *merge_reducer_maps(__cilkrts_worker **w_ptr,
				cilkred_map *left_map,
				cilkred_map *right_map);

/**
 * Similar to merge_reducer_maps(), except that after merging
 * RIGHT_MAP into LEFT_MAP, it repeatedly merges (*w_ptr)->reducer_map
 * into LEFT_MAP.  This procedure ensures that any new reducers
 * created by the reductions themselves also get merged into LEFT_MAP.
 */ 
extern
cilkred_map *repeated_merge_reducer_maps(__cilkrts_worker **w_ptr,
					 cilkred_map *left_map,
					 cilkred_map *right_map);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_REDUCER_IMPL_DOT_H)
