/* full_frame.c                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2010-2013, Intel Corporation
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
 *
 **************************************************************************/

#include "full_frame.h"
#include "stats.h"
#include "os.h"
#include "bug.h"
#include "jmpbuf.h"
#include "frame_malloc.h"

COMMON_PORTABLE
full_frame *__cilkrts_make_full_frame(__cilkrts_worker *w,
                                      __cilkrts_stack_frame *sf)
{
    full_frame *ff;

    START_INTERVAL(w, INTERVAL_ALLOC_FULL_FRAME) {
        ff = (full_frame *)__cilkrts_frame_malloc(w, sizeof(*ff));
        __cilkrts_mutex_init(&ff->lock);

        ff->full_frame_magic_0 = FULL_FRAME_MAGIC_0;
        ff->join_counter = 0;
        ff->parent = 0;
        ff->rightmost_child = 0;
        ff->left_sibling = ff->right_sibling = 0;
        ff->call_stack = sf;
        ff->is_call_child = 0;
        ff->simulated_stolen = 0;
	ff->children_reducer_map = ff->right_reducer_map = 0;
        ff->pending_exception = 
            ff->child_pending_exception = 
            ff->right_pending_exception = NULL;

        ff->sync_sp = 0;
#ifdef _WIN32
        ff->exception_sp = 0;
        ff->trylevel = (unsigned long)-1;
        ff->registration = 0;
#endif
	ff->frame_size = 0;
        ff->fiber_self = 0;
        ff->fiber_child = 0;

        ff->sync_master = 0;

        /*__cilkrts_init_full_frame_sysdep(w, ff);*/
        ff->full_frame_magic_1 = FULL_FRAME_MAGIC_1;
    } STOP_INTERVAL(w, INTERVAL_ALLOC_FULL_FRAME);
    return ff;
}

COMMON_PORTABLE void __cilkrts_put_stack(full_frame *ff,
                                         __cilkrts_stack_frame *sf)
{
    /* When suspending frame ff prior to stealing it, __cilkrts_put_stack is
     * used to store the stack pointer for eventual sync.  When suspending
     * frame ff prior to a sync, __cilkrts_put_stack is called to re-establish
     * the sync stack pointer, offsetting it by any change in the stack depth
     * that occured between the spawn and the sync.
     * Although it is not usually meaningful to add two pointers, the value of
     * ff->sync_sp at the time of this call is really an integer, not a
     * pointer.
     */
    ptrdiff_t sync_sp_i = (ptrdiff_t) ff->sync_sp;
    char* sp = (char*) __cilkrts_get_sp(sf);

    ff->sync_sp = sp + sync_sp_i;

    DBGPRINTF("%d-                __cilkrts_put_stack - adjust (+) sync "
              "stack of full frame %p (+sp: %p) to %p\n",
              __cilkrts_get_tls_worker()->self, ff, sp, ff->sync_sp);
}

COMMON_PORTABLE void __cilkrts_take_stack(full_frame *ff, void *sp)
{
    /* When resuming the parent after a steal, __cilkrts_take_stack is used to
     * subtract the new stack pointer from the current stack pointer, storing
     * the offset in ff->sync_sp.  When resuming after a sync,
     * __cilkrts_take_stack is used to subtract the new stack pointer from
     * itself, leaving ff->sync_sp at zero (null).  Although the pointers being
     * subtracted are not part of the same contiguous chunk of memory, the
     * flat memory model allows us to subtract them and get a useable offset.
     */
    ptrdiff_t sync_sp_i = ff->sync_sp - (char*) sp;

    ff->sync_sp = (char *) sync_sp_i;

    DBGPRINTF("%d-                __cilkrts_take_stack - adjust (-) sync "
              "stack of full frame %p to %p (-sp: %p)\n",
              __cilkrts_get_tls_worker()->self, ff, ff->sync_sp, sp);
}

COMMON_PORTABLE void __cilkrts_adjust_stack(full_frame *ff, size_t size)
{
    /* When resuming the parent after a steal, __cilkrts_take_stack is used to
     * subtract the new stack pointer from the current stack pointer, storing
     * the offset in ff->sync_sp.  When resuming after a sync,
     * __cilkrts_take_stack is used to subtract the new stack pointer from
     * itself, leaving ff->sync_sp at zero (null).  Although the pointers being
     * subtracted are not part of the same contiguous chunk of memory, the
     * flat memory model allows us to subtract them and get a useable offset.
     *
     * __cilkrts_adjust_stack() is used to deallocate a Variable Length Array
     * by adding it's size to ff->sync_sp.
     */
    ff->sync_sp = ff->sync_sp + size;

    DBGPRINTF("%d-                __cilkrts_adjust_stack - adjust (+) sync "
              "stack of full frame %p to %p (+ size: 0x%x)\n",
              __cilkrts_get_tls_worker()->self, ff, ff->sync_sp, size);
}

COMMON_PORTABLE
void __cilkrts_destroy_full_frame(__cilkrts_worker *w, full_frame *ff)
{
    validate_full_frame(ff);
    CILK_ASSERT(ff->children_reducer_map == 0);
    CILK_ASSERT(ff->right_reducer_map == 0);
    CILK_ASSERT(NULL == ff->pending_exception);
    CILK_ASSERT(NULL == ff->child_pending_exception);
    CILK_ASSERT(NULL == ff->right_pending_exception);
    __cilkrts_mutex_destroy(w, &ff->lock);
    __cilkrts_frame_free(w, ff, sizeof(*ff));
}

COMMON_PORTABLE void validate_full_frame(full_frame *ff)
{
    /* check the magic numbers, for debugging purposes */
    if (ff->full_frame_magic_0 != FULL_FRAME_MAGIC_0 ||
        ff->full_frame_magic_1 != FULL_FRAME_MAGIC_1)
        abort_because_rts_is_corrupted();
}

void __cilkrts_frame_lock(__cilkrts_worker *w, full_frame *ff)
{
    validate_full_frame(ff);
    __cilkrts_mutex_lock(w, &ff->lock);
}

void __cilkrts_frame_unlock(__cilkrts_worker *w, full_frame *ff)
{
    __cilkrts_mutex_unlock(w, &ff->lock);
}

/* End full_frame.c */
