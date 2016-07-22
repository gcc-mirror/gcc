/* cilk-abi-vla-internal.c        -*-C++-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2013-2016, Intel Corporation
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

/*
 * These functions are provided in their own compilation unit so I can debug
 * them.  cilk-abi-vla.c must always be compiled with optimization on so that
 * inlining occurs.
 */

#include "internal/abi.h"
#include "cilk-abi-vla-internal.h"
#include "bug.h"
#include "full_frame.h"
#include "local_state.h"

#include <stdlib.h>
#include <stdint.h>

#include "bug.h"

void *vla_internal_heap_alloc(__cilkrts_stack_frame *sf,
                              size_t full_size,
                              uint32_t align)
{
    return malloc(full_size);
}

void vla_internal_heap_free(void *t, size_t size)
{
    free(t);
}

void vla_free_from_original_stack(__cilkrts_stack_frame *sf,
                                  size_t full_size)
{
    // The __cilkrts_stack_frame must be initialized
    CILK_ASSERT(sf->worker);

#if 1
    // Add full_size to ff->sync_sp so that when we return, the VLA will no
    // longer be allocated on the stack
    __cilkrts_adjust_stack(sf->worker->l->frame_ff, full_size);
#else
    // Inline __cilkrts_adjust_stack for Kevin
    full_frame *ff = sf->worker->l->frame_ff;
    ff->sync_sp = ff->sync_sp + full_size;
#endif
}
