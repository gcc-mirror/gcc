/* cilk-abi-vla.cpp                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2013, Intel Corporation
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

/*
 * Implementation of Variable Length Array (VLA) ABI.
 *
 * The compiler calls these functions to allocate Variable Length Arrays
 * at runtime.  The compiler must guarantee that __cilkrts_stack_free() is
 * called to cleanup any memory allocated by __cilkrts_stack_alloc().
 *
 * This generic implementation always allocates the memory from the heap.
 * Optimally, the implementation should expand the frame of the calling
 * function if possible, since that will be faster.  See the x86 version
 * for one possible implementation.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

#include "internal/abi.h"
#include "cilk-abi-vla-internal.h"

#define c_cilk_ptr_from_heap  0xc2f2f00d
#define c_cilk_ptr_from_stack 0xc3f30d0f

// Allocate space for a variable length array
CILK_ABI(__cilkrts_void_ptr)
__cilkrts_stack_alloc(
    __cilkrts_stack_frame *sf,
    size_t size,
    size_t distance_from_sp_to_alloca_area,
    uint32_t align,     // align is always >= minimum stack alignment and
                        // >= ptr_size as well, and must be a power of 2.
    uint32_t needs_tag  // non-zero if the pointer being returned needs to
                        // be tagged
)
{
    // full_size will be a multiple of align, and contains
    // enough extra space to allocate a marker.
    size_t full_size = (size + align - 1) & ~(align - 1);

    // Allocate memory from the heap.  The compiler is responsible
    // for guaranteeing us a chance to free it before the function
    // exits

    return (void *)vla_internal_heap_alloc(sf, full_size, align);
}

// Free the space allocated for a variable length array.
CILK_ABI(void)
__cilkrts_stack_free(
    __cilkrts_stack_frame *sf,
    void *p,
    size_t size,
    size_t distance_from_sp_to_alloca_area,
    uint32_t align, // same requirements as for align in allocation,
                    // and must match alignment that was passed when
                    // doing the allocation 
    uint32_t known_from_stack  // non-zero if this is known to be allocated
                               // on the stack, and therefore has no tag
)
{
    // full_size will be a multiple of align, and contains
    // enough extra space to allocate a marker if one was needed.
    size_t full_size = (size + align - 1) & ~(align - 1);

    // Just free the allocated memory to the heap since we don't know
    // how to expand/contract the calling frame
    vla_internal_heap_free(t, full_size);
}
