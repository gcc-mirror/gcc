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
 * __cilkrts_stack_alloc() and __cilkrts_stack_free must be compiled
 * such that ebp/rbp is used for the stack frames.  This is done by having
 * each of them use alloca, which forces the special frame types needed on
 * each of the ABIs.  Additionally, for some forms of stack frame, special
 * care must be taken because the alloca space may not be at the bottom of the
 * stack frame of the caller.  For Intel64 windows, and for some options
 * with other ABIs, a preallocated parameter block may exist on the stack
 * at a lower address than the alloca.  If this is the case, the parameter
 * distance_from_sp_to_alloca_area will be non-zero, and will indicate how
 * much pre-allocated parameter space resides in the caller's stack frame
 * between the alloca area, and the bottom of the stack when the call to
 * the cilkrts is made.  As such, when non-zero it also includes any space
 * used for passing the cilkrts_stack_alloc or cilkrts_stack_free parameters.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#ifdef _WIN32
# define alloca _alloca
# define INLINE static __inline
# pragma warning(disable:1025)  // Don't whine about zero extending result of unary operation
#else
# include <alloca.h>
# define INLINE static inline
#endif

#include "internal/abi.h"
#include "cilk-abi-vla-internal.h"

#if defined(__x86_64) || defined(_M_X64)
INLINE void setsp(void *val)
{
    __asm__("movq %0, %%rsp" : : "r"(val): "rsp");
}
INLINE char* getsp(void)
{
    void *res;

    __asm__("movq %%rsp, %0" : "=r"(res): : "rsp");
    return res;
}
INLINE char* getbp(void)
{
    void *res;

    __asm__("movq %%rbp, %0" : "=r"(res): : "rbp");
    return res;
}
INLINE void copy_frame_down_and_move_bp(
    char *dst,
    char *src,
    size_t cpy_bytes,
    char *new_ebp
)
{
    // In this version, dst is guaranteed to be lower address than src,
    // therefore copying upwards from src into dst is safe in case
    // there is overlap. The number of bytes is also guaranteed to be
    // a multiple of 8, and the copy is done in 64 bit word chunks for
    // best efficiency.
    __asm__(
        "movq %0, %%rdi;"
        "movq %1, %%rsi;"
        "movq %2, %%rcx;"
        "shrq $3, %%rcx;"
        "rep movsq;"
        "movq %3, %%rbp" : 
        :
        "rm"(dst), "rm"(src), "rm"(cpy_bytes), "rm"(new_ebp) :
        "rsi", "rdi", "rcx", "rbp", "memory");
}
INLINE void copy_frame_up_and_move_bp(
    char *dst,
    char *src,
    size_t cpy_bytes,
    char *new_ebp
)
{
    // In this version, dst is guaranteed to be higher address than src,
    // therefore copying downwards from src into dst is safe in case
    // there is overlap. The number of bytes is also guaranteed to be
    // a multiple of 8, and the copy is done in 64 bit word chunks for
    // best efficiency.
    dst += cpy_bytes - 8;
    src += cpy_bytes - 8;
    __asm__(
        "movq %0, %%rdi;"
        "movq %1, %%rsi;"
        "movq %2, %%rcx;"
        "shrq $3, %%rcx;"
        "std; rep movsq; cld;"
        "movl %3, %%rbp;" : 
        :
        "rm"(dst), "rm"(src), "rm"(cpy_bytes), "rm"(new_ebp) :
        "rsi", "rdi", "rcx", "rbp", "memory");
}
#else
INLINE void setsp(void *val)
{
    __asm__("movl %0, %%esp" : : "r"(val): "esp");
}
INLINE char* getsp(void)
{
    void *res;

    __asm__("movl %%esp, %0" : "=r"(res): : "esp");
    return res;
}
INLINE char* getbp(void)
{
    void *res;

    __asm__("movl %%ebp, %0" : "=r"(res): : "ebp");
    return res;
}
INLINE void copy_frame_down_and_move_bp(
    char *dst,
    char *src,
    size_t cpy_bytes,
    char *new_ebp
)
{
    // In this version, dst is guaranteed to be lower address than src,
    // therefore copying upwards from src into dst is safe in case
    // there is overlap. The number of bytes is also guaranteed to be
    // a multiple of 4, and the copy is done in 32 bit word chunks for
    // best efficiency.
    __asm__(
        "movl %0, %%edi;"
        "movl %1, %%esi;"
        "movl %2, %%ecx;"
        "shrl $2, %%ecx;"
        "rep movsd;"
        "movl %3, %%ebp" : 
        :
        "rm"(dst), "rm"(src), "rm"(cpy_bytes), "rm"(new_ebp) :
        "esi", "edi", "ecx", "ebp", "memory");
}
INLINE void copy_frame_up_and_move_bp(
    char *dst,
    char *src,
    size_t cpy_bytes,
    char *new_ebp
)
{
    // In this version, dst is guaranteed to be higher address than src,
    // therefore copying downwards from src into dst is safe in case
    // there is overlap. The number of bytes is also guaranteed to be
    // a multiple of 4, and the copy is done in 32 bit word chunks for
    // best efficiency.
    dst += cpy_bytes - 4;
    src += cpy_bytes - 4;
    __asm__(
        "movl %0, %%edi;"
        "movl %1, %%esi;"
        "movl %2, %%ecx;"
        "shrl $2, %%ecx;"
        "std; rep movsd; cld;"
        "movl %3, %%ebp" : 
        // "=D"(dst), "=S"(src), "=C"(cpy_bytes) :
        :
        "rm"(dst), "rm"(src), "rm"(cpy_bytes), "rm"(new_ebp) :
        "esi", "edi", "ecx", "ebp", "memory");
}
#endif


#define c_cilk_ptr_from_heap  0xc2f2f00d
#define c_cilk_ptr_from_stack 0xc3f30d0f

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
#ifdef __INTEL_COMPILER
    // full_size will be a multiple of align, and contains
    // enough extra space to allocate a marker.
    size_t full_size = (size + align - 1) & ~(align - 1);

    if (needs_tag) {
        full_size += align;
    }

    char *t;
    if (sf->worker != 0 &&
        ((sf->flags & CILK_FRAME_UNSYNCHED) != 0)) {
        t = vla_internal_heap_alloc(sf, full_size, align);
        if (needs_tag) {
            t += align;
            ((uint32_t*)t)[-1] = c_cilk_ptr_from_heap;
        }
        return (void *)t;
    }
    
    // stack is still synced, allocate full_size from esp,
    // and record in 32 bits immediately below the space
    // allocated that this was space that this was
    // allocated in the stack.
    char *old_ebp = getbp();
    char *old_esp = getsp();

    // make top_ptr point to base of first parameter.
    char *top_ptr = ((char *)(_AddressOfReturnAddress()) +
                    sizeof(char *));
    size_t param_size = 0;

#if defined(__x86_64)
    // For Intel64 linux & MACH ABI, all the parameters were passed in
    // register, so top of the stack frame above the return address
    // is just the size of the return address plus
    // distance_from_sp_to_alloca_area on the chance that the alloca
    // area isn't at the very bottom of the calling functions stack.
#elif defined(__MACH__)
    // For ia32 MACH, parameter size is always a mutliple of 16
    // bytes to keep the stack 16 byte aligned.  So we need to round
    // number of parameters up to multiple of 4.
    param_size = 8 * sizeof(char *);
#else
    // For both windows Intel64 ABI, and the IA32 windows and
    // linux ABIs, space is reserved on the stack for all these
    // parameters.  param_size is 5 * size of a stack slot.
    param_size = 5 * sizeof(char *);
#endif

    // now make top_ptr point above the params, or if
    // distance_from_sp_to_alloca_area is not zero, make
    // it point above that area.  When non-zero,
    // distance_from_sp_to_alloca area is expected to contain
    // the parameter space, so we only add one or the other,
    // not both.
    top_ptr += (distance_from_sp_to_alloca_area != 0) ?
                   distance_from_sp_to_alloca_area : param_size;
    
    // t needs to end up at current value of top_ptr less full_size and less
    // distance_from_sp_to_alloca_area and
    // then rounded down to the alignment needed.  Then we have to bump
    // esp down by current frame_size, so that when all is done with respect
    // to executing the return sequence, the final value of esp will be the
    // same value as t.
    t = (top_ptr - full_size) - distance_from_sp_to_alloca_area;
    intptr_t temp = (intptr_t)t;
    temp &= ~((intptr_t)(align - 1));
    t = (char *)temp;

    // ok, the value of t is set where we need it.  Now set esp
    // to the value of t less the current frame size.
    // So now when we do regular return esp should be left such
    // that it has moved down by full_size.
    size_t cur_fm_size = (top_ptr - old_esp);
    char *new_esp = t - cur_fm_size;
    char *new_ebp = old_ebp - (old_esp - new_esp);

    // extend the stack down by at least the difference between where
    // I want it to be and where it currently is.  This should take care
    // of touching any pages necessary.
    char *foo = alloca(old_esp - new_esp);
    setsp(foo < new_esp ? foo : new_esp);

    // Now set esp exactly where I want it.
    // setsp(new_esp);

    copy_frame_down_and_move_bp(new_esp, old_esp, cur_fm_size, new_ebp);

    if (needs_tag) {
        t += align;
        ((uint32_t*)t)[-1] = c_cilk_ptr_from_stack;
    }

    return t;
#else // Not __INTEL_COMPILER
    // Not supported unless we can figure out how to get the size of the frame
    return NULL;
#endif
}

// This frees the space allocated for a variable length array.
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
#ifdef __INTEL_COMPILER
    uint32_t *t = (uint32_t*)p;

    // full_size will be a multiple of align, and contains
    // enough extra space to allocate a marker if one was needed.
    size_t full_size = (size + align - 1) & ~(align - 1);
    if (known_from_stack == 0) {
        // if the compiler hasn't told the run-time that this is
        // known to be on the stack, then this pointer must have been
        // tagged such that the run-time can tell.
        assert(t[-1] == c_cilk_ptr_from_stack ||
               t[-1] == c_cilk_ptr_from_heap);

        known_from_stack = t[-1] == c_cilk_ptr_from_stack;
        full_size += align;    // accounts for extra space for marker
        t = (uint32_t *)(((char *)t) - align);
    }

    if (known_from_stack) {
        // alloca useage forces an ebp/rbp based stack frame even though
        // 0 and unused.
        char *foo = alloca(0);
        if (sf->worker == 0 || (sf->flags & CILK_FRAME_UNSYNCHED) == 0) {
            // p was allocated from current stack frame and we
            // are synced on current stack frame.  Return the
            // amount of the stack that needs to be freed.
            char *old_ebp = getbp();
            char *old_esp = getsp();

            // make top_ptr point to base of first parameter.
            char *top_ptr = ((char *)(_AddressOfReturnAddress()) +
                            sizeof(char *));
            size_t param_size = 0;

#if defined(__x86_64)
            // For Intel64 linux & MACH ABI, all the parameters were passed in
            // register, so top of the stack frame above the return address
            // is just the size of the return address plus
            // distance_from_sp_to_alloca_area on the chance that the alloca
            // area isn't at the very bottom of the calling functions stack.
#elif defined(__MACH__)
            // For ia32 MACH, parameter size is always a mutliple of 16
            // bytes to keep the stack 16 byte aligned.  So we need to round
            // number of parameters up to multiple of 4.
            param_size = 8 * sizeof(char *);
#else
            // For both windows Intel64 ABI, and the IA32 windows and
            // linux ABIs, space is reserved on the stack for all these
            // parameters.  param_size is 5 * size of a stack slot.
            param_size = 6 * sizeof(char *);
#endif

            // now make top_ptr point above the params, or if
            // distance_from_sp_to_alloca_area is not zero, make
            // it point above that area.  When non-zero,
            // distance_from_sp_to_alloca area is expected to contain
            // the parameter space, so we only add one or the other,
            // not both.
            top_ptr += (distance_from_sp_to_alloca_area != 0) ?
                           distance_from_sp_to_alloca_area : param_size;

            size_t cur_fm_size = (top_ptr - old_esp);
            char *new_esp = old_esp + full_size;
            char *new_ebp = old_ebp + full_size;

            copy_frame_up_and_move_bp(new_esp, old_esp, cur_fm_size, new_ebp);
            setsp(new_esp);
        }
        else {
            // p was allocated on stack frame, but that is
            // no longer the current stack frame.  Need to adjust the
            // saved esp that is somewhere in the cilk runtime so that
            // on sync, esp will be cut back correctly.
            vla_free_from_original_stack(sf, full_size);
        }
    }
    else {
        vla_internal_heap_free(t, full_size);
    }
#else // Not __INTEL_COMPILER
    // Not supported unless we can figure out how to get the size of the frame
#endif
}
