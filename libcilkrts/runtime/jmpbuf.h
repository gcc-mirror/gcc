/* jmpbuf.h                  -*-C++-*-
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
 * @file jmpbuf.h
 *
 * @brief Macros and functions to access the _JUMP_BUFFER initialized by a 
 * call to CILK_SETJMP before a cilk_spawn or cilk_sync.  The definition of
 * CILK_SETJMP and CILK_LONGJMP are OS dependent and in abi.h
 *
 */

#ifndef INCLUDED_JMPBUF_DOT_H
#define INCLUDED_JMPBUF_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>
#include <stddef.h>
#include <setjmp.h>

#if 0 /* defined CILK_USE_C_SETJMP && defined JB_RSP */
#   define JMPBUF_SP(ctx) (ctx)[0].__jmpbuf[JB_RSP]
#   define JMPBUF_FP(ctx) (ctx)[0].__jmpbuf[JB_RBP]
#   define JMPBUF_PC(ctx) (ctx)[0].__jmpbuf[JB_PC]
#elif 0 /* defined CILK_USE_C_SETJMP && defined JB_SP */
#   define JMPBUF_SP(ctx) (ctx)[0].__jmpbuf[JB_SP]
#   define JMPBUF_FP(ctx) (ctx)[0].__jmpbuf[JB_BP]
#   define JMPBUF_PC(ctx) (ctx)[0].__jmpbuf[JB_PC]
#elif defined _WIN64
#   define JMPBUF_SP(ctx) ((_JUMP_BUFFER*)(&(ctx)))->Rsp
#   define JMPBUF_FP(ctx) ((_JUMP_BUFFER*)(&(ctx)))->Rbp
#   define JMPBUF_PC(ctx) ((_JUMP_BUFFER*)(&(ctx)))->Rip
#elif defined _WIN32
    /** Fetch stack pointer from a __cilkrts_stack_frame */
#   define JMPBUF_SP(ctx) (ctx).Esp
    /** Fetch frame pointer from a __cilkrts_stack_frame */
#   define JMPBUF_FP(ctx) (ctx).Ebp
    /** Fetch program counter from a __cilkrts_stack_frame */
#   define JMPBUF_PC(ctx) (ctx).Eip
#else /* defined __GNUC__ || defined __ICC */
    /* word 0 is frame address
     * word 1 is resume address
     * word 2 is stack address */
#   define JMPBUF_FP(ctx) (ctx)[0]
#   define JMPBUF_PC(ctx) (ctx)[1]
#   define JMPBUF_SP(ctx) (ctx)[2]
#endif

/**
 * @brief Get frame pointer from jump buffer in__cilkrts_stack_frame.
 */
#define FP(SF) JMPBUF_FP((SF)->ctx)

/**
 * @brief Get program counter from jump buffer in__cilkrts_stack_frame.
 */
#define PC(SF) JMPBUF_PC((SF)->ctx)

/**
 * @brief Get stack pointer from jump buffer in__cilkrts_stack_frame.
 */
#define SP(SF) JMPBUF_SP((SF)->ctx)


__CILKRTS_BEGIN_EXTERN_C

/**
 * Fetch the stack pointer from a __cilkrts_stack_frame.  The jmpbuf was
 * initialized before a cilk_spawn or cilk_sync.
 *
 * @param sf __cilkrts_stack_frame containing the jmpbuf.
 *
 * @return the stack pointer from the ctx.
 */
inline char *__cilkrts_get_sp(__cilkrts_stack_frame *sf)
{
    return (char *)SP(sf);
}

/**
 * Calculate the frame size from __cilkrts_stack_frame.  The jmpbuf was
 * initialized before a cilk_spawn or cilk_sync.
 *
 * @warning Returning an arbitrary value on Windows!
 *
 * @param sf __cilkrts_stack_frame containing the jmpbuf.
 *
 * @return the stack pointer from the ctx.
 */
inline ptrdiff_t __cilkrts_get_frame_size(__cilkrts_stack_frame *sf)
{
#ifdef _WIN32
    if (0 == SP(sf))
        return 256;         // Arbitrary!
#endif
    return (ptrdiff_t)FP(sf) - (ptrdiff_t)SP(sf);
}

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_JMPBUF_DOT_H)
