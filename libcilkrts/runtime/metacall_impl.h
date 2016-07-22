/* metacall_impl.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  Copyright (C) 2010-2016, Intel Corporation
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
 *
 **************************************************************************/

/**
 * @file metacall_impl.h
 *
 * @brief Meta-function calls to be used within the Cilk runtime system.
 * 
 * These differ from the macros in cilkscreen.h and cilkview.h because they go
 * through the __cilkrts_metacall interface, which ensures that the operation
 * is performed even when instrumentation is disabled.
 */

#ifndef INCLUDED_CILKRTS_METACALL_H
#define INCLUDED_CILKRTS_METACALL_H

#include "rts-common.h"
#include <internal/metacall.h>
#include <cilk/common.h>

__CILKRTS_BEGIN_EXTERN_C

/**
 * This function is effectively an unconditional call from the runtime into
 * a tool.  It is used for operations that must be performed by the tool,
 * even when the tool is not instrumenting.  For example, Cilkscreen always
 * recognizes the address of this function and performs the action specified
 * in the contained metadata.
 *
 * Note that this function MUST NOT BE INLINED within the runtime.  This must
 * be the ONLY instance of the cilkscreen_metacall metadata.
 */
CILK_API_VOID
__cilkrts_metacall(unsigned int tool, unsigned int code, void *data);

/**
 * Return non-zero if running under Cilkscreen or Cilkview
 */
COMMON_PORTABLE
int __cilkrts_running_under_sequential_ptool(void);

/**
 * Disable Cilkscreen implementation
 */
#define __cilkrts_cilkscreen_disable_instrumentation() \
    __cilkrts_metacall(METACALL_TOOL_SYSTEM, HYPER_DISABLE_INSTRUMENTATION, 0)

/**
 * Enable Cilkscreen implementation
 */
#define __cilkrts_cilkscreen_enable_instrumentation() \
    __cilkrts_metacall(METACALL_TOOL_SYSTEM, HYPER_ENABLE_INSTRUMENTATION, 0)

/**
 * Set the worker on entering runtime.
 *
 * @attention Deprecated in favor of __cilkrts_cilkscreen_ignore_block.  The
 * begin/enter pairs in the current metadata mean Cilkscreen no longer has to
 * have improper knowledge of the __cilkrts_worker or __cilkrts_stack_frame
 * structures.
 */
#define __cilkrts_cilkscreen_establish_worker(w) \
    __cilkrts_metacall(METACALL_TOOL_SYSTEM, HYPER_ESTABLISH_WORKER, w)

/**
 * Notify Cilkscreen of the extent of the stack.
 *
 * @param[in] begin Start (low address) of stack
 * @param[in] end   One past high address of stack
 */
void __cilkrts_cilkscreen_establish_c_stack(char *begin, char *end);

/**
 * Tell tools to ignore a block of memory - currently the global state and
 * memory allocated for workers.
 */
#define __cilkrts_cilkscreen_ignore_block(_begin, _end) \
{                                                       \
    void *block[2] = {_begin, _end};                    \
    __cilkrts_metacall(METACALL_TOOL_SYSTEM,            \
                       HYPER_IGNORE_MEMORY_BLOCK,       \
                       block);                          \
}

__CILKRTS_END_EXTERN_C

#endif /* ! defined(INCLUDED_CILKRTS_METACALL_H) */
