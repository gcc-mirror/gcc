/* sysdep.h                  -*-C++-*-
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
 * @file sysdep.h
 *
 * @brief Common system-dependent functions
 */

#ifndef INCLUDED_SYSDEP_DOT_H
#define INCLUDED_SYSDEP_DOT_H

#include <cilk/common.h>
#include <internal/abi.h>

#include "global_state.h"
#include "full_frame.h"
#include "os.h"
#include "os_mutex.h"

/**
 * @brief Default page size for Cilk stacks.
 *
 * All Cilk stacks should have size that is a multiple of this value.
 */
#define PAGE 4096

/**
 * @brief Size of a scheduling stack.
 *
 * A scheduling stack is used to by system workers to execute runtime
 * code.  Since this stack is only executing runtime functions, we
 * don't need it to be a full size stack.
 *
 * The number "18" should be small since the runtime doesn't require a
 * large stack, but large enough to call "printf" for debugging.
 */ 
#define CILK_SCHEDULING_STACK_SIZE (18*PAGE)

__CILKRTS_BEGIN_EXTERN_C


/**
 * Code to initialize the system-dependent portion of the global_state_t
 *
 * @param g Pointer to the global state.
 */
COMMON_SYSDEP
void __cilkrts_init_global_sysdep(global_state_t *g);

/**
 * Code to clean up the system-dependent portion of the global_state_t
 *
 * @param g Pointer to the global state.
 */
COMMON_SYSDEP
void __cilkrts_destroy_global_sysdep(global_state_t *g);

/**
 * Passes stack range to Cilkscreen.  This functionality should be moved
 * into Cilkscreen.
 */
COMMON_SYSDEP
void __cilkrts_establish_c_stack(void);


/**
 * Save system dependent information in the full_frame and
 * __cilkrts_stack_frame.  Part of promoting a
 * __cilkrts_stack_frame to a full_frame.
 *
 * @param w The worker the frame was running on.  Not used.
 * @param ff The full frame that is being created for the
 * __cilkrts_stack_frame.
 * @param sf The __cilkrts_stack_frame that's being promoted
 * to a full frame.
 * @param state_valid ?
 * @param why A description of why make_unrunnable was called.
 * Used for debugging.
 */
COMMON_SYSDEP
void __cilkrts_make_unrunnable_sysdep(__cilkrts_worker *w,
                                      full_frame *ff,
                                      __cilkrts_stack_frame *sf,
                                      int state_valid,
                                      const char *why);


/**
 * OS-specific code to spawn worker threads.
 *
 * @param g The global state.
 * @param n Number of worker threads to start.
 */
COMMON_SYSDEP
void __cilkrts_start_workers(global_state_t *g, int n);

/**
 * @brief OS-specific code to stop worker threads.
 *
 * @param g The global state.
 */
COMMON_SYSDEP
void __cilkrts_stop_workers(global_state_t *g);

/**
 * @brief Imports a user thread the first time it returns to a stolen parent.
 *
 * The thread has been bound to a worker, but additional steps need to
 * be taken to start running a scheduling loop.
 *
 * @param w The worker bound to the thread.
 */
COMMON_SYSDEP
void __cilkrts_sysdep_import_user_thread(__cilkrts_worker *w);

/**
 * @brief Function to be run for each of the system worker threads.
 * 
 * This declaration also appears in cilk/cilk_undocumented.h -- don't
 * change one declaration without also changing the other.
 *
 * @param arg The context value passed to the thread creation routine for
 * the OS we're running on.
 *
 * @returns OS dependent.
 */
#ifdef _WIN32
/* Do not use CILK_API because __cilkrts_worker_stub must be __stdcall */
CILK_EXPORT unsigned __CILKRTS_NOTHROW __stdcall
__cilkrts_worker_stub(void *arg);
#else
/* Do not use CILK_API because __cilkrts_worker_stub have default visibility */
__attribute__((visibility("default")))
void* __CILKRTS_NOTHROW __cilkrts_worker_stub(void *arg);
#endif

/**
 * Initialize any OS-depenendent portions of a newly created
 * __cilkrts_worker.
 *
 * Exported for Piersol.  Without the export, Piersol doesn't display
 * useful information in the stack trace.  This declaration also appears in
 * cilk/cilk_undocumented.h -- do not modify one without modifying the other.
 *
 * @param w The worker being initialized.
 */
COMMON_SYSDEP
CILK_EXPORT
void __cilkrts_init_worker_sysdep(__cilkrts_worker *w);

/**
 * Deallocate any OS-depenendent portions of a __cilkrts_worker.
 *
 * @param w The worker being deallocaed.
 */
COMMON_SYSDEP
void __cilkrts_destroy_worker_sysdep(__cilkrts_worker *w);

/**
 * Called to do any OS-dependent setup before starting execution on a
 * frame. Mostly deals with exception handling data.
 *
 * @param w The worker the frame will run on.
 * @param ff The full_frame that is about to be resumed.
 */
COMMON_SYSDEP
void __cilkrts_setup_for_execution_sysdep(__cilkrts_worker *w,
                                          full_frame *ff);

/**
 * @brief OS-specific implementaton of resetting fiber and frame state
 * to resume exeuction.
 *
 * This method:
 *  1. Calculates the value of stack pointer where we should resume
 *     execution of "sf".  This calculation uses info stored in the
 *     fiber, and takes into account alignment and frame size.
 *  2. Updates sf and ff to match the calculated stack pointer.
 *
 *  On Unix, the stack pointer calculation looks up the base of the
 *  stack from the fiber.
 *
 *  On Windows, this calculation is calls "alloca" to find a stack
 *  pointer on the currently executing stack.  Thus, the Windows code
 *  assumes @c fiber is the currently executing fiber.
 *
 * @param fiber   fiber to resume execution on.
 * @param ff      full_frame for the frame we're resuming.
 * @param sf      __cilkrts_stack_frame that we should resume
 * @return    The calculated stack pointer.
 */
COMMON_SYSDEP
char* sysdep_reset_jump_buffers_for_resume(cilk_fiber* fiber,
                                           full_frame *ff,
                                           __cilkrts_stack_frame *sf);

/**
 * @brief System-dependent longjmp to user code for resuming execution
 *   of a @c __cilkrts_stack_frame.
 *
 * This method:
 *  - Changes the stack pointer in @c sf to @c new_sp.
 *  - If @c ff_for_exceptions is not NULL, changes fields in @c sf and
 *    @c ff_for_exceptions for exception processing.
 *  - Restores any floating point state
 *  - Finishes with a longjmp to user code, never to return. 
 *
 * @param new_sp             stack pointer where we should resume execution
 * @param sf                 @c __cilkrts_stack_frame for the frame we're resuming.
 * @param ff_for_exceptions  full_frame to safe exception info into, if necessary
 */
COMMON_SYSDEP
NORETURN
sysdep_longjmp_to_sf(char* new_sp,
                     __cilkrts_stack_frame *sf,
                     full_frame *ff_for_exceptions);

/**
 * @brief System-dependent code to save floating point control information
 * to a @c __cilkrts_stack_frame.  This function will be called by compilers
 * that cannot inline the code.
 *
 * Note that this function does *not* save the current floating point
 * registers.  It saves the floating point control words that control
 * precision and rounding and stuff like that.
 *
 * This function will be a noop for architectures that don't have warts
 * like the floating point control words, or where the information is
 * already being saved by the setjmp.
 *
 * @param sf                 @c __cilkrts_stack_frame for the frame we're
 * saving the floating point control information in.
 */
COMMON_SYSDEP
void
sysdep_save_fp_ctrl_state(__cilkrts_stack_frame *sf);


/**
 * @brief restore x86 floating point state
 *
 * Only used for x86 and Intel64 processors
 */
COMMON_SYSDEP
void restore_x86_fp_state(__cilkrts_stack_frame *sf);

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_SYSDEP_DOT_H)
