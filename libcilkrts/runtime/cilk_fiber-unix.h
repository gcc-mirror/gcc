/* cilk_fiber-unix.h                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2012-2013, Intel Corporation
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

#ifndef INCLUDED_CILK_FIBER_UNIX_DOT_H
#define INCLUDED_CILK_FIBER_UNIX_DOT_H

#ifndef __cplusplus
#   error cilk_fiber-unix.h is a C++-only header
#endif

#include "cilk_fiber.h"
#include "jmpbuf.h"

/**
 * @file cilk_fiber-unix.h
 *
 * @brief Unix-specific implementation for cilk_fiber.
 */

/**
 * @brief Unix-specific fiber class derived from portable fiber class
 */
struct cilk_fiber_sysdep : public cilk_fiber
{
  public:

#if SUPPORT_GET_CURRENT_FIBER
    /**
     * @brief Gets the current fiber from TLS.
     */
    static cilk_fiber_sysdep* get_current_fiber_sysdep();
#endif

    /**
     * @brief Construct the system-dependent portion of a fiber.
     *
     * @param stack_size  The size of the stack for this fiber.
     */ 
    cilk_fiber_sysdep(std::size_t stack_size);

    /**
     * @brief Construct the system-dependent of a fiber created from a
     * thread.
     */ 
    cilk_fiber_sysdep(from_thread_t);

    /**
     * @brief Destructor
     */ 
    ~cilk_fiber_sysdep();

    /**
     * @brief OS-specific calls to convert this fiber back to thread.
     *
     * Nothing to do for Linux.
     */
    void convert_fiber_back_to_thread();

    /**
     * @brief System-dependent function to suspend self and resume execution of "other".
     *
     * This fiber is suspended.
     *          
     * @pre @c is_resumable() should be true. 
     *
     * @param other              Fiber to resume.
     */
    void suspend_self_and_resume_other_sysdep(cilk_fiber_sysdep* other);

    /**
     * @brief System-dependent function called to jump to @p other
     * fiber.
     *
     * @pre @c is_resumable() should be false.
     *
     * @param other  Fiber to resume.
     */
    NORETURN jump_to_resume_other_sysdep(cilk_fiber_sysdep* other);
    
    /**
     * @brief Runs the start_proc.
     * @pre is_resumable() should be false.
     * @pre is_allocated_from_thread() should be false.
     * @pre m_start_proc must be valid.
     */
    NORETURN run();

    /**
     * @brief Returns the base of this fiber's stack.
     */
    inline char* get_stack_base_sysdep() { return m_stack_base; }

  private:
    char*                       m_stack_base;     ///< The base of this fiber's stack.
    char*                       m_stack;          // Stack memory (low address)
    __CILK_JUMP_BUFFER          m_resume_jmpbuf;  // Place to resume fiber
    unsigned                    m_magic;          // Magic number for checking

    static int                  s_page_size;      // Page size for
                                                  // stacks.

    // Allocate memory for a stack.  This method
    // initializes m_stack and m_stack_base.
    void make_stack(size_t stack_size);

    // Deallocates memory for the stack.
    void free_stack();

    // Common helper method for implementation of resume_other_sysdep
    // variants.
    inline void resume_other_sysdep(cilk_fiber_sysdep* other);
};

#endif // ! defined(INCLUDED_CILK_FIBER_UNIX_DOT_H)
