/* cilk_fiber-unix.cpp                  -*-C++-*-
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

#include "cilk_fiber-unix.h"
#include "cilk_malloc.h"
#include "bug.h"
#include "os.h"

#include <cstdio>
#include <cstdlib>

#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>

// You'd think that getting a defintion for alloca would be easy.  But you'd
// be wrong. Here's a variant on what's recommended in the autoconf doc.  I've
// remove the Windows portion since this is Unix-specific code.
#if defined HAVE_ALLOCA_H
#   include <alloca.h>
#elif defined __GNUC__
#   define alloca __builtin_alloca
#elif defined _AIX
#   define alloca __alloca
#else
#   include <stddef.h>
#   ifdef  __cplusplus
extern "C"
#   endif
void *alloca (size_t);
#endif

// MAP_ANON is deprecated on Linux, but seems to be required on Mac...
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

// Magic number for sanity checking fiber structure
const unsigned magic_number = 0x5afef00d;

int cilk_fiber_sysdep::s_page_size = getpagesize();

cilk_fiber_sysdep::cilk_fiber_sysdep(std::size_t stack_size)
    : cilk_fiber(stack_size)
    , m_magic(magic_number)
{
    // Set m_stack and m_stack_base.
    make_stack(stack_size);

    // Get high-address of stack, with 32-bytes of spare space, and rounded
    // down to the nearest 32-byte boundary.
    const uintptr_t align_mask = 32 - 1;
    m_stack_base -= ((std::size_t) m_stack_base) & align_mask;
}

cilk_fiber_sysdep::cilk_fiber_sysdep(from_thread_t)
    : cilk_fiber()
    , m_magic(magic_number)
{
    this->set_allocated_from_thread(true);

    // Dummy stack data for thread-main fiber
    m_stack      = NULL;
    m_stack_base = NULL;
}

void cilk_fiber_sysdep::convert_fiber_back_to_thread()
{
    // Does nothing on Linux.
}

cilk_fiber_sysdep::~cilk_fiber_sysdep()
{
    CILK_ASSERT(magic_number == m_magic);
    if (!this->is_allocated_from_thread())
        free_stack();
}

#if SUPPORT_GET_CURRENT_FIBER
cilk_fiber_sysdep* cilk_fiber_sysdep::get_current_fiber_sysdep()
{
    return cilkos_get_tls_cilk_fiber();
}
#endif

// Jump to resume other fiber.  We may or may not come back.
inline void cilk_fiber_sysdep::resume_other_sysdep(cilk_fiber_sysdep* other)
{
    if (other->is_resumable()) {
        other->set_resumable(false);
        // Resume by longjmp'ing to the place where we suspended.
        CILK_LONGJMP(other->m_resume_jmpbuf);
    }
    else {
        // Otherwise, we've never ran this fiber before.  Start the
        // proc method.
        other->run();
    }
}

void cilk_fiber_sysdep::suspend_self_and_resume_other_sysdep(cilk_fiber_sysdep* other)
{
#if SUPPORT_GET_CURRENT_FIBER
    cilkos_set_tls_cilk_fiber(other);
#endif
    CILK_ASSERT(this->is_resumable());


    // Jump to the other fiber.  We expect to come back.
    if (! CILK_SETJMP(m_resume_jmpbuf)) {
        resume_other_sysdep(other);
    }

    // Return here when another fiber resumes me.
    // If the fiber that switched to me wants to be deallocated, do it now.
    do_post_switch_actions();
}

NORETURN cilk_fiber_sysdep::jump_to_resume_other_sysdep(cilk_fiber_sysdep* other)
{
#if SUPPORT_GET_CURRENT_FIBER
    cilkos_set_tls_cilk_fiber(other);
#endif
    CILK_ASSERT(!this->is_resumable());

    // Jump to the other fiber.  But we are never coming back because
    // this fiber is being reset.
    resume_other_sysdep(other);

    // We should never come back here...
    __cilkrts_bug("Should not get here");
}

// GCC doesn't allow us to call __builtin_longjmp in the same function that
// calls __builtin_setjmp, so create a new function to house the call to
// __builtin_longjmp
static void __attribute__((noinline))
do_cilk_longjmp(__CILK_JUMP_BUFFER jmpbuf)
{
    CILK_LONGJMP(jmpbuf);
}

NORETURN cilk_fiber_sysdep::run()
{
    // Only fibers created from a pool have a proc method to run and execute. 
    CILK_ASSERT(m_start_proc);
    CILK_ASSERT(!this->is_allocated_from_thread());
    CILK_ASSERT(!this->is_resumable());

    // TBD: This setjmp/longjmp pair simply changes the stack pointer.
    // We could probably replace this code with some assembly.
    if (! CILK_SETJMP(m_resume_jmpbuf))
    {
        // Calculate the size of the current stack frame (i.e., this
        // run() function.  
        size_t frame_size = (size_t)JMPBUF_FP(m_resume_jmpbuf) - (size_t)JMPBUF_SP(m_resume_jmpbuf);

        // Macs require 16-byte alignment.  Do it always because it just
        // doesn't matter
        if (frame_size & (16-1))
            frame_size += 16 - (frame_size  & (16-1));

        // Assert that we are getting a reasonable frame size out of
        // it.  If this run() function is using more than 4096 bytes
        // of space for its local variables / any state that spills to
        // registers, something is probably *very* wrong here...
        //
        // 4096 bytes just happens to be a number that seems "large
        // enough" --- for an example GCC 32-bit compilation, the
        // frame size was 48 bytes.
        CILK_ASSERT(frame_size < 4096);

        // Change stack pointer to fiber stack.  Offset the
        // calculation by the frame size, so that we've allocated
        // enough extra space from the top of the stack we are
        // switching to for any temporaries required for this run()
        // function.
        JMPBUF_SP(m_resume_jmpbuf) = m_stack_base - frame_size;

        // GCC doesn't allow us to call __builtin_longjmp in the same function
        // that calls __builtin_setjmp, so it's been moved into it's own
        // function that cannot be inlined.
        do_cilk_longjmp(m_resume_jmpbuf);
    }

    // Note: our resetting of the stack pointer is valid only if the
    // compiler has not saved any temporaries onto the stack for this
    // function before the longjmp that we still care about at this
    // point.
    
    // Verify that 1) 'this' is still valid and 2) '*this' has not been
    // corrupted.
    CILK_ASSERT(magic_number == m_magic);

    // If the fiber that switched to me wants to be deallocated, do it now.
    do_post_switch_actions();

    // Now call the user proc on the new stack
    m_start_proc(this);

    // alloca() to force generation of frame pointer.  The argument to alloca
    // is contrived to prevent the compiler from optimizing it away.  This
    // code should never actually be executed.
    int* dummy = (int*) alloca((sizeof(int) + (std::size_t) m_start_proc) & 0x1);
    *dummy = 0xface;

    // User proc should never return.
    __cilkrts_bug("Should not get here");
}

void cilk_fiber_sysdep::make_stack(size_t stack_size)
{
    char* p;
    // We've already validated that the stack size is page-aligned and
    // is a reasonable value.  No need to do any extra rounding here.
    size_t rounded_stack_size = stack_size;

    // Normally, we have already validated that the stack size is
    // aligned to 4K.  In the rare case that pages are huge though, we
    // need to do some extra checks.
    if (rounded_stack_size < 3 * (size_t)s_page_size) {
        // If the specified stack size is too small, round up to 3
        // pages.  We need at least 2 extra for the guard pages.
        rounded_stack_size = 3 * (size_t)s_page_size;
    }
    else {
        // Otherwise, the stack size is large enough, but might not be
        // a multiple of page size.  Round up to nearest multiple of
        // s_page_size, just to be safe.
        size_t remainder = rounded_stack_size % s_page_size;
        if (remainder) {
            rounded_stack_size += s_page_size - remainder;
        }
    }

    p = (char*)mmap(0, rounded_stack_size,
                    PROT_READ|PROT_WRITE,
                    MAP_PRIVATE|MAP_ANONYMOUS,
                    -1, 0);
    if (MAP_FAILED == p) {
        // For whatever reason (probably ran out of memory), mmap() failed.
        // There is no stack to return, so the program loses parallelism.
        m_stack = NULL;
        m_stack_base = NULL;
        return;
    }

    // mprotect guard pages.
    mprotect(p + rounded_stack_size - s_page_size, s_page_size, PROT_NONE);
    mprotect(p, s_page_size, PROT_NONE);

    m_stack = p;
    m_stack_base = p + rounded_stack_size - s_page_size;
}


void cilk_fiber_sysdep::free_stack()
{
    if (m_stack) {
        size_t rounded_stack_size = m_stack_base - m_stack + s_page_size;
        if (munmap(m_stack, rounded_stack_size) < 0)
            __cilkrts_bug("Cilk: stack munmap failed error %d\n", errno);
    }
}

/* End cilk_fiber-unix.cpp */
