/* bug.h                  -*-C++-*-
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
 * @file bug.h
 *
 * @brief Support for reporting bugs and debugging.
 */

#ifndef INCLUDED_BUG_DOT_H
#define INCLUDED_BUG_DOT_H

#include "rts-common.h"
#include <cilk/common.h>

__CILKRTS_BEGIN_EXTERN_C

/**
 * Flush all output, write error message to stderr and abort the execution.
 * On Windows the error is also written to the debugger.
 *
 * @param fmt printf-style format string.  Any remaining parameters will be
 * be interpreted based on the format string text.
 */
COMMON_PORTABLE NORETURN __cilkrts_bug(const char *fmt,...) cilk_nothrow;

#ifndef CILK_ASSERT

/** Standard text for failed assertion */
COMMON_PORTABLE extern const char *const __cilkrts_assertion_failed;

/**
 * Macro to assert an invariant that must be true.  If the statement evalutes
 * to false, __cilkrts_bug will be called to report the failure and terminate
 * the application.
 */
#define CILK_ASSERT(ex)                                                 \
    (__builtin_expect((ex) != 0, 1) ? (void)0 :                         \
     __cilkrts_bug(__cilkrts_assertion_failed, __FILE__, __LINE__,  #ex))

#define CILK_ASSERT_MSG(ex, msg)                                        \
    (__builtin_expect((ex) != 0, 1) ? (void)0 :                         \
     __cilkrts_bug(__cilkrts_assertion_failed, __FILE__, __LINE__,      \
                   #ex "\n    " msg))
#endif  // CILK_ASSERT

/**
 * Assert that there is no uncaught exception.
 *
 * Not valid on Windows or Android.
 *
 * On Android, calling std::uncaught_exception with the stlport library causes
 * a seg fault.  Since we're not supporting exceptions there at this point,
 * just don't do the check.  It works with the GNU STL library, but that's
 * GPL V3 licensed.
 */
COMMON_PORTABLE void cilkbug_assert_no_uncaught_exception(void);
#if defined(_WIN32) || defined(__ANDROID__)
#  define CILKBUG_ASSERT_NO_UNCAUGHT_EXCEPTION()
#else
#  define CILKBUG_ASSERT_NO_UNCAUGHT_EXCEPTION() \
    cilkbug_assert_no_uncaught_exception()
#endif


/**
 * Call __cilkrts_bug with a standard message that the runtime state is
 * corrupted and the application is being terminated.
 */
COMMON_SYSDEP void abort_because_rts_is_corrupted(void);

// Debugging aids
#ifndef _DEBUG
#       define DBGPRINTF(_fmt, ...)
#elif defined(_WIN32)

/**
 * Write debugging output.  On windows this is written to the debugger.
 *
 * @param fmt printf-style format string.  Any remaining parameters will be
 * be interpreted based on the format string text.
 */
COMMON_SYSDEP void __cilkrts_dbgprintf(const char *fmt,...) cilk_nothrow;

/**
 * Macro to write debugging output which will be elided if this is not a
 * debug build.  The macro is currently always elided on non-Windows builds.
 *
 * @param _fmt printf-style format string.  Any remaining parameters will be
 * be interpreted based on the format string text.
 */
#       define DBGPRINTF(_fmt, ...) __cilkrts_dbgprintf(_fmt, __VA_ARGS__)

#else /* if _DEBUG && !_WIN32 */
    /* Non-Windows debug logging.  Someday we should make GetCurrentFiber()
     * and GetWorkerFiber() do something.
     */
#   include <stdio.h>
    __CILKRTS_INLINE void* GetCurrentFiber() { return 0; }
    __CILKRTS_INLINE void* GetWorkerFiber(__cilkrts_worker* w) { return 0; }
#       define DBGPRINTF(_fmt, ...) fprintf(stderr, _fmt, __VA_ARGS__)
#endif  // _DEBUG

__CILKRTS_END_EXTERN_C

#endif // ! defined(INCLUDED_BUG_DOT_H)
