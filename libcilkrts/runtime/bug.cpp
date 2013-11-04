/* bug.cpp                  -*-C++-*-
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

#include "bug.h"

#include <exception>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#ifdef _WIN32
#   include "windows-clean.h"
#   include "internal/abi.h"
#   include "cilktools/cilkscreen.h"
#   include <crtdbg.h>
#endif

__CILKRTS_BEGIN_EXTERN_C

COMMON_PORTABLE const char *const __cilkrts_assertion_failed =
    "%s:%d: cilk assertion failed: %s\n";

COMMON_PORTABLE void __cilkrts_bug(const char *fmt,...) cilk_nothrow
{
#if defined (_WIN32) && defined(_DEBUG)
    _CRTIMP void __cdecl _wassert(__in_z const wchar_t * _Message,
                                  __in_z const wchar_t *_File,
                                  __in unsigned _Line);
    char message[256];
    wchar_t wmessage[256];
    va_list l;
    va_start(l, fmt);
    _vsnprintf_s(message, 256, _TRUNCATE, fmt, l);
    va_end(l);
    _snwprintf_s(wmessage, 256, _TRUNCATE, _CRT_WIDE("%S"),
                 message); /* widen */

    // Force asserts to go to stderr and the debugger.  This isn't polite, but
    // we're about to kill the app anyway and it will prevent our tests from
    // hanging
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE| _CRTDBG_MODE_DEBUG);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);

    _wassert(wmessage, _CRT_WIDE(__FILE__), __LINE__);

    // If there's a debugger attached, give it a chance to look at the failure
    if (IsDebuggerPresent())
        DebugBreak();

    abort();
/*    __asm int 3 */
#else
    /* To reduce user confusion, write all user-generated output
       before the system-generated error message. */
    va_list l;
    fflush(NULL);
    va_start(l, fmt);
    vfprintf(stderr, fmt, l);
    va_end(l);
    fflush(stderr);

#ifndef _WIN32
    abort();
#endif

#endif

    exit(1);
}

COMMON_PORTABLE void cilkbug_assert_no_uncaught_exception(void)
{
    bool uncaught = std::uncaught_exception();
    CILK_ASSERT(!uncaught);
}

COMMON_SYSDEP void abort_because_rts_is_corrupted(void)
{
    __cilkrts_bug("The Cilk Plus runtime system detected a corruption "
                  "in its data structures.  This is most likely caused "
                  "by an application bug.  Aborting execution.\n");
}

#ifdef WIN32
COMMON_SYSDEP void __cilkrts_dbgprintf(const char *fmt,...)
{
    char message[2048];
    va_list l;

    // Cilkscreen shouldn't watch this
    __cilkscreen_disable_checking();

    va_start(l, fmt);
    _vsnprintf_s(message, 2048, _TRUNCATE, fmt, l);
    va_end(l);
    OutputDebugStringA (message);

    // Re-enable Cilkscreen
    __cilkscreen_enable_checking();
}
#endif

__CILKRTS_END_EXTERN_C

/* End bug.cpp */
