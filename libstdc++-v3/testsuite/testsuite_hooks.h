// Utility subroutines for the C++ library testsuite.
//
// Copyright (C) 2000, 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// This file provides the following:
//
// 1)  VERIFY(), via DEBUG_ASSERT, from Brent Verner <brent@rcfile.org>.
//   This file is included in the various testsuite programs to provide
//   #define(able) assert() behavior for debugging/testing. It may be
//   a suitable location for other furry woodland creatures as well.
//
// 2)  __set_testsuite_memlimit()
//   __set_testsuite_memlimit() uses setrlimit() to restrict dynamic memory
//   allocation.  We provide a default memory limit if none is passed by the
//   calling application.  The argument to __set_testsuite_memlimit() is the
//   limit in megabytes (a floating-point number).  If _GLIBCPP_MEM_LIMITS is
//   #defined before including this header, then no limiting is attempted.

#ifndef _GLIBCPP_TESTSUITE_HOOKS_H
#define _GLIBCPP_TESTSUITE_HOOKS_H

#ifdef DEBUG_ASSERT
# include <cassert>
# define VERIFY(fn) assert(fn)
#else
# define VERIFY(fn) test &= (fn)
# define VERIFY(fn) fn
#endif

#include <bits/c++config.h>

// Defined in GLIBCPP_CONFIGURE_TESTSUITE.
#ifndef _GLIBCPP_MEM_LIMITS

// Don't do memory limits.
void
__set_testsuite_memlimit(float x = 0)
{ }

#else

// Do memory limits.
#include <sys/resource.h>
#include <unistd.h>

#ifndef MEMLIMIT_MB
#define MEMLIMIT_MB 16.0
#endif

void
__set_testsuite_memlimit(float __size = MEMLIMIT_MB)
{
    struct rlimit r;
    r.rlim_cur = (rlim_t)(__size * 1048576);

    // Heap size, seems to be common.
#if _GLIBCPP_HAVE_MEMLIMIT_DATA
    setrlimit(RLIMIT_DATA, &r);
#endif

    // Resident set size.
#if _GLIBCPP_HAVE_MEMLIMIT_RSS
    setrlimit(RLIMIT_RSS, &r);
#endif

    // Mapped memory (brk + mmap).
#if _GLIBCPP_HAVE_MEMLIMIT_VMEM
    setrlimit(RLIMIT_VMEM, &r);
#endif

    // Virtual memory.
#if _GLIBCPP_HAVE_MEMLIMIT_AS
    setrlimit(RLIMIT_AS, &r);
#endif
}
#endif

#endif // _GLIBCPP_TESTSUITE_HOOKS_H

