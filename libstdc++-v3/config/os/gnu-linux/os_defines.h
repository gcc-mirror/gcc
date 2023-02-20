// Specific definitions for GNU/Linux  -*- C++ -*-

// Copyright (C) 2000-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/os_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */

#ifndef _GLIBCXX_OS_DEFINES
#define _GLIBCXX_OS_DEFINES 1

// System-specific #define, typedefs, corrections, etc, go here.  This
// file will come before all others.

// This keeps isalnum, et al from being propagated as macros.
#define __NO_CTYPE 1

#include <features.h>

// Provide a declaration for the possibly deprecated gets function, as
// glibc 2.15 and later does not declare gets for ISO C11 when
// __GNU_SOURCE is defined.
#if __GLIBC_PREREQ(2,15) && defined(_GNU_SOURCE)
# undef _GLIBCXX_HAVE_GETS
#endif

// Glibc 2.23 removed the obsolete isinf and isnan declarations. Check the
// version dynamically in case it has changed since libstdc++ was configured.
#define _GLIBCXX_NO_OBSOLETE_ISINF_ISNAN_DYNAMIC __GLIBC_PREREQ(2,23)

// Glibc 2.26 on i?86/x86-64/ia64/ppc64le added *f128 support.
// Glibc 2.27 added it also on many other arches but those have IEEE quad
// long double.
#if __GLIBC_PREREQ(2, 26) \
    && (defined(__i386__) || defined(__x86_64__) || defined (__ia64__) \
	|| (defined(__powerpc__) && defined(_ARCH_PWR8) \
	    && defined(__LITTLE_ENDIAN__) && (_CALL_ELF == 2) \
	    && defined(__FLOAT128__)))
# define _GLIBCXX_HAVE_FLOAT128_MATH 1
#endif

#ifdef __linux__
// The following libpthread properties only apply to Linux, not GNU/Hurd.

# if __GLIBC_PREREQ(2, 27)
// Since glibc 2.27 pthread_self() is usable without linking to libpthread.
#  define _GLIBCXX_NATIVE_THREAD_ID pthread_self()
# else
// Before then it was in libc.so.6 but not libc.a, and always returns 0,
// which breaks the invariant this_thread::get_id() != thread::id{}.
// So only use it if we know the libpthread version is available.
// Otherwise use (__gthread_t)1 as the ID of the main (and only) thread.
#  define _GLIBCXX_NATIVE_THREAD_ID \
   (__gthread_active_p() ? __gthread_self() : (__gthread_t)1)
# endif

# if __GLIBC_PREREQ(2, 34)
// Since glibc 2.34 all pthreads functions are usable without linking to
// libpthread.
#  define _GLIBCXX_GTHREAD_USE_WEAK 0
# endif
#endif // __linux__

#endif
