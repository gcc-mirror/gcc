// Specific definitions for Darwin -*- C++ -*-

// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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


#ifndef _GLIBCXX_OS_DEFINES
#define _GLIBCXX_OS_DEFINES 1

// System-specific #define, typedefs, corrections, etc, go here.  This
// file will come before all others.

/* Darwin has the pthread routines in libSystem, which every program
   links to, so there's no need for weak-ness for that.  */
#define _GLIBCXX_GTHREAD_USE_WEAK 0

// On Darwin, in order to enable overriding of operator new and delete, the
// ABI library exports a weak definition. The static linker will override this
// iff a user-provided implementation is given (providing that the user
// implementation is not itself a weak definition).
#define _GLIBCXX_WEAK_DEFINITION __attribute__ ((__weak__))

#if defined (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) \
     && (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1080)
// Static initializer macro is absent for Darwin < 11 and buggy in Darwin 11,
// see libstdc++/51906.  Fixed in Darwin 12 (OS X 10.8).
#define _GTHREAD_USE_RECURSIVE_MUTEX_INIT_FUNC
#endif

// Configure checks for nanosleep fail on Darwin, but nanosleep and
// sched_yield are always available, so use them.
#define _GLIBCXX_USE_NANOSLEEP 1
#define _GLIBCXX_USE_SCHED_YIELD 1

// No support for referencing weak symbols without a definition.
#define _GLIBCXX_USE_WEAK_REF 0

#endif
