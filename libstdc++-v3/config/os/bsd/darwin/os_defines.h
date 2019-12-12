// Specific definitions for Darwin -*- C++ -*-

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// On Darwin, in order to enable overriding of operator new and delete,
// GCC makes the definition of these functions weak, relies on the
// loader to implement weak semantics properly, and uses
// -flat_namespace to work around the way that it doesn't.
#define _GLIBCXX_WEAK_DEFINITION __attribute__ ((weak))

// Static initializer macro is buggy in darwin, see libstdc++/51906
#define _GTHREAD_USE_RECURSIVE_MUTEX_INIT_FUNC

// Configure checks for nanosleep fail on Darwin, but nanosleep and
// sched_yield are always available, so use them.
#define _GLIBCXX_USE_NANOSLEEP 1
#define _GLIBCXX_USE_SCHED_YIELD 1

// No support for referencing weak symbols without a definition.
#define _GLIBCXX_USE_WEAK_REF 0

#endif
