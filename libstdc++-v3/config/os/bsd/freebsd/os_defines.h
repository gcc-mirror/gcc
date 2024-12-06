// Specific definitions for BSD  -*- C++ -*-

// Copyright (C) 2000-2024 Free Software Foundation, Inc.
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

#include <sys/cdefs.h> // For __LONG_LONG_SUPPORTED

#define _GLIBCXX_USE_C99_STDIO 1
#define _GLIBCXX_USE_C99_STDLIB 1
#define _GLIBCXX_USE_C99_WCHAR 1

#define _GLIBCXX_USE_C99_CHECK 1
#define _GLIBCXX_USE_C99_DYNAMIC (!(__ISO_C_VISIBLE >= 1999))
#define _GLIBCXX_USE_C99_LONG_LONG_CHECK 1
#if _GLIBCXX_USE_C99_DYNAMIC || !defined __LONG_LONG_SUPPORTED
#define _GLIBCXX_USE_C99_LONG_LONG_DYNAMIC 1
#else
#define _GLIBCXX_USE_C99_LONG_LONG_DYNAMIC 0
#endif
#define _GLIBCXX_USE_C99_FLOAT_TRANSCENDENTALS_CHECK 1
#ifdef _XOPEN_SOURCE
#define _GLIBCXX_USE_C99_FLOAT_TRANSCENDENTALS_DYNAMIC 1
#else
#define _GLIBCXX_USE_C99_FLOAT_TRANSCENDENTALS_DYNAMIC 0
#endif

// read(2) can return EINVAL for n >= INT_MAX.
#define _GLIBCXX_MAX_READ_SIZE (__INT_MAX__ - 1)

#endif
