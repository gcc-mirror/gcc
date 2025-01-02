// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile }

// Ensure the library only uses the __name__ form for attributes.
// Don't test 'const' because it is reserved anyway.

#define abi_tag 1

#ifndef __APPLE__
// darwin headers use these, see PR 64883
# define always_inline 1
# define cold 1
# if __cplusplus < 201703L
#  define deprecated 1 // Reserved since C++17
# endif
# if __cplusplus < 201103L
#  define noreturn 1 // Reserved since C++11
# endif
# define visibility 1
#endif

#if __cplusplus < 202002L
# define no_unique_address 1
# define likely 1
# define unlikely 1
#endif

#ifndef __s390__
// kernel-headers <asm/types.h> uses __attribute__((packed,aligned(4))) on
// S390.
#define packed 1
#endif

#define pure 1

// glibc's sysdeps/unix/sysv/linux/arm/sys/ucontext.h uses this on ARM.
#ifndef __arm__
#define unused 1
#endif

#include <bits/extc++.h>
#include <cxxabi.h>
