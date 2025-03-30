// Specific definitions for GCN platforms  -*- C++ -*-

// Copyright (C) 2025 Free Software Foundation, Inc.
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

/** @file bits/cpu_defines.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{iosfwd}
 */

#ifndef _GLIBCXX_CPU_DEFINES
#define _GLIBCXX_CPU_DEFINES 1

/* GCN appears to run into issues similar to PR69506:

       ld: error: relocation R_AMDGPU_REL32_LO cannot be used against symbol '_ZGTtnam'; recompile with -fPIC
       >>> defined in [...]/amdgcn-amdhsa/./libstdc++-v3/src/.libs/libstdc++.a(cow-stdexcept.o)
       >>> referenced by cow-stdexcept.cc:259 ([...]/libstdc++-v3/src/c++11/cow-stdexcept.cc:259)
       >>>               cow-stdexcept.o:(_txnal_cow_string_C1_for_exceptions(void*, char const*, void*)) in archive [...]/amdgcn-amdhsa/./libstdc++-v3/src/.libs/libstdc++.a
       
       ld: error: relocation R_AMDGPU_REL32_HI cannot be used against symbol '_ZGTtnam'; recompile with -fPIC
       >>> defined in [...]/amdgcn-amdhsa/./libstdc++-v3/src/.libs/libstdc++.a(cow-stdexcept.o)
       >>> referenced by cow-stdexcept.cc:259 ([...]/source-gcc/libstdc++-v3/src/c++11/cow-stdexcept.cc:259)
       >>>               cow-stdexcept.o:(_txnal_cow_string_C1_for_exceptions(void*, char const*, void*)) in archive [...]/amdgcn-amdhsa/./libstdc++-v3/src/.libs/libstdc++.a
       
       [...]

   ..., which is:

       $ c++filt _ZGTtnam
       transaction clone for operator new[](unsigned long)

   ..., and similarly for other libitm symbols.  See PR119369.  */
#define _GLIBCXX_USE_WEAK_REF 0

#endif
