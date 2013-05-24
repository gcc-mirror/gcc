// Compatibility symbols for previous versions, C++0x bits -*- C++ -*-

// Copyright (C) 2013 Free Software Foundation, Inc.
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

#define _GLIBCXX_COMPATIBILITY_CXX0X
#include <bits/c++config.h>

#if defined(_GLIBCXX_SYMVER_GNU) && defined(_GLIBCXX_SHARED) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE)\
    && defined(_GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT)

#define steady_clock steady_clockXX
#include "chrono.cc"

// The rename syntax for default exported names is
//   asm (".symver name1,exportedname@GLIBCXX_3.4.17")
//   asm (".symver name2,exportedname@@GLIBCXX_3.4.19")
// In the future, GLIBCXX_ABI > 6 should remove all uses of
// _GLIBCXX_*_SYMVER macros in this file.

#define _GLIBCXX_3_4_17_SYMVER(XXname, name) \
   extern "C" void \
   _X##name() \
   __attribute__ ((alias(#XXname))); \
   asm (".symver " "_X" #name "," #name "@GLIBCXX_3.4.17");

#define _GLIBCXX_3_4_19_SYMVER(XXname, name) \
   extern "C" void \
   _Y##name() \
   __attribute__ ((alias(#XXname))); \
   asm (".symver " "_Y" #name  "," #name "@@GLIBCXX_3.4.19");

_GLIBCXX_3_4_17_SYMVER(_ZNSt6chrono14steady_clockXX3nowEv,
		       _ZNSt6chrono12steady_clock3nowEv)
_GLIBCXX_3_4_19_SYMVER(_ZNSt6chrono14steady_clockXX3nowEv,
		       _ZNSt6chrono12steady_clock3nowEv)

#endif
