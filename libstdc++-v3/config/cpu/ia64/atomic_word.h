// Low-level type for atomic operations -*- C++ -*-

// Copyright (C) 2004, 2005, 2006, 2007, 2009 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_ATOMIC_WORD_H
#define _GLIBCXX_ATOMIC_WORD_H	1

#include <bits/cxxabi_tweaks.h>

typedef int _Atomic_word;

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
  // Test the first byte of __g and ensure that no loads are hoisted across
  // the test.
  inline bool
  __test_and_acquire (__cxxabiv1::__guard *__g)
  {
    unsigned char __c;
    unsigned char *__p = reinterpret_cast<unsigned char *>(__g);
    // ldN.acq is a load with an implied hoist barrier.
    // would ld8+mask be faster than just doing an ld1?
    __asm __volatile ("ld1.acq %0 = %1" : "=r"(__c) : "m"(*__p) : "memory");
    return __c != 0;
  }

  // Set the first byte of __g to 1 and ensure that no stores are sunk
  // across the store.
  inline void
  __set_and_release (__cxxabiv1::__guard *__g)
  {
    unsigned char *__p = reinterpret_cast<unsigned char *>(__g);
    // stN.rel is a store with an implied sink barrier.
    // could load word, set flag, and CAS it back
    __asm __volatile ("st1.rel %0 = %1" : "=m"(*__p) : "r"(1) : "memory");
  }

  // We don't define the _BARRIER macros on ia64 because the barriers are
  // included in the test and set, above.
#define _GLIBCXX_GUARD_TEST_AND_ACQUIRE(G) __gnu_cxx::__test_and_acquire (G)
#define _GLIBCXX_GUARD_SET_AND_RELEASE(G) __gnu_cxx::__set_and_release (G)
}

#endif 
