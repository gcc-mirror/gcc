// Low-level functions for atomic operations: x86, x >= 4 version  -*- C++ -*-

// Copyright (C) 1999, 2000, 2001, 2004, 2005, 2009 Free Software Foundation, Inc.
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

#include <ext/atomicity.h>

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  _Atomic_word 
  __attribute__ ((__unused__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    register _Atomic_word __result;
    __asm__ __volatile__ ("lock; xadd{l} {%0,%1|%1,%0}"
			  : "=r" (__result), "=m" (*__mem) 
			  : "0" (__val), "m" (*__mem));
    return __result;
  }
  
  void
  __attribute__ ((__unused__))
  __atomic_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    __asm__ __volatile__ ("lock; add{l} {%1,%0|%0,%1}"
			  : "=m" (*__mem) : "ir" (__val), "m" (*__mem));
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

