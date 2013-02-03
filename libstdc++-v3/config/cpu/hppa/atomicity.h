// Low-level functions for atomic operations: PA-RISC version  -*- C++ -*-

// Copyright (C) 2002-2013 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#include <ext/atomicity.h>

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<int _Inst>
    struct _Atomicity_lock
    {
      static volatile int _S_atomicity_lock;
    };
  
  template<int _Inst>
  volatile int
  _Atomicity_lock<_Inst>::_S_atomicity_lock __attribute__ ((aligned (16))) = 1;

  // Because of the lack of weak support when using the hpux som
  // linker, we explicitly instantiate the atomicity lock.
  template volatile int _Atomicity_lock<0>::_S_atomicity_lock;

  int
  __attribute__ ((__unused__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word result;
    int tmp;
    volatile int& lock = _Atomicity_lock<0>::_S_atomicity_lock;
    
    __asm__ __volatile__ ("ldcw 0(%1),%0\n\t"
			  "cmpib,<>,n 0,%0,.+20\n\t"
			  "ldw 0(%1),%0\n\t"
			  "cmpib,= 0,%0,.-4\n\t"
			  "nop\n\t"
			  "b,n .-20"
			  : "=&r" (tmp)
			  : "r" (&lock)
			  : "memory");
    
    result = *__mem;
    *__mem = result + __val;
    __asm__ __volatile__ ("stw %1,0(%0)"
			  : : "r" (&lock), "r" (tmp) : "memory");
    return result;
  }
  
  void
  __attribute__ ((__unused__))
  __atomic_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    int tmp;
    volatile int& lock = _Atomicity_lock<0>::_S_atomicity_lock;
    
    __asm__ __volatile__ ("ldcw 0(%1),%0\n\t"
			  "cmpib,<>,n 0,%0,.+20\n\t"
			  "ldw 0(%1),%0\n\t"
			  "cmpib,= 0,%0,.-4\n\t"
			  "nop\n\t"
			  "b,n .-20"
			  : "=&r" (tmp)
			  : "r" (&lock)
			  : "memory");
    
    *__mem += __val;
    __asm__ __volatile__ ("stw %1,0(%0)"
			  : : "r" (&lock), "r" (tmp) : "memory");
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
