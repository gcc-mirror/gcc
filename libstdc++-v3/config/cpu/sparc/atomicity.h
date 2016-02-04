// Low-level functions for atomic operations: Sparc version  -*- C++ -*-

// Copyright (C) 1999-2016 Free Software Foundation, Inc.
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

#ifdef __arch64__
  _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word __tmp1, __tmp2;
    _Atomic_word __val_extended = __val;

    __asm__ __volatile__("1:	ldx	[%3], %0\n\t"
			 "	add	%0, %4, %1\n\t"
			 "	casx	[%3], %0, %1\n\t"
			 "	sub	%0, %1, %0\n\t"
			 "	brnz,pn	%0, 1b\n\t"
			 "	 nop"
			 : "=&r" (__tmp1), "=&r" (__tmp2), "=m" (*__mem)
			 : "r" (__mem), "r" (__val_extended), "m" (*__mem));
    return __tmp2;
  }
  
  void
  __attribute__ ((__unused__))
  __atomic_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word __tmp1, __tmp2;
    _Atomic_word __val_extended = __val;
    
    __asm__ __volatile__("1:	ldx	[%3], %0\n\t"
			 "	add	%0, %4, %1\n\t"
			 "	casx	[%3], %0, %1\n\t"
			 "	sub	%0, %1, %0\n\t"
			 "	brnz,pn	%0, 1b\n\t"
			 "	 nop"
			 : "=&r" (__tmp1), "=&r" (__tmp2), "=m" (*__mem)
			 : "r" (__mem), "r" (__val_extended), "m" (*__mem));
  }
  
#else /* __arch32__ */

  template<int __inst>
    struct _Atomicity_lock
    {
      static unsigned char _S_atomicity_lock;
    };

  template<int __inst>
  unsigned char _Atomicity_lock<__inst>::_S_atomicity_lock = 0;
  
  template unsigned char _Atomicity_lock<0>::_S_atomicity_lock;
  
  _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word __result, __tmp;
    
    __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
			 "	cmp	%0, 0\n\t"
			 "	bne	1b\n\t"
			 "	 nop"
			 : "=&r" (__tmp)
			 : "r" (&_Atomicity_lock<0>::_S_atomicity_lock)
			 : "memory");
    __result = *__mem;
    *__mem += __val;
    __asm__ __volatile__("stb	%%g0, [%0]"
			 : /* no outputs */
			 : "r" (&_Atomicity_lock<0>::_S_atomicity_lock)
			 : "memory");
    return __result;
  }
  
  void
  __attribute__ ((__unused__))
  __atomic_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word __tmp;
    
    __asm__ __volatile__("1:	ldstub	[%1], %0\n\t"
			 "	cmp	%0, 0\n\t"
			 "	bne	1b\n\t"
			 "	 nop"
			 : "=&r" (__tmp)
			 : "r" (&_Atomicity_lock<0>::_S_atomicity_lock)
			 : "memory");
    *__mem += __val;
    __asm__ __volatile__("stb	%%g0, [%0]"
			 : /* no outputs */
			 : "r" (&_Atomicity_lock<0>::_S_atomicity_lock)
			 : "memory");
  }  
#endif /* __arch32__ */

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
