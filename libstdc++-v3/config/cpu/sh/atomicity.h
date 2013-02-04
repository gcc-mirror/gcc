// Low-level functions for atomic operations: sh version  -*- C++ -*-

// Copyright (C) 1999-2013 Free Software Foundation, Inc.
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

#ifdef __SH4A__

#include <ext/atomicity.h>

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  typedef int _Atomic_word;

  _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add (volatile _Atomic_word* __mem, int __val) throw ()
  {
    _Atomic_word __result;

    __asm__ __volatile__
      ("0:\n"
       "\tmovli.l\t@%2,r0\n"
       "\tmov\tr0,%1\n"
       "\tadd\t%3,r0\n"
       "\tmovco.l\tr0,@%2\n"
       "\tbf\t0b"
       : "+m" (*__mem), "=&r" (__result)
       : "r" (__mem), "rI08" (__val)
       : "r0");

    return __result;
  }


  void
  __attribute__ ((__unused__))
  __atomic_add (volatile _Atomic_word* __mem, int __val) throw ()
  {
    asm("0:\n"
	"\tmovli.l\t@%1,r0\n"
	"\tadd\t%2,r0\n"
	"\tmovco.l\tr0,@%1\n"
	"\tbf\t0b"
	: "+m" (*__mem)
	: "r" (__mem), "rI08" (__val)
	: "r0");
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#else /* !__SH4A__ */

/* This is generic/atomicity.h */

#include <ext/atomicity.h>
#include <ext/concurrence.h>

namespace 
{
  __gnu_cxx::__mutex atomic_mutex;
} // anonymous namespace

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  _Atomic_word
  __attribute__ ((__unused__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val) throw ()
  {
    __gnu_cxx::__scoped_lock sentry(atomic_mutex);
    _Atomic_word __result;
    __result = *__mem;
    *__mem += __val;
    return __result;
  }

  void
  __attribute__ ((__unused__))
  __atomic_add(volatile _Atomic_word* __mem, int __val) throw ()
  { __exchange_and_add(__mem, __val); }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif /* !__SH4A__ */
