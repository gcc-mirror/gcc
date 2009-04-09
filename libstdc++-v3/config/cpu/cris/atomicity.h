// Low-level functions for atomic operations: CRIS version  -*- C++ -*-

// Copyright (C) 2001, 2003, 2004, 2005, 2009 Free Software Foundation, Inc.
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

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  _Atomic_word
  __exchange_and_add(volatile _Atomic_word* __mem, int __val)
  {
    int __tmp;
    _Atomic_word __result;

#if (__CRIS_arch_version >= 32)
  __asm__ __volatile__ (" clearf p	       \n"
		       "0:		       \n"
		       " move.d %4,%2	       \n"
		       " move.d [%3],%0	       \n"
		       " add.d %0,%2	       \n"
		       " ax		       \n"
		       " move.d %2,[%3]	       \n"
		       " bcs 0b		       \n"
		       " clearf p	       \n"
		       :  "=&r" (__result), "=Q" (*__mem), "=&r" (__tmp)
		       : "r" (__mem), "g" (__val), "Q" (*__mem)
		       : "memory");
#elif (__CRIS_arch_version >= 10)
    __asm__ __volatile__ (" clearf		\n"
			"0:			\n"
			" move.d %4,%2		\n"
			" move.d [%3],%0	\n"
			" add.d %0,%2		\n"
			" ax			\n"
			" move.d %2,[%3]	\n"
			" bwf 0b		\n"
			" clearf		\n"
			:  "=&r" (__result), "=Q" (*__mem), "=&r" (__tmp)
			: "r" (__mem), "g" (__val), "Q" (*__mem)
			/* The memory clobber must stay, regardless of
			   current uses of this function.  */
			: "memory");
#else
    __asm__ __volatile__ (" move $ccr,$r9	\n"
			" di			\n"
			" move.d %4,%2		\n"
			" move.d [%3],%0	\n"
			" add.d %0,%2		\n"
			" move.d %2,[%3]	\n"
			" move $r9,$ccr		\n"
			:  "=&r" (__result), "=Q" (*__mem), "=&r" (__tmp)
			: "r" (__mem), "g" (__val), "Q" (*__mem)
			: "r9",
			  /* The memory clobber must stay, regardless of
			     current uses of this function.  */
			  "memory");
#endif

    return __result;
  }

  void
  __atomic_add(volatile _Atomic_word* __mem, int __val)
  { __exchange_and_add(__mem, __val); }

_GLIBCXX_END_NAMESPACE
