// Exception Handling support header for -*- C++ -*-

// Copyright (C) 2016-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file eh_atomics.h
 *  This is an internal header file, included by library source files.
 *  Do not attempt to use it directly.
 */

#ifndef _EH_ATOMICS_H
#define _EH_ATOMICS_H 1

#include <bits/c++config.h>
#include <bits/atomic_word.h>
#include <bits/atomic_lockfree_defines.h>
#if ATOMIC_INT_LOCK_FREE <= 1
# include <ext/atomicity.h>
#endif

#pragma GCC visibility push(default)
extern "C++" {
namespace __gnu_cxx
{
  void
  __eh_atomic_inc (_Atomic_word* __count) __attribute__((always_inline));

  bool
  __eh_atomic_dec (_Atomic_word* __count) __attribute__((always_inline));

  // Increments the count.
  inline void
  __eh_atomic_inc (_Atomic_word* __count)
  {
#if ATOMIC_INT_LOCK_FREE > 1
    __atomic_add_fetch (__count, 1, __ATOMIC_ACQ_REL);
#else
    _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE (__count);
    __gnu_cxx::__atomic_add_dispatch (__count, 1);
    _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER (__count);
#endif
  }

  // Decrements the count and returns true if it reached zero.
  inline bool
  __eh_atomic_dec (_Atomic_word* __count)
  {
#if ATOMIC_INT_LOCK_FREE > 1
    return __atomic_sub_fetch (__count, 1, __ATOMIC_ACQ_REL) == 0;
#else
    _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE (__count);
    if (__gnu_cxx::__exchange_and_add_dispatch (__count, -1) == 1)
      {
	_GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER (__count);
	return true;
      }
    return false;
#endif
  }
} // namespace __gnu_cxx
}
#pragma GCC visibility pop

#endif // _EH_ATOMICS_H
