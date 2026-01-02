// Support for atomic operations -*- C++ -*-

// Copyright (C) 2004-2026 Free Software Foundation, Inc.
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

/** @file ext/atomicity.h
 *  This file is a GNU extension to the Standard C++ Library.
 */

#ifndef _GLIBCXX_ATOMICITY_H
#define _GLIBCXX_ATOMICITY_H	1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/c++config.h>
#include <bits/gthr.h>
#include <bits/atomic_word.h>
#if __has_include(<sys/single_threaded.h>)
# include <sys/single_threaded.h>
#endif
#if __cplusplus >= 201103L
# include <type_traits> // make_unsigned_t
#endif

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  __attribute__((__always_inline__))
  inline bool
  __is_single_threaded() _GLIBCXX_NOTHROW
  {
#ifndef __GTHREADS
    return true;
#elif __has_include(<sys/single_threaded.h>)
    return ::__libc_single_threaded;
#else
    return !__gthread_active_p();
#endif
  }

  // Functions for portable atomic access.
  // To abstract locking primitives across all thread policies, use:
  // __exchange_and_add_dispatch
  // __atomic_add_dispatch
#ifdef _GLIBCXX_ATOMIC_WORD_BUILTINS
  inline _Atomic_word
  __attribute__((__always_inline__))
  __exchange_and_add(volatile _Atomic_word* __mem, int __val)
  { return __atomic_fetch_add(__mem, __val, __ATOMIC_ACQ_REL); }

  inline void
  __attribute__((__always_inline__))
  __atomic_add(volatile _Atomic_word* __mem, int __val)
  { __atomic_fetch_add(__mem, __val, __ATOMIC_ACQ_REL); }
#else // Defined in config/cpu/.../atomicity.h
  _Atomic_word
  __exchange_and_add(volatile _Atomic_word*, int) _GLIBCXX_NOTHROW;

  void
  __atomic_add(volatile _Atomic_word*, int) _GLIBCXX_NOTHROW;
#endif

#if __cplusplus < 201103L
  // The array bound will be ill-formed in the very unlikely case that
  // _Atomic_word is wider than long and we need to use unsigned long long
  // below in __exchange_and_add_single and __atomic_add_single.
  typedef int
    _Atomic_word_fits_in_long[sizeof(_Atomic_word) <= sizeof(long) ? 1 : -1];
#endif

  // Targets where _Atomic_word uses __attribute__((__aligned__(n))) will get
  // a warning for make_unsigned<_Atomic_word>. That warning can be ignored,
  // because we only need an unsigned type, we don't care about its alignment.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wignored-attributes"

  // We need an unsigned type that can be used for the arithmetic below.
  // This type must not be use for atomic ops because it might not be
  // sufficiently aligned. Define it as a macro that we #undef below,
  // to prevent misuse elsewhere in the library.
#if __cplusplus >= 201103L
# define _GLIBCXX_UNSIGNED_ATOMIC_WORD std::make_unsigned<_Atomic_word>::type
#else
  // For most targets make_unsigned_t<_Atomic_word> is unsigned int,
  // but 64-bit sparc uses long for _Atomic_word, so needs unsigned long.
  // Sign-extending to unsigned long works for both cases, so use that.
# define _GLIBCXX_UNSIGNED_ATOMIC_WORD unsigned long
#endif

  inline _Atomic_word
  __attribute__((__always_inline__))
  __exchange_and_add_single(_Atomic_word* __mem, int __val)
  {
    _Atomic_word __result = *__mem;
    // Do the addition with an unsigned type so that overflow is well defined.
    _GLIBCXX_UNSIGNED_ATOMIC_WORD __u;
    __u = __result;
    __u += __val;
    *__mem = __u;
    return __result;
  }

  inline void
  __attribute__((__always_inline__))
  __atomic_add_single(_Atomic_word* __mem, int __val)
  {
    _GLIBCXX_UNSIGNED_ATOMIC_WORD __u;
    __u = *__mem;
    __u += __val;
    *__mem = __u;
  }
#undef _GLIBCXX_UNSIGNED_ATOMIC_WORD
#pragma GCC diagnostic pop

  inline _Atomic_word
  __attribute__ ((__always_inline__))
  __exchange_and_add_dispatch(_Atomic_word* __mem, int __val)
  {
    if (__is_single_threaded())
      return __exchange_and_add_single(__mem, __val);
    else
      return __exchange_and_add(__mem, __val);
  }

  inline void
  __attribute__ ((__always_inline__))
  __atomic_add_dispatch(_Atomic_word* __mem, int __val)
  {
    if (__is_single_threaded())
      __atomic_add_single(__mem, __val);
    else
      __atomic_add(__mem, __val);
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Even if the CPU doesn't need a memory barrier, we need to ensure
// that the compiler doesn't reorder memory accesses across the
// barriers.
#ifndef _GLIBCXX_READ_MEM_BARRIER
#define _GLIBCXX_READ_MEM_BARRIER __atomic_thread_fence (__ATOMIC_ACQUIRE)
#endif
#ifndef _GLIBCXX_WRITE_MEM_BARRIER
#define _GLIBCXX_WRITE_MEM_BARRIER __atomic_thread_fence (__ATOMIC_RELEASE)
#endif

#endif
