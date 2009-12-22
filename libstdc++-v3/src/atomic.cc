// Support for atomic operations -*- C++ -*-

// Copyright (C) 2008, 2009
// Free Software Foundation, Inc.
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

#include "gstdint.h"
#include <atomic>
#include <mutex>

#define LOGSIZE 4

namespace
{
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
  std::mutex&
  get_atomic_mutex()
  {
    static std::mutex atomic_mutex;
    return atomic_mutex;
  }
#endif

  std::__atomic_flag_base flag_table[ 1 << LOGSIZE ] =
    {
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
    };
} // anonymous namespace

namespace std
{
  namespace __atomic0
  {
    bool
    atomic_flag::test_and_set(memory_order)
    {
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
      lock_guard<mutex> __lock(get_atomic_mutex());
#endif
      bool result = _M_i;
      _M_i = true;
      return result;
    }

    void
    atomic_flag::clear(memory_order)
    {
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
      lock_guard<mutex> __lock(get_atomic_mutex());
#endif
      _M_i = false;
    }
  }

  extern "C"
  {
    bool
    atomic_flag_test_and_set_explicit(__atomic_flag_base* __a,
				      memory_order __m) throw()
    {
      atomic_flag* d = static_cast<volatile atomic_flag*>(__a);
      return d->test_and_set(__m);
    }

    void
    atomic_flag_clear_explicit(__atomic_flag_base* __a, 
			       memory_order __m) throw()
    {
      atomic_flag* d = static_cast<volatile atomic_flag*>(__a);
      return d->clear(__m);
    }

    void
    __atomic_flag_wait_explicit(__atomic_flag_base* __a,
				memory_order __x) throw()
    {
      while (atomic_flag_test_and_set_explicit(__a, __x))
	{ };
    }

    __atomic_flag_base*
    __atomic_flag_for_address(const void* __z) throw()
    {
      uintptr_t __u = reinterpret_cast<uintptr_t>(__z);
      __u += (__u >> 2) + (__u << 4);
      __u += (__u >> 7) + (__u << 5);
      __u += (__u >> 17) + (__u << 13);
      if (sizeof(uintptr_t) > 4)
	__u += (__u >> 31);
      __u &= ~((~uintptr_t(0)) << LOGSIZE);
      return flag_table + __u;
    }
  } // extern "C"
} // namespace std


// XXX GLIBCXX_ABI Deprecated
// gcc-4.5.0
// <atomic> signature changes

// The rename syntax for default exported names is
//   asm (".symver name1,exportedname@GLIBCXX_3.4")
//   asm (".symver name2,exportedname@@GLIBCXX_3.4.5")
// In the future, GLIBCXX_ABI > 6 should remove all uses of
// _GLIBCXX_*_SYMVER macros in this file.

#if defined(_GLIBCXX_SYMVER_GNU) && defined(PIC) \
    && defined(_GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE)

#define _GLIBCXX_ASM_SYMVER(cur, old, version) \
   asm (".symver " #cur "," #old "@@" #version);

_GLIBCXX_ASM_SYMVER(_ZNSt9__atomic011atomic_flag5clearESt12memory_order, _ZNVSt9__atomic011atomic_flag5clearESt12memory_order, GLIBCXX_3.4.11)

_GLIBCXX_ASM_SYMVER(_ZNSt9__atomic011atomic_flag12test_and_setESt12memory_order, _ZNVSt9__atomic011atomic_flag12test_and_setESt12memory_order, GLIBCXX_3.4.11)

#endif
