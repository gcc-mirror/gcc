// Support for atomic operations -*- C++ -*-

// Copyright (C) 2008, 2009
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include "gstdint.h"
#include <cstdatomic>
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

  std::__atomic_flag_base volatile flag_table[ 1 << LOGSIZE ] =
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
    atomic_flag::test_and_set(memory_order) volatile
    {
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
      lock_guard<mutex> __lock(get_atomic_mutex());
#endif
      bool result = _M_i;
      _M_i = true;
      return result;
    }

    void
    atomic_flag::clear(memory_order) volatile
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
    atomic_flag_test_and_set_explicit(volatile __atomic_flag_base* __a,
				      memory_order __m)
    {
      volatile atomic_flag d(__a->_M_i);
      return d.test_and_set(__m);
    }

    void
    atomic_flag_clear_explicit(volatile __atomic_flag_base* __a,
			       memory_order __m)
    {
      volatile atomic_flag d(__a->_M_i);
      return d.clear(__m);
    }

    void
    __atomic_flag_wait_explicit(volatile __atomic_flag_base* __a,
				memory_order __x)
    {
      while (atomic_flag_test_and_set_explicit(__a, __x))
	{ };
    }

    volatile __atomic_flag_base*
    __atomic_flag_for_address(const volatile void* __z)
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
