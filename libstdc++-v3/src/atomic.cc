// Support for atomic operations -*- C++ -*-

// Copyright (C) 2008
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

#define LOGSIZE 4

namespace
{
  atomic_flag volatile __atomic_flag_anon_table__[ 1 << LOGSIZE ] =
    {
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
      ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT, ATOMIC_FLAG_INIT,
    };
} // anonymous namespace

namespace std
{
  extern "C" {

  const atomic_flag atomic_global_fence_compatibility = ATOMIC_FLAG_INIT;

  bool 
  atomic_flag_test_and_set_explicit(volatile atomic_flag* __a, 
				    memory_order __x
				    __attribute__ ((__unused__)))
  {
#ifdef _GLIBCXX_ATOMIC_BUILTINS_1
    if (__x >= memory_order_acq_rel)
      __sync_synchronize();
    return __sync_lock_test_and_set(&(__a->_M_base._M_b), 1);
#else
    bool result = __a->_M_base._M_b;
     __a->_M_base._M_b = true;
    return result;
#endif
  }

  bool 
  atomic_flag_test_and_set(volatile atomic_flag* __a)
  { return atomic_flag_test_and_set_explicit(__a, memory_order_seq_cst); }
  
  void 
  atomic_flag_clear_explicit(volatile atomic_flag* __a,
			     memory_order __x __attribute__ ((__unused__)))
  {
#ifdef _GLIBCXX_ATOMIC_BUILTINS_1
    __sync_lock_release(&(__a->_M_base._M_b));
    if (__x >= memory_order_acq_rel)
      __sync_synchronize();
#else
     __a->_M_base._M_b = false;
#endif
  } 

  void 
  atomic_flag_clear(volatile atomic_flag* __a)
  { atomic_flag_clear_explicit(__a, memory_order_seq_cst); }
  
  void 
  atomic_flag_fence(const volatile atomic_flag*, memory_order)
  {
#ifdef _GLIBCXX_ATOMIC_BUILTINS_1
    __sync_synchronize(); 
#endif
  } 

  void 
  __atomic_flag_wait_explicit(volatile atomic_flag* __a, memory_order __x)
  { 
    while (atomic_flag_test_and_set_explicit(__a, __x))
      { }; 
  }

  volatile atomic_flag* 
  __atomic_flag_for_address(const volatile void* __z)
  {
    uintptr_t __u = reinterpret_cast<uintptr_t>(__z);
    __u += (__u >> 2) + (__u << 4);
    __u += (__u >> 7) + (__u << 5);
    __u += (__u >> 17) + (__u << 13);
    if (sizeof(uintptr_t) > 4) __u += (__u >> 31);
    __u &= ~((~uintptr_t(0)) << LOGSIZE);
    return __atomic_flag_anon_table__ + __u;
  }

  } // extern "C"
} // namespace std
