// -*- C++ -*- header.

// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

/** @file bits/atomic_base.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{atomic}
 */

#ifndef _GLIBCXX_ATOMIC_BASE_H
#define _GLIBCXX_ATOMIC_BASE_H 1

#pragma GCC system_header

#include <bits/c++config.h>
#include <stdbool.h>
#include <stdint.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   * @defgroup atomics Atomics
   *
   * Components for performing atomic operations.
   * @{
   */

  /// Enumeration for memory_order
  typedef enum memory_order
    {
      memory_order_relaxed,
      memory_order_consume,
      memory_order_acquire,
      memory_order_release,
      memory_order_acq_rel,
      memory_order_seq_cst
    } memory_order;

  inline memory_order
  __calculate_memory_order(memory_order __m)
  {
    const bool __cond1 = __m == memory_order_release;
    const bool __cond2 = __m == memory_order_acq_rel;
    memory_order __mo1(__cond1 ? memory_order_relaxed : __m);
    memory_order __mo2(__cond2 ? memory_order_acquire : __mo1);
    return __mo2;
  }

  void
  atomic_thread_fence(memory_order);

  void
  atomic_signal_fence(memory_order);

  /// kill_dependency
  template<typename _Tp>
    inline _Tp
    kill_dependency(_Tp __y)
    {
      _Tp __ret(__y);
      return __ret;
    }

  /**
   *  @brief Base type for atomic_flag.
   *
   *  Base type is POD with data, allowing atomic_flag to derive from
   *  it and meet the standard layout type requirement. In addition to
   *  compatibilty with a C interface, this allows different
   *  implementations of atomic_flag to use the same atomic operation
   *  functions, via a standard conversion to the __atomic_flag_base
   *  argument.
  */
  _GLIBCXX_BEGIN_EXTERN_C

  struct __atomic_flag_base
  {
    bool _M_i;
  };

  _GLIBCXX_END_EXTERN_C

#define ATOMIC_FLAG_INIT { false }


  // Base types for atomics.
  //
  // Three nested namespaces for atomic implementation details.
  //
  // The nested namespace inlined into std:: is determined by the value
  // of the _GLIBCXX_ATOMIC_PROPERTY macro and the resulting
  // ATOMIC_*_LOCK_FREE macros.
  //
  // 0 == __atomic0 == Never lock-free
  // 1 == __atomic1 == Best available, sometimes lock-free
  // 2 == __atomic2 == Always lock-free

  namespace __atomic0
  {
    struct atomic_flag;

    template<typename _IntTp>
      struct __atomic_base;
  }

  namespace __atomic2
  {
    struct atomic_flag;

    template<typename _IntTp>
      struct __atomic_base;
  }

  namespace __atomic1
  {
    using __atomic2::atomic_flag;
    using __atomic0::__atomic_base;
  }

  /// Lock-free Property
#if defined(_GLIBCXX_ATOMIC_BUILTINS_1) && defined(_GLIBCXX_ATOMIC_BUILTINS_2) \
  && defined(_GLIBCXX_ATOMIC_BUILTINS_4) && defined(_GLIBCXX_ATOMIC_BUILTINS_8)
# define _GLIBCXX_ATOMIC_PROPERTY 2
# define _GLIBCXX_ATOMIC_NAMESPACE __atomic2
#elif defined(_GLIBCXX_ATOMIC_BUILTINS_1)
# define _GLIBCXX_ATOMIC_PROPERTY 1
# define _GLIBCXX_ATOMIC_NAMESPACE __atomic1
#else
# define _GLIBCXX_ATOMIC_PROPERTY 0
# define _GLIBCXX_ATOMIC_NAMESPACE __atomic0
#endif

#define ATOMIC_CHAR_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_CHAR16_T_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_CHAR32_T_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_WCHAR_T_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_SHORT_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_INT_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_LONG_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY
#define ATOMIC_LLONG_LOCK_FREE _GLIBCXX_ATOMIC_PROPERTY

  inline namespace _GLIBCXX_ATOMIC_NAMESPACE { }


  /// atomic_char
  typedef __atomic_base<char>  	       		atomic_char;

  /// atomic_schar
  typedef __atomic_base<signed char>	     	atomic_schar;

  /// atomic_uchar
  typedef __atomic_base<unsigned char>		atomic_uchar;

  /// atomic_short
  typedef __atomic_base<short>			atomic_short;

  /// atomic_ushort
  typedef __atomic_base<unsigned short>	 	atomic_ushort;

  /// atomic_int
  typedef __atomic_base<int>  	       		atomic_int;

  /// atomic_uint
  typedef __atomic_base<unsigned int>	     	atomic_uint;

  /// atomic_long
  typedef __atomic_base<long>  	       		atomic_long;

  /// atomic_ulong
  typedef __atomic_base<unsigned long>		atomic_ulong;

  /// atomic_llong
  typedef __atomic_base<long long>  		atomic_llong;

  /// atomic_ullong
  typedef __atomic_base<unsigned long long> 	atomic_ullong;

  /// atomic_wchar_t
  typedef __atomic_base<wchar_t>  		atomic_wchar_t;

  /// atomic_char16_t
  typedef __atomic_base<char16_t>  		atomic_char16_t;

  /// atomic_char32_t
  typedef __atomic_base<char32_t>  		atomic_char32_t;

  /// atomic_char32_t
  typedef __atomic_base<char32_t>  		atomic_char32_t;


  /// atomic_int_least8_t
  typedef __atomic_base<int_least8_t>  		atomic_int_least8_t;

  /// atomic_uint_least8_t
  typedef __atomic_base<uint_least8_t>	       	atomic_uint_least8_t;

  /// atomic_int_least16_t
  typedef __atomic_base<int_least16_t>	       	atomic_int_least16_t;

  /// atomic_uint_least16_t
  typedef __atomic_base<uint_least16_t>	       	atomic_uint_least16_t;

  /// atomic_int_least32_t
  typedef __atomic_base<int_least32_t>	       	atomic_int_least32_t;

  /// atomic_uint_least32_t
  typedef __atomic_base<uint_least32_t>	       	atomic_uint_least32_t;

  /// atomic_int_least64_t
  typedef __atomic_base<int_least64_t>	       	atomic_int_least64_t;

  /// atomic_uint_least64_t
  typedef __atomic_base<uint_least64_t>	       	atomic_uint_least64_t;


  /// atomic_int_fast8_t
  typedef __atomic_base<int_fast8_t>  		atomic_int_fast8_t;

  /// atomic_uint_fast8_t
  typedef __atomic_base<uint_fast8_t>	      	atomic_uint_fast8_t;

  /// atomic_int_fast16_t
  typedef __atomic_base<int_fast16_t>	      	atomic_int_fast16_t;

  /// atomic_uint_fast16_t
  typedef __atomic_base<uint_fast16_t>	      	atomic_uint_fast16_t;

  /// atomic_int_fast32_t
  typedef __atomic_base<int_fast32_t>	      	atomic_int_fast32_t;

  /// atomic_uint_fast32_t
  typedef __atomic_base<uint_fast32_t>	      	atomic_uint_fast32_t;

  /// atomic_int_fast64_t
  typedef __atomic_base<int_fast64_t>	      	atomic_int_fast64_t;

  /// atomic_uint_fast64_t
  typedef __atomic_base<uint_fast64_t>	      	atomic_uint_fast64_t;


  /// atomic_intptr_t
  typedef __atomic_base<intptr_t>  	       	atomic_intptr_t;

  /// atomic_uintptr_t
  typedef __atomic_base<uintptr_t>  	       	atomic_uintptr_t;

  /// atomic_size_t
  typedef __atomic_base<size_t>	 	       	atomic_size_t;

  /// atomic_intmax_t
  typedef __atomic_base<intmax_t>  	       	atomic_intmax_t;

  /// atomic_uintmax_t
  typedef __atomic_base<uintmax_t>  	       	atomic_uintmax_t;

  /// atomic_ptrdiff_t
  typedef __atomic_base<ptrdiff_t>  	       	atomic_ptrdiff_t;


#define ATOMIC_VAR_INIT(_VI) { _VI }

  template<typename _Tp>
    struct atomic;

  template<typename _Tp>
    struct atomic<_Tp*>;

  // @} group atomics

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
