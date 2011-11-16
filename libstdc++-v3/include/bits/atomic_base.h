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
  __calculate_memory_order(memory_order __m) noexcept
  {
    const bool __cond1 = __m == memory_order_release;
    const bool __cond2 = __m == memory_order_acq_rel;
    memory_order __mo1(__cond1 ? memory_order_relaxed : __m);
    memory_order __mo2(__cond2 ? memory_order_acquire : __mo1);
    return __mo2;
  }

  inline void
  atomic_thread_fence(memory_order __m) noexcept
  {
    __atomic_thread_fence (__m);
  }

  inline void
  atomic_signal_fence(memory_order __m) noexcept
  {
    __atomic_thread_fence (__m);
  }

  /// kill_dependency
  template<typename _Tp>
    inline _Tp
    kill_dependency(_Tp __y) noexcept
    {
      _Tp __ret(__y);
      return __ret;
    }

  /// Lock-free Property

#define LOCKFREE_PROP(T) (__atomic_always_lock_free (sizeof (T), 0) ? 2 : 1)

#define ATOMIC_BOOL_LOCK_FREE		LOCKFREE_PROP (bool)
#define ATOMIC_CHAR_LOCK_FREE 		LOCKFREE_PROP (char)
#define ATOMIC_CHAR16_T_LOCK_FREE	LOCKFREE_PROP (char16_t)
#define ATOMIC_CHAR32_T_LOCK_FREE	LOCKFREE_PROP (char32_t)
#define ATOMIC_WCHAR_T_LOCK_FREE	LOCKFREE_PROP (wchar_t)
#define ATOMIC_SHORT_LOCK_FREE		LOCKFREE_PROP (short)
#define ATOMIC_INT_LOCK_FREE		LOCKFREE_PROP (int)
#define ATOMIC_LONG_LOCK_FREE		LOCKFREE_PROP (long)
#define ATOMIC_LLONG_LOCK_FREE		LOCKFREE_PROP (long long)
#define ATOMIC_POINTER_LOCK_FREE	LOCKFREE_PROP (void *)

  // Base types for atomics.
  template<typename _IntTp>
    struct __atomic_base;

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

  /// atomic_flag
  struct atomic_flag : public __atomic_flag_base
  {
    atomic_flag() noexcept = default;
    ~atomic_flag() noexcept = default;
    atomic_flag(const atomic_flag&) = delete;
    atomic_flag& operator=(const atomic_flag&) = delete;
    atomic_flag& operator=(const atomic_flag&) volatile = delete;

    // Conversion to ATOMIC_FLAG_INIT.
    atomic_flag(bool __i) noexcept : __atomic_flag_base({ __i }) { }

    bool
    test_and_set(memory_order __m = memory_order_seq_cst) noexcept
    {
      return __atomic_test_and_set (&_M_i, __m);
    }

    bool
    test_and_set(memory_order __m = memory_order_seq_cst) volatile noexcept
    {
      return __atomic_test_and_set (&_M_i, __m);
    }

    void
    clear(memory_order __m = memory_order_seq_cst) noexcept
    {
      __glibcxx_assert(__m != memory_order_consume);
      __glibcxx_assert(__m != memory_order_acquire);
      __glibcxx_assert(__m != memory_order_acq_rel);

      __atomic_clear (&_M_i, __m);
    }

    void
    clear(memory_order __m = memory_order_seq_cst) volatile noexcept
    {
      __glibcxx_assert(__m != memory_order_consume);
      __glibcxx_assert(__m != memory_order_acquire);
      __glibcxx_assert(__m != memory_order_acq_rel);

      __atomic_clear (&_M_i, __m);
    }
  };


  /// Base class for atomic integrals.
  //
  // For each of the integral types, define atomic_[integral type] struct
  //
  // atomic_bool     bool
  // atomic_char     char
  // atomic_schar    signed char
  // atomic_uchar    unsigned char
  // atomic_short    short
  // atomic_ushort   unsigned short
  // atomic_int      int
  // atomic_uint     unsigned int
  // atomic_long     long
  // atomic_ulong    unsigned long
  // atomic_llong    long long
  // atomic_ullong   unsigned long long
  // atomic_char16_t char16_t
  // atomic_char32_t char32_t
  // atomic_wchar_t  wchar_t
  //
  // NB: Assuming _ITp is an integral scalar type that is 1, 2, 4, or
  // 8 bytes, since that is what GCC built-in functions for atomic
  // memory access expect.
  template<typename _ITp>
    struct __atomic_base
    {
    private:
      typedef _ITp 	__int_type;

      __int_type 	_M_i;

    public:
      __atomic_base() noexcept = default;
      ~__atomic_base() noexcept = default;
      __atomic_base(const __atomic_base&) = delete;
      __atomic_base& operator=(const __atomic_base&) = delete;
      __atomic_base& operator=(const __atomic_base&) volatile = delete;

      // Requires __int_type convertible to _M_i.
      constexpr __atomic_base(__int_type __i) noexcept : _M_i (__i) { }

      operator __int_type() const noexcept
      { return load(); }

      operator __int_type() const volatile noexcept
      { return load(); }

      __int_type
      operator=(__int_type __i) noexcept
      {
	store(__i);
	return __i;
      }

      __int_type
      operator=(__int_type __i) volatile noexcept
      {
	store(__i);
	return __i;
      }

      __int_type
      operator++(int) noexcept
      { return fetch_add(1); }

      __int_type
      operator++(int) volatile noexcept
      { return fetch_add(1); }

      __int_type
      operator--(int) noexcept
      { return fetch_sub(1); }

      __int_type
      operator--(int) volatile noexcept
      { return fetch_sub(1); }

      __int_type
      operator++() noexcept
      { return __atomic_add_fetch(&_M_i, 1, memory_order_seq_cst); }

      __int_type
      operator++() volatile noexcept
      { return __atomic_add_fetch(&_M_i, 1, memory_order_seq_cst); }

      __int_type
      operator--() noexcept
      { return __atomic_sub_fetch(&_M_i, 1, memory_order_seq_cst); }

      __int_type
      operator--() volatile noexcept
      { return __atomic_sub_fetch(&_M_i, 1, memory_order_seq_cst); }

      __int_type
      operator+=(__int_type __i) noexcept
      { return __atomic_add_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator+=(__int_type __i) volatile noexcept
      { return __atomic_add_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator-=(__int_type __i) noexcept
      { return __atomic_sub_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator-=(__int_type __i) volatile noexcept
      { return __atomic_sub_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator&=(__int_type __i) noexcept
      { return __atomic_and_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator&=(__int_type __i) volatile noexcept
      { return __atomic_and_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator|=(__int_type __i) noexcept
      { return __atomic_or_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator|=(__int_type __i) volatile noexcept
      { return __atomic_or_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator^=(__int_type __i) noexcept
      { return __atomic_xor_fetch(&_M_i, __i, memory_order_seq_cst); }

      __int_type
      operator^=(__int_type __i) volatile noexcept
      { return __atomic_xor_fetch(&_M_i, __i, memory_order_seq_cst); }

      bool
      is_lock_free() const noexcept
      { return __atomic_is_lock_free (sizeof (_M_i), &_M_i); }

      bool
      is_lock_free() const volatile noexcept
      { return __atomic_is_lock_free (sizeof (_M_i), &_M_i); }

      void
      store(__int_type __i, memory_order __m = memory_order_seq_cst) noexcept
      {
	__glibcxx_assert(__m != memory_order_acquire);
	__glibcxx_assert(__m != memory_order_acq_rel);
	__glibcxx_assert(__m != memory_order_consume);

	__atomic_store_n(&_M_i, __i, __m);
      }

      void
      store(__int_type __i,
	    memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	__glibcxx_assert(__m != memory_order_acquire);
	__glibcxx_assert(__m != memory_order_acq_rel);
	__glibcxx_assert(__m != memory_order_consume);

	__atomic_store_n(&_M_i, __i, __m);
      }

      __int_type
      load(memory_order __m = memory_order_seq_cst) const noexcept
      {
	__glibcxx_assert(__m != memory_order_release);
	__glibcxx_assert(__m != memory_order_acq_rel);

	return __atomic_load_n(&_M_i, __m);
      }

      __int_type
      load(memory_order __m = memory_order_seq_cst) const volatile noexcept
      {
	__glibcxx_assert(__m != memory_order_release);
	__glibcxx_assert(__m != memory_order_acq_rel);

	return __atomic_load_n(&_M_i, __m);
      }

      __int_type
      exchange(__int_type __i,
	       memory_order __m = memory_order_seq_cst) noexcept
      {
	return __atomic_exchange_n(&_M_i, __i, __m);
      }


      __int_type
      exchange(__int_type __i,
	       memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	return __atomic_exchange_n(&_M_i, __i, __m);
      }

      bool
      compare_exchange_weak(__int_type& __i1, __int_type __i2,
			    memory_order __m1, memory_order __m2) noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_i, &__i1, __i2, 1, __m1, __m2);
      }

      bool
      compare_exchange_weak(__int_type& __i1, __int_type __i2,
			    memory_order __m1,
			    memory_order __m2) volatile noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_i, &__i1, __i2, 1, __m1, __m2);
      }

      bool
      compare_exchange_weak(__int_type& __i1, __int_type __i2,
			    memory_order __m = memory_order_seq_cst) noexcept
      {
	return compare_exchange_weak(__i1, __i2, __m,
				     __calculate_memory_order(__m));
      }

      bool
      compare_exchange_weak(__int_type& __i1, __int_type __i2,
		   memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	return compare_exchange_weak(__i1, __i2, __m,
				     __calculate_memory_order(__m));
      }

      bool
      compare_exchange_strong(__int_type& __i1, __int_type __i2,
			      memory_order __m1, memory_order __m2) noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_i, &__i1, __i2, 0, __m1, __m2);
      }

      bool
      compare_exchange_strong(__int_type& __i1, __int_type __i2,
			      memory_order __m1,
			      memory_order __m2) volatile noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_i, &__i1, __i2, 0, __m1, __m2);
      }

      bool
      compare_exchange_strong(__int_type& __i1, __int_type __i2,
			      memory_order __m = memory_order_seq_cst) noexcept
      {
	return compare_exchange_strong(__i1, __i2, __m,
				       __calculate_memory_order(__m));
      }

      bool
      compare_exchange_strong(__int_type& __i1, __int_type __i2,
		 memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	return compare_exchange_strong(__i1, __i2, __m,
				       __calculate_memory_order(__m));
      }

      __int_type
      fetch_add(__int_type __i,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_add(&_M_i, __i, __m); }

      __int_type
      fetch_add(__int_type __i,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_add(&_M_i, __i, __m); }

      __int_type
      fetch_sub(__int_type __i,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_sub(&_M_i, __i, __m); }

      __int_type
      fetch_sub(__int_type __i,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_sub(&_M_i, __i, __m); }

      __int_type
      fetch_and(__int_type __i,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_and(&_M_i, __i, __m); }

      __int_type
      fetch_and(__int_type __i,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_and(&_M_i, __i, __m); }

      __int_type
      fetch_or(__int_type __i,
	       memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_or(&_M_i, __i, __m); }

      __int_type
      fetch_or(__int_type __i,
	       memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_or(&_M_i, __i, __m); }

      __int_type
      fetch_xor(__int_type __i,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_xor(&_M_i, __i, __m); }

      __int_type
      fetch_xor(__int_type __i,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_xor(&_M_i, __i, __m); }
    };


  /// Partial specialization for pointer types.
  template<typename _PTp>
    struct __atomic_base<_PTp*>
    {
    private:
      typedef _PTp* 	__pointer_type;

      __pointer_type 	_M_p;

    public:
      __atomic_base() noexcept = default;
      ~__atomic_base() noexcept = default;
      __atomic_base(const __atomic_base&) = delete;
      __atomic_base& operator=(const __atomic_base&) = delete;
      __atomic_base& operator=(const __atomic_base&) volatile = delete;

      // Requires __pointer_type convertible to _M_p.
      constexpr __atomic_base(__pointer_type __p) noexcept : _M_p (__p) { }

      operator __pointer_type() const noexcept
      { return load(); }

      operator __pointer_type() const volatile noexcept
      { return load(); }

      __pointer_type
      operator=(__pointer_type __p) noexcept
      {
	store(__p);
	return __p;
      }

      __pointer_type
      operator=(__pointer_type __p) volatile noexcept
      {
	store(__p);
	return __p;
      }

      __pointer_type
      operator++(int) noexcept
      { return fetch_add(1); }

      __pointer_type
      operator++(int) volatile noexcept
      { return fetch_add(1); }

      __pointer_type
      operator--(int) noexcept
      { return fetch_sub(1); }

      __pointer_type
      operator--(int) volatile noexcept
      { return fetch_sub(1); }

      __pointer_type
      operator++() noexcept
      { return __atomic_add_fetch(&_M_p, 1, memory_order_seq_cst); }

      __pointer_type
      operator++() volatile noexcept
      { return __atomic_add_fetch(&_M_p, 1, memory_order_seq_cst); }

      __pointer_type
      operator--() noexcept
      { return __atomic_sub_fetch(&_M_p, 1, memory_order_seq_cst); }

      __pointer_type
      operator--() volatile noexcept
      { return __atomic_sub_fetch(&_M_p, 1, memory_order_seq_cst); }

      __pointer_type
      operator+=(ptrdiff_t __d) noexcept
      { return __atomic_add_fetch(&_M_p, __d, memory_order_seq_cst); }

      __pointer_type
      operator+=(ptrdiff_t __d) volatile noexcept
      { return __atomic_add_fetch(&_M_p, __d, memory_order_seq_cst); }

      __pointer_type
      operator-=(ptrdiff_t __d) noexcept
      { return __atomic_sub_fetch(&_M_p, __d, memory_order_seq_cst); }

      __pointer_type
      operator-=(ptrdiff_t __d) volatile noexcept
      { return __atomic_sub_fetch(&_M_p, __d, memory_order_seq_cst); }

      bool
      is_lock_free() const noexcept
      { return __atomic_is_lock_free (sizeof (_M_p), &_M_p); }

      bool
      is_lock_free() const volatile noexcept
      { return __atomic_is_lock_free (sizeof (_M_p), &_M_p); }

      void
      store(__pointer_type __p,
	    memory_order __m = memory_order_seq_cst) noexcept
      {
	__glibcxx_assert(__m != memory_order_acquire);
	__glibcxx_assert(__m != memory_order_acq_rel);
	__glibcxx_assert(__m != memory_order_consume);

	__atomic_store_n(&_M_p, __p, __m);
      }

      void
      store(__pointer_type __p,
	    memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	__glibcxx_assert(__m != memory_order_acquire);
	__glibcxx_assert(__m != memory_order_acq_rel);
	__glibcxx_assert(__m != memory_order_consume);

	__atomic_store_n(&_M_p, __p, __m);
      }

      __pointer_type
      load(memory_order __m = memory_order_seq_cst) const noexcept
      {
	__glibcxx_assert(__m != memory_order_release);
	__glibcxx_assert(__m != memory_order_acq_rel);

	return __atomic_load_n(&_M_p, __m);
      }

      __pointer_type
      load(memory_order __m = memory_order_seq_cst) const volatile noexcept
      {
	__glibcxx_assert(__m != memory_order_release);
	__glibcxx_assert(__m != memory_order_acq_rel);

	return __atomic_load_n(&_M_p, __m);
      }

      __pointer_type
      exchange(__pointer_type __p,
	       memory_order __m = memory_order_seq_cst) noexcept
      {
	return __atomic_exchange_n(&_M_p, __p, __m);
      }


      __pointer_type
      exchange(__pointer_type __p,
	       memory_order __m = memory_order_seq_cst) volatile noexcept
      {
	return __atomic_exchange_n(&_M_p, __p, __m);
      }

      bool
      compare_exchange_strong(__pointer_type& __p1, __pointer_type __p2,
			      memory_order __m1,
			      memory_order __m2) noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_p, &__p1, __p2, 0, __m1, __m2);
      }

      bool
      compare_exchange_strong(__pointer_type& __p1, __pointer_type __p2,
			      memory_order __m1,
			      memory_order __m2) volatile noexcept
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);

	return __atomic_compare_exchange_n(&_M_p, &__p1, __p2, 0, __m1, __m2);
      }

      __pointer_type
      fetch_add(ptrdiff_t __d,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_add(&_M_p, __d, __m); }

      __pointer_type
      fetch_add(ptrdiff_t __d,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_add(&_M_p, __d, __m); }

      __pointer_type
      fetch_sub(ptrdiff_t __d,
		memory_order __m = memory_order_seq_cst) noexcept
      { return __atomic_fetch_sub(&_M_p, __d, __m); }

      __pointer_type
      fetch_sub(ptrdiff_t __d,
		memory_order __m = memory_order_seq_cst) volatile noexcept
      { return __atomic_fetch_sub(&_M_p, __d, __m); }
    };

  // @} group atomics

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif
