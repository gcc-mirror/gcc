// -*- C++ -*- header.

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

/** @file bits/atomic_0.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _GLIBCXX_ATOMIC_0_H
#define _GLIBCXX_ATOMIC_0_H 1

#pragma GCC system_header

// _GLIBCXX_BEGIN_NAMESPACE(std)

  // 0 == __atomic0 == Never lock-free
namespace __atomic0
{
  struct atomic_flag;

  // Implementation specific defines.
#define _ATOMIC_LOAD_(__a, __x)						   \
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	   \
    volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);     \
    __atomic_flag_wait_explicit(__g, __x);				   \
    __typeof__ _ATOMIC_MEMBER_ __r = *__p;				   \
    atomic_flag_clear_explicit(__g, __x);		       		   \
    __r; })

#define _ATOMIC_STORE_(__a, __m, __x)					   \
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	   \
    __typeof__(__m) __v = (__m);			       		   \
    volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);     \
    __atomic_flag_wait_explicit(__g, __x);				   \
    *__p = __v;								   \
    atomic_flag_clear_explicit(__g, __x);		       		   \
    __v; })

#define _ATOMIC_MODIFY_(__a, __o, __m, __x)				   \
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	   \
    __typeof__(__m) __v = (__m);			       		   \
    volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);     \
    __atomic_flag_wait_explicit(__g, __x);				   \
    __typeof__ _ATOMIC_MEMBER_ __r = *__p;				   \
    *__p __o __v;					       		   \
    atomic_flag_clear_explicit(__g, __x);		       		   \
    __r; })

#define _ATOMIC_CMPEXCHNG_(__a, __e, __m, __x)				   \
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	   \
    __typeof__(__e) __q = (__e);			       		   \
    __typeof__(__m) __v = (__m);			       		   \
    bool __r;						       		   \
    volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);     \
    __atomic_flag_wait_explicit(__g, __x);				   \
    __typeof__ _ATOMIC_MEMBER_ __t__ = *__p;		       		   \
    if (__t__ == *__q) { *__p = __v; __r = true; }			   \
    else { *__q = __t__; __r = false; }		       			   \
    atomic_flag_clear_explicit(__g, __x);		       		   \
    __r; })

  /// atomic_flag
  struct atomic_flag : public __atomic_flag_base
  {
    atomic_flag() = default;
    ~atomic_flag() = default;
    atomic_flag(const atomic_flag&) = delete;
    atomic_flag& operator=(const atomic_flag&) = delete;

    // Conversion to ATOMIC_FLAG_INIT.
    atomic_flag(bool __i): __atomic_flag_base({ __i }) { }

    bool
    test_and_set(memory_order __m = memory_order_seq_cst) volatile;

    void
    clear(memory_order __m = memory_order_seq_cst) volatile;
  };

  /// 29.4.2, address types
  struct atomic_address
  {
  private:
    void* _M_i;

  public:
    atomic_address() = default;
    ~atomic_address() = default;
    atomic_address(const atomic_address&) = delete;
    atomic_address& operator=(const atomic_address&) = delete;

    atomic_address(void* __v) { _M_i = __v; }

    bool
    is_lock_free() const volatile
    { return false; }

    void
    store(void* __v, memory_order __m = memory_order_seq_cst) volatile
    {
      __glibcxx_assert(__m != memory_order_acquire);
      __glibcxx_assert(__m != memory_order_acq_rel);
      __glibcxx_assert(__m != memory_order_consume);
      _ATOMIC_STORE_(this, __v, __m);
    }

    void*
    load(memory_order __m = memory_order_seq_cst) const volatile
    {
      __glibcxx_assert(__m != memory_order_release);
      __glibcxx_assert(__m != memory_order_acq_rel);
      return _ATOMIC_LOAD_(this, __m);
    }

    void*
    exchange(void* __v, memory_order __m = memory_order_seq_cst) volatile
    { return _ATOMIC_MODIFY_(this, =, __v, __m); }

    bool
    compare_exchange_weak(void*& __v1, void* __v2, memory_order __m1,
			  memory_order __m2) volatile
    {
      __glibcxx_assert(__m2 != memory_order_release);
      __glibcxx_assert(__m2 != memory_order_acq_rel);
      __glibcxx_assert(__m2 <= __m1);
      return _ATOMIC_CMPEXCHNG_(this, &__v1, __v2, __m1);
    }

    bool
    compare_exchange_weak(void*& __v1, void* __v2,
			  memory_order __m = memory_order_seq_cst) volatile
    {
      return compare_exchange_weak(__v1, __v2, __m,
				   __calculate_memory_order(__m));
    }

    bool
    compare_exchange_strong(void*& __v1, void* __v2, memory_order __m1,
			    memory_order __m2) volatile
    {
      __glibcxx_assert(__m2 != memory_order_release);
      __glibcxx_assert(__m2 != memory_order_acq_rel);
      __glibcxx_assert(__m2 <= __m1);
      return _ATOMIC_CMPEXCHNG_(this, &__v1, __v2, __m1);
    }

    bool
    compare_exchange_strong(void*& __v1, void* __v2,
			  memory_order __m = memory_order_seq_cst) volatile
    {
      return compare_exchange_strong(__v1, __v2, __m,
				     __calculate_memory_order(__m));
    }

    void*
    fetch_add(ptrdiff_t __d, memory_order __m = memory_order_seq_cst) volatile
    {
      void* volatile* __p = &(_M_i);
      volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);
      __atomic_flag_wait_explicit(__g, __m);
      void* __r = *__p;
      *__p = (void*)((char*)(*__p) + __d);
      atomic_flag_clear_explicit(__g, __m);
      return __r;
    }

    void*
    fetch_sub(ptrdiff_t __d, memory_order __m = memory_order_seq_cst) volatile
    {
      void* volatile* __p = &(_M_i);
      volatile __atomic_flag_base* __g = __atomic_flag_for_address(__p);
      __atomic_flag_wait_explicit(__g, __m);
      void* __r = *__p;
      *__p = (void*)((char*)(*__p) - __d);
      atomic_flag_clear_explicit(__g, __m);
      return __r;
    }

    operator void*() const volatile
    { return load(); }

    void*
    operator=(void* __v) // XXX volatile
    {
      store(__v);
      return __v;
    }

    void*
    operator+=(ptrdiff_t __d) volatile
    { return fetch_add(__d) + __d; }

    void*
    operator-=(ptrdiff_t __d) volatile
    { return fetch_sub(__d) - __d; }
  };


  // 29.3.1 atomic integral types
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

  // Base type.
  // NB: Assuming _ITp is an integral scalar type that is 1, 2, 4, or 8 bytes,
  // since that is what GCC built-in functions for atomic memory access work on.
  template<typename _ITp>
    struct __atomic_base
    {
    private:
      typedef _ITp 	__integral_type;

      __integral_type 	_M_i;

    public:
      __atomic_base() = default;
      ~__atomic_base() = default;
      __atomic_base(const __atomic_base&) = delete;
      __atomic_base& operator=(const __atomic_base&) = delete;

      // Requires __integral_type convertible to _M_base._M_i.
      __atomic_base(__integral_type __i) { _M_i = __i; }

      operator __integral_type() const volatile
      { return load(); }

      __integral_type
      operator=(__integral_type __i) // XXX volatile
      {
	store(__i);
	return __i;
      }

      __integral_type
      operator++(int) volatile
      { return fetch_add(1); }

      __integral_type
      operator--(int) volatile
      { return fetch_sub(1); }

      __integral_type
      operator++() volatile
      { return fetch_add(1) + 1; }

      __integral_type
      operator--() volatile
      { return fetch_sub(1) - 1; }

      __integral_type
      operator+=(__integral_type __i) volatile
      { return fetch_add(__i) + __i; }

      __integral_type
      operator-=(__integral_type __i) volatile
      { return fetch_sub(__i) - __i; }

      __integral_type
      operator&=(__integral_type __i) volatile
      { return fetch_and(__i) & __i; }

      __integral_type
      operator|=(__integral_type __i) volatile
      { return fetch_or(__i) | __i; }

      __integral_type
      operator^=(__integral_type __i) volatile
      { return fetch_xor(__i) ^ __i; }

      bool
      is_lock_free() const volatile
      { return false; }

      void
      store(__integral_type __i,
	    memory_order __m = memory_order_seq_cst) volatile
      {
	__glibcxx_assert(__m != memory_order_acquire);
	__glibcxx_assert(__m != memory_order_acq_rel);
	__glibcxx_assert(__m != memory_order_consume);
	_ATOMIC_STORE_(this, __i, __m);
      }

      __integral_type
      load(memory_order __m = memory_order_seq_cst) const volatile
      {
	__glibcxx_assert(__m != memory_order_release);
	__glibcxx_assert(__m != memory_order_acq_rel);
	return _ATOMIC_LOAD_(this, __m);
      }

      __integral_type
      exchange(__integral_type __i,
	       memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, =, __i, __m); }

      bool
      compare_exchange_weak(__integral_type& __i1, __integral_type __i2,
			    memory_order __m1, memory_order __m2) volatile
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);
	return _ATOMIC_CMPEXCHNG_(this, &__i1, __i2, __m1);
      }

      bool
      compare_exchange_weak(__integral_type& __i1, __integral_type __i2,
			    memory_order __m = memory_order_seq_cst) volatile
      {
	return compare_exchange_weak(__i1, __i2, __m,
				     __calculate_memory_order(__m));
      }

      bool
      compare_exchange_strong(__integral_type& __i1, __integral_type __i2,
			      memory_order __m1, memory_order __m2) volatile
      {
	__glibcxx_assert(__m2 != memory_order_release);
	__glibcxx_assert(__m2 != memory_order_acq_rel);
	__glibcxx_assert(__m2 <= __m1);
	return _ATOMIC_CMPEXCHNG_(this, &__i1, __i2, __m1);
      }

      bool
      compare_exchange_strong(__integral_type& __i1, __integral_type __i2,
			      memory_order __m = memory_order_seq_cst) volatile
      {
	return compare_exchange_strong(__i1, __i2, __m,
				       __calculate_memory_order(__m));
      }

      __integral_type
      fetch_add(__integral_type __i,
		memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, +=, __i, __m); }

      __integral_type
      fetch_sub(__integral_type __i,
		memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, -=, __i, __m); }

      __integral_type
      fetch_and(__integral_type __i,
		memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, &=, __i, __m); }

      __integral_type
      fetch_or(__integral_type __i,
	       memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, |=, __i, __m); }

      __integral_type
      fetch_xor(__integral_type __i,
		memory_order __m = memory_order_seq_cst) volatile
      { return _ATOMIC_MODIFY_(this, ^=, __i, __m); }
    };


  /// atomic_bool
  // NB: No operators or fetch-operations for this type.
  struct atomic_bool
  {
  private:
    __atomic_base<bool>	_M_base;

  public:
    atomic_bool() = default;
    ~atomic_bool() = default;
    atomic_bool(const atomic_bool&) = delete;
    atomic_bool& operator=(const atomic_bool&) = delete;

    atomic_bool(bool __i) : _M_base(__i) { }

    bool
    operator=(bool __i) // XXX volatile
    { return _M_base.operator=(__i); }

    operator bool() const volatile
    { return _M_base.load(); }

    bool
    is_lock_free() const volatile
    { return _M_base.is_lock_free(); }

    void
    store(bool __i, memory_order __m = memory_order_seq_cst) volatile
    { _M_base.store(__i, __m); }

    bool
    load(memory_order __m = memory_order_seq_cst) const volatile
    { return _M_base.load(__m); }

    bool
    exchange(bool __i, memory_order __m = memory_order_seq_cst) volatile
    { return _M_base.exchange(__i, __m); }

    bool
    compare_exchange_weak(bool& __i1, bool __i2, memory_order __m1,
			  memory_order __m2) volatile
    { return _M_base.compare_exchange_weak(__i1, __i2, __m1, __m2); }

    bool
    compare_exchange_weak(bool& __i1, bool __i2,
			  memory_order __m = memory_order_seq_cst) volatile
    { return _M_base.compare_exchange_weak(__i1, __i2, __m); }

    bool
    compare_exchange_strong(bool& __i1, bool __i2, memory_order __m1,
			    memory_order __m2) volatile
    { return _M_base.compare_exchange_strong(__i1, __i2, __m1, __m2); }


    bool
    compare_exchange_strong(bool& __i1, bool __i2,
			    memory_order __m = memory_order_seq_cst) volatile
    { return _M_base.compare_exchange_strong(__i1, __i2, __m); }
  };

#undef _ATOMIC_LOAD_
#undef _ATOMIC_STORE_
#undef _ATOMIC_MODIFY_
#undef _ATOMIC_CMPEXCHNG_
} // namespace __atomic0

// _GLIBCXX_END_NAMESPACE

#endif
