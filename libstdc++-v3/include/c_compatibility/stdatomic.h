// -*- C++ -*- compatibility header.

// Copyright (C) 2008 Free Software Foundation, Inc.
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

/** @file stdatomic.h
 *  This is a Standard C++ Library header.
 */

#include <bits/c++config.h>
#include <stddef.h>
#include <stdbool.h> // XXX need to define bool w/o stdbool.h in tr1/cstdbool

#ifndef _GLIBCXX_STDATOMIC_H
#define _GLIBCXX_STDATOMIC_H 1

_GLIBCXX_BEGIN_NAMESPACE(std)
_GLIBCXX_BEGIN_EXTERN_C

  /// Enumeration for memory_order
  typedef enum memory_order 
    {
      memory_order_relaxed, 
      memory_order_acquire, 
      memory_order_release,
      memory_order_acq_rel, 
      memory_order_seq_cst
    } memory_order;


  // Base for atomic_flag.
  struct __atomic_flag_base
  {
    bool _M_b;
  };

  // Base for atomic_address
  struct __atomic_address_base
  {
    void* _M_i;
  };

  // POD base classes for atomic intgral types.
  struct __atomic_bool_base
  {
    bool _M_i;
  };

  struct __atomic_char_base
  {
    char _M_i;
  };

  struct __atomic_schar_base
  {
    signed char _M_i;
  };

  struct __atomic_uchar_base
  {
    unsigned char _M_i;
  };

  struct __atomic_short_base
  {
    short _M_i;
  };

  struct __atomic_ushort_base
  {
    unsigned short _M_i;
  };

  struct __atomic_int_base
  {
    int _M_i;
  };

  struct __atomic_uint_base
  {
    unsigned int _M_i;
  };

  struct __atomic_long_base
  {
    long _M_i;
  };

  struct __atomic_ulong_base
  {
    unsigned long _M_i;
  };

  struct __atomic_llong_base
  {
    long long _M_i;
  };

  struct __atomic_ullong_base
  {
    unsigned long long _M_i;
  };

  struct __atomic_wchar_t_base
  {
    wchar_t _M_i;
  };

  // Switch atomic integral base types based on C or C++.  In
  // addition, for "C" only provide type-generic macros for atomic
  // operations. (As C++ accomplishes the same thing with sets of
  // overloaded functions.
#ifdef __cplusplus

#define ATOMIC_FLAG_INIT { { false } }
#define _ATOMIC_MEMBER_ ((__a)->_M_base._M_i)

extern "C++"
{
  struct atomic_flag;
  struct atomic_address;
  struct atomic_bool;
  struct atomic_char;
  struct atomic_schar;
  struct atomic_uchar;
  struct atomic_short;
  struct atomic_ushort;
  struct atomic_int;
  struct atomic_uint;
  struct atomic_long;
  struct atomic_ulong;
  struct atomic_llong;
  struct atomic_ullong;
  struct atomic_wchar_t;
  template<typename _Tp>
    struct atomic;
}
#else

#define ATOMIC_FLAG_INIT { false }
#define _ATOMIC_MEMBER_ ((__a)->_M_i)

  typedef struct __atomic_flag_base 	atomic_flag;
  typedef struct __atomic_address_base 	atomic_address;
  typedef struct __atomic_bool_base	atomic_bool;
  typedef struct __atomic_char_base 	atomic_char;
  typedef struct __atomic_schar_base 	atomic_schar;
  typedef struct __atomic_uchar_base 	atomic_uchar;
  typedef struct __atomic_short_base 	atomic_short;
  typedef struct __atomic_ushort_base 	atomic_ushort;
  typedef struct __atomic_int_base 	atomic_int;
  typedef struct __atomic_uint_base 	atomic_uint;
  typedef struct __atomic_long_base 	atomic_long;
  typedef struct __atomic_ulong_base 	atomic_ulong;
  typedef struct __atomic_llong_base 	atomic_llong;
  typedef struct __atomic_ullong_base 	atomic_ullong;
  typedef struct __atomic_wchar_t_base 	atomic_wchar_t;

#define atomic_is_lock_free(__a)				\
  false

#define atomic_load(__a)					\
  _ATOMIC_LOAD_(__a, memory_order_seq_cst)

#define atomic_load_explicit(__a, __x)				\
  _ATOMIC_LOAD_(__a, __x)

#define atomic_store(__a, __m)					\
  _ATOMIC_STORE_(__a, __m, memory_order_seq_cst)

#define atomic_store_explicit(__a, __m, __x)			\
  _ATOMIC_STORE_(__a, __m, __x)

#define atomic_swap(__a, __m)					\
  _ATOMIC_MODIFY_(__a, =, __m, memory_order_seq_cst)

#define atomic_swap_explicit(__a, __m, __x)			\
  _ATOMIC_MODIFY_(__a, =, __m, __x)

#define atomic_compare_swap(__a, __e, __m)			\
  _ATOMIC_CMPSWP_(__a, __e, __m, memory_order_seq_cst)

#define atomic_compare_swap_explicit(__a, __e, __m, __x, __y)	\
  _ATOMIC_CMPSWP_(__a, __e, __m, __x)

#define atomic_fence(__a, __x)					\
  ({ _ATOMIC_FENCE_(__a, __x); })

#define atomic_fetch_add_explicit(__a, __m, __x)		\
  _ATOMIC_MODIFY_(__a, +=, __m, __x)

#define atomic_fetch_add(__a, __m)				\
  _ATOMIC_MODIFY_(__a, +=, __m, memory_order_seq_cst)

#define atomic_fetch_sub_explicit(__a, __m, __x)		\
  _ATOMIC_MODIFY_(__a, -=, __m, __x)

#define atomic_fetch_sub(__a, __m)				\
  _ATOMIC_MODIFY_(__a, -=, __m, memory_order_seq_cst)

#define atomic_fetch_and_explicit(__a, __m, __x)		\
  _ATOMIC_MODIFY_(__a, &=, __m, __x)

#define atomic_fetch_and(__a, __m)				\
  _ATOMIC_MODIFY_(__a, &=, __m, memory_order_seq_cst)

#define atomic_fetch_or_explicit(__a, __m, __x) 		\
  _ATOMIC_MODIFY_(__a, |=, __m, __x)

#define atomic_fetch_or(__a, __m)				\
  _ATOMIC_MODIFY_(__a, |=, __m, memory_order_seq_cst)

#define atomic_fetch_xor_explicit(__a, __m, __x)		\
  _ATOMIC_MODIFY_(__a, ^=, __m, __x)

#define atomic_fetch_xor(__a, __m)				\
  _ATOMIC_MODIFY_(__a, ^=, __m, memory_order_seq_cst)

#endif
  
  // Typedefs for other atomic integral types.
  typedef atomic_schar 		atomic_int_least8_t;
  typedef atomic_uchar 		atomic_uint_least8_t;
  typedef atomic_short 		atomic_int_least16_t;
  typedef atomic_ushort 	atomic_uint_least16_t;
  typedef atomic_int 		atomic_int_least32_t;
  typedef atomic_uint 		atomic_uint_least32_t;
  typedef atomic_llong 		atomic_int_least64_t;
  typedef atomic_ullong 	atomic_uint_least64_t;

  typedef atomic_schar 		atomic_int_fast8_t;
  typedef atomic_uchar 		atomic_uint_fast8_t;
  typedef atomic_short 		atomic_int_fast16_t;
  typedef atomic_ushort 	atomic_uint_fast16_t;
  typedef atomic_int 		atomic_int_fast32_t;
  typedef atomic_uint 		atomic_uint_fast32_t;
  typedef atomic_llong 		atomic_int_fast64_t;
  typedef atomic_ullong 	atomic_uint_fast64_t;

  typedef atomic_long 		atomic_intptr_t;
  typedef atomic_ulong 		atomic_uintptr_t;

  typedef atomic_long 		atomic_ssize_t;
  typedef atomic_ulong 		atomic_size_t;

  typedef atomic_llong 		atomic_intmax_t;
  typedef atomic_ullong 	atomic_uintmax_t;

  typedef atomic_long 		atomic_ptrdiff_t;

  typedef atomic_int_least16_t	atomic_char16_t;
  typedef atomic_int_least32_t	atomic_char32_t;

  // Accessor functions for atomic_flag.
  extern bool 
  atomic_flag_test_and_set(volatile atomic_flag*);
  
  extern bool 
  atomic_flag_test_and_set_explicit(volatile atomic_flag*, memory_order);
  
  extern void 
  atomic_flag_clear(volatile atomic_flag*);
  
  extern void 
  atomic_flag_clear_explicit(volatile atomic_flag*, memory_order);
  
  extern void 
  atomic_flag_fence(const volatile atomic_flag*, memory_order);
  
  extern void 
  __atomic_flag_wait_explicit(volatile atomic_flag*, memory_order);
  
  extern volatile atomic_flag* 
  __atomic_flag_for_address(const volatile void* __z) __attribute__((const));
   
  // External object.
  extern const atomic_flag atomic_global_fence_compatibility;
  
  /// 29.2 Lock-free Property
#define ATOMIC_INTEGRAL_LOCK_FREE 0
#define ATOMIC_ADDRESS_LOCK_FREE 0

  // Implementation specific defines.
#define _ATOMIC_LOAD_(__a, __x)						\
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	\
    volatile atomic_flag* __g = __atomic_flag_for_address(__p); 	\
    __atomic_flag_wait_explicit(__g, __x);				\
    __typeof__ _ATOMIC_MEMBER_ __r = *__p;				\
    atomic_flag_clear_explicit(__g, __x);		       		\
    __r; })

#define _ATOMIC_STORE_(__a, __m, __x)					\
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	\
    __typeof__(__m) __v = (__m);			       		\
    volatile atomic_flag* __g = __atomic_flag_for_address(__p); 	\
    __atomic_flag_wait_explicit(__g, __x);				\
    *__p = __v;								\
    atomic_flag_clear_explicit(__g, __x);		       		\
    __v; })

#define _ATOMIC_MODIFY_(__a, __o, __m, __x)				\
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	\
    __typeof__(__m) __v = (__m);			       		\
    volatile atomic_flag* __g = __atomic_flag_for_address(__p); 	\
    __atomic_flag_wait_explicit(__g, __x);				\
    __typeof__ _ATOMIC_MEMBER_ __r = *__p;				\
    *__p __o __v;					       		\
    atomic_flag_clear_explicit(__g, __x);		       		\
    __r; })

#define _ATOMIC_CMPSWP_(__a, __e, __m, __x)				\
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	\
    __typeof__(__e) __q = (__e);			       		\
    __typeof__(__m) __v = (__m);			       		\
    bool __r;						       		\
    volatile atomic_flag* __g = __atomic_flag_for_address(__p); 	\
    __atomic_flag_wait_explicit(__g, __x);				\
    __typeof__ _ATOMIC_MEMBER_ __t__ = *__p;		       		\
    if (__t__ == *__q) { *__p = __v; __r = true; }			\
    else { *__q = __t__; __r = false; }		       			\
    atomic_flag_clear_explicit(__g, __x);		       		\
    __r; })

#define _ATOMIC_FENCE_(__a, __x)			       		\
  ({ volatile __typeof__ _ATOMIC_MEMBER_* __p = &_ATOMIC_MEMBER_;	\
    volatile atomic_flag* __g = __atomic_flag_for_address(__p); 	\
    atomic_flag_fence(__g, __x);			       		\
  })

_GLIBCXX_END_EXTERN_C
_GLIBCXX_END_NAMESPACE

#ifdef __cplusplus
// Inject into global namespace iff C++.
using std::memory_order;
using std::memory_order_relaxed;
using std::memory_order_acquire;
using std::memory_order_release;
using std::memory_order_acq_rel;
using std::memory_order_seq_cst;

using std::atomic_flag;

using std::atomic_bool;
using std::atomic_char;
using std::atomic_schar;
using std::atomic_uchar;
using std::atomic_short;
using std::atomic_ushort;
using std::atomic_int;
using std::atomic_uint;
using std::atomic_long;
using std::atomic_ulong;
using std::atomic_llong;
using std::atomic_ullong;
using std::atomic_wchar_t;

using std::atomic_address;
using std::atomic;

#endif

#endif
