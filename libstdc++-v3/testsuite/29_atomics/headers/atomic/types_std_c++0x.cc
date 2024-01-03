// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <atomic>

void test01()
{
  using std::memory_order;
  using std::memory_order_relaxed;
  using std::memory_order_consume;
  using std::memory_order_acquire;
  using std::memory_order_release;
  using std::memory_order_acq_rel;
  using std::memory_order_seq_cst;

  using std::atomic_flag;

  // atomics for builtins types
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
#ifdef _GLIBCXX_USE_CHAR8_T
  using std::atomic_char8_t;
#endif
  using std::atomic_char16_t;
  using std::atomic_char32_t;

  // atomics for standard typedefs
  using std::atomic_int_least8_t;
  using std::atomic_uint_least8_t;
  using std::atomic_int_least16_t;
  using std::atomic_uint_least16_t;
  using std::atomic_int_least32_t;
  using std::atomic_uint_least32_t;
  using std::atomic_int_least64_t;
  using std::atomic_uint_least64_t;
  using std::atomic_int_fast8_t;
  using std::atomic_uint_fast8_t;
  using std::atomic_int_fast16_t;
  using std::atomic_uint_fast16_t;
  using std::atomic_int_fast32_t;
  using std::atomic_uint_fast32_t;
  using std::atomic_int_fast64_t;
  using std::atomic_uint_fast64_t;
  using std::atomic_intptr_t;
  using std::atomic_uintptr_t;
  using std::atomic_size_t;
  using std::atomic_ptrdiff_t;
  using std::atomic_intmax_t;
  using std::atomic_uintmax_t;

  // DR 2441
  using std::atomic_int8_t;
  using std::atomic_uint8_t;
  using std::atomic_int16_t;
  using std::atomic_uint16_t;
  using std::atomic_int32_t;
  using std::atomic_uint32_t;
  using std::atomic_int64_t;
  using std::atomic_uint64_t;
}
