// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
  // Not global scoped, only namespace std.
  using memory_order;
  using memory_order_relaxed;
  using memory_order_consume;
  using memory_order_acquire;
  using memory_order_release;
  using memory_order_acq_rel;
  using memory_order_seq_cst;

  using atomic_flag;

  using atomic_bool;
  using atomic_char;
  using atomic_schar;
  using atomic_uchar;
  using atomic_short;
  using atomic_ushort;
  using atomic_int;
  using atomic_uint;
  using atomic_long;
  using atomic_ulong;
  using atomic_llong;
  using atomic_ullong;
  using atomic_wchar_t;
  using atomic_char16_t;
  using atomic_char32_t;

  using atomic_int_least8_t;
  using atomic_uint_least8_t;
  using atomic_int_least16_t;
  using atomic_uint_least16_t;
  using atomic_int_least32_t;
  using atomic_uint_least32_t;
  using atomic_int_least64_t;
  using atomic_uint_least64_t;
  using atomic_int_fast8_t;
  using atomic_uint_fast8_t;
  using atomic_int_fast16_t;
  using atomic_uint_fast16_t;
  using atomic_int_fast32_t;
  using atomic_uint_fast32_t;
  using atomic_int_fast64_t;
  using atomic_uint_fast64_t;
  using atomic_intptr_t;
  using atomic_uintptr_t;
  using atomic_size_t;
  using atomic_ptrdiff_t;
  using atomic_intmax_t;
  using atomic_uintmax_t;

  using atomic_address;
}

// { dg-error "expected nested-name-specifier" "" { target *-*-* } 26 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 27 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 28 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 29 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 30 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 31 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 32 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 34 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 36 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 37 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 38 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 39 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 40 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 41 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 42 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 43 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 44 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 45 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 46 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 47 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 48 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 49 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 50 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 52 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 53 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 54 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 55 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 56 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 57 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 58 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 59 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 60 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 61 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 62 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 63 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 64 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 65 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 66 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 67 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 68 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 69 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 70 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 71 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 72 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 73 }
// { dg-error "expected nested-name-specifier" "" { target *-*-* } 75 }

// { dg-error "declared" "" { target *-*-* } 26 }
// { dg-error "declared" "" { target *-*-* } 27 }
// { dg-error "declared" "" { target *-*-* } 28 }
// { dg-error "declared" "" { target *-*-* } 29 }
// { dg-error "declared" "" { target *-*-* } 30 }
// { dg-error "declared" "" { target *-*-* } 31 }
// { dg-error "declared" "" { target *-*-* } 32 }
// { dg-error "declared" "" { target *-*-* } 34 }
// { dg-error "declared" "" { target *-*-* } 36 }
// { dg-error "declared" "" { target *-*-* } 37 }
// { dg-error "declared" "" { target *-*-* } 38 }
// { dg-error "declared" "" { target *-*-* } 39 }
// { dg-error "declared" "" { target *-*-* } 40 }
// { dg-error "declared" "" { target *-*-* } 41 }
// { dg-error "declared" "" { target *-*-* } 42 }
// { dg-error "declared" "" { target *-*-* } 43 }
// { dg-error "declared" "" { target *-*-* } 44 }
// { dg-error "declared" "" { target *-*-* } 45 }
// { dg-error "declared" "" { target *-*-* } 46 }
// { dg-error "declared" "" { target *-*-* } 47 }
// { dg-error "declared" "" { target *-*-* } 48 }
// { dg-error "declared" "" { target *-*-* } 49 }
// { dg-error "declared" "" { target *-*-* } 50 }
// { dg-error "declared" "" { target *-*-* } 52 }
// { dg-error "declared" "" { target *-*-* } 53 }
// { dg-error "declared" "" { target *-*-* } 54 }
// { dg-error "declared" "" { target *-*-* } 55 }
// { dg-error "declared" "" { target *-*-* } 56 }
// { dg-error "declared" "" { target *-*-* } 57 }
// { dg-error "declared" "" { target *-*-* } 58 }
// { dg-error "declared" "" { target *-*-* } 59 }
// { dg-error "declared" "" { target *-*-* } 60 }
// { dg-error "declared" "" { target *-*-* } 61 }
// { dg-error "declared" "" { target *-*-* } 62 }
// { dg-error "declared" "" { target *-*-* } 63 }
// { dg-error "declared" "" { target *-*-* } 64 }
// { dg-error "declared" "" { target *-*-* } 65 }
// { dg-error "declared" "" { target *-*-* } 66 }
// { dg-error "declared" "" { target *-*-* } 67 }
// { dg-error "declared" "" { target *-*-* } 68 }
// { dg-error "declared" "" { target *-*-* } 69 }
// { dg-error "declared" "" { target *-*-* } 70 }
// { dg-error "declared" "" { target *-*-* } 71 }
// { dg-error "declared" "" { target *-*-* } 72 }
// { dg-error "declared" "" { target *-*-* } 73 }
// { dg-error "declared" "" { target *-*-* } 75 }
