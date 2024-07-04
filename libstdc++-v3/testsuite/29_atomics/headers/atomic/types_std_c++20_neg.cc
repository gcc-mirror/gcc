// { dg-do compile { target c++20 } }
// { dg-require-cstdint "" }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
  using memory_order;		// { dg-error "expected nested-name-specifier" }
  // TODO add checks for new enumerators memory_order::relaxed etc.
  using memory_order_relaxed;	// { dg-error "expected nested-name-specifier" }
  using memory_order_consume;	// { dg-error "expected nested-name-specifier" }
  using memory_order_acquire;	// { dg-error "expected nested-name-specifier" }
  using memory_order_release;	// { dg-error "expected nested-name-specifier" }
  using memory_order_acq_rel;	// { dg-error "expected nested-name-specifier" }
  using memory_order_seq_cst;	// { dg-error "expected nested-name-specifier" }

  using atomic_flag;		// { dg-error "expected nested-name-specifier" }

  using atomic_bool;		// { dg-error "expected nested-name-specifier" }
  using atomic_char;		// { dg-error "expected nested-name-specifier" }
  using atomic_schar;		// { dg-error "expected nested-name-specifier" }
  using atomic_uchar;		// { dg-error "expected nested-name-specifier" }
  using atomic_short;		// { dg-error "expected nested-name-specifier" }
  using atomic_ushort;		// { dg-error "expected nested-name-specifier" }
  using atomic_int;		// { dg-error "expected nested-name-specifier" }
  using atomic_uint;		// { dg-error "expected nested-name-specifier" }
  using atomic_long;		// { dg-error "expected nested-name-specifier" }
  using atomic_ulong;		// { dg-error "expected nested-name-specifier" }
  using atomic_llong;		// { dg-error "expected nested-name-specifier" }
  using atomic_ullong;		// { dg-error "expected nested-name-specifier" }
  using atomic_wchar_t;		// { dg-error "expected nested-name-specifier" }
  using atomic_char8_t;		// { dg-error "expected nested-name-specifier" }
  using atomic_char16_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_char32_t;	// { dg-error "expected nested-name-specifier" }

  using atomic_int_least8_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_least8_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_least16_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_least16_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_least32_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_least32_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_least64_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_least64_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_fast8_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_fast8_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_fast16_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_fast16_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_fast32_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_fast32_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_int_fast64_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uint_fast64_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_intptr_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uintptr_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_size_t;		// { dg-error "expected nested-name-specifier" }
  using atomic_ptrdiff_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_intmax_t;	// { dg-error "expected nested-name-specifier" }
  using atomic_uintmax_t;	// { dg-error "expected nested-name-specifier" }

  using atomic_address;		// { dg-error "expected nested-name-specifier" }
  // Present in C++0x drafts but not final C++11 standard:
  using std::atomic_address;	// { dg-error "has not been declared" }
}
