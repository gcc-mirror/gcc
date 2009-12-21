// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2009 Free Software Foundation, Inc.
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
