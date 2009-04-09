// { dg-options "-x c" }
// { dg-do compile }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

#include <stdatomic.h>

void test01()
{
  typedef memory_order t_01;
  memory_order t_02 __attribute__((unused)) = memory_order_relaxed;
  memory_order t_03 __attribute__((unused)) = memory_order_acquire;
  memory_order t_04 __attribute__((unused)) = memory_order_release;
  memory_order t_05 __attribute__((unused)) = memory_order_acq_rel;
  memory_order t_06 __attribute__((unused)) = memory_order_seq_cst;

  typedef atomic_flag t_07;

  // atomics for builtins types
  typedef atomic_bool t_08;
  typedef atomic_char t_09;
  typedef atomic_schar t_10;
  typedef atomic_uchar t_11;
  typedef atomic_short t_12;
  typedef atomic_ushort t_13;
  typedef atomic_int t_14;
  typedef atomic_uint t_15;
  typedef atomic_long t_16;
  typedef atomic_ulong t_17;
  typedef atomic_llong t_18;
  typedef atomic_ullong t_19;
  typedef atomic_wchar_t t_20;
  typedef atomic_char16_t t_21;
  typedef atomic_char32_t t_22;

  // atomics for standard typedefs
  typedef atomic_int_least8_t t_23;
  typedef atomic_uint_least8_t t_24;
  typedef atomic_int_least16_t t_25;
  typedef atomic_uint_least16_t t_26;
  typedef atomic_int_least32_t t_27;
  typedef atomic_uint_least32_t t_28;
  typedef atomic_int_least64_t t_29;
  typedef atomic_uint_least64_t t_30;
  typedef atomic_int_fast8_t t_31;
  typedef atomic_uint_fast8_t t_32;
  typedef atomic_int_fast16_t t_33;
  typedef atomic_uint_fast16_t t_34;
  typedef atomic_int_fast32_t t_35;
  typedef atomic_uint_fast32_t t_36;
  typedef atomic_int_fast64_t t_37;
  typedef atomic_uint_fast64_t t_38;
  typedef atomic_intptr_t t_39;
  typedef atomic_uintptr_t t_40;
  typedef atomic_size_t t_41;
  typedef atomic_ssize_t t_42;
  typedef atomic_ptrdiff_t t_43;
  typedef atomic_intmax_t t_44;
  typedef atomic_uintmax_t t_45;

  typedef atomic_address t_46;
}
