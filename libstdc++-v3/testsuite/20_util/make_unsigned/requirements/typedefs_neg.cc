// { dg-do compile { target c++11 } }

// 2007-05-03  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

#include <type_traits>

struct pod_class { };

void test01()
{
  using std::make_unsigned;

  // Negative tests.
  using T1 = make_unsigned<bool>::type; // { dg-error "incomplete" }
  using T2 = make_unsigned<const bool>::type; // { dg-error "incomplete" }
  using T3 = make_unsigned<volatile bool>::type; // { dg-error "incomplete" }
  using T4 = make_unsigned<const volatile bool>::type; // { dg-error "incomplete" }

  using T5 = make_unsigned<pod_class>::type; // { dg-error "here" }

  using T6 = make_unsigned<int[4]>::type; // { dg-error "here" }

  using fn_type = void ();
  using T7 = make_unsigned<fn_type>::type; // { dg-error "here" }

  using T8 = make_unsigned<float>::type; // { dg-error "here" }
}

// { dg-error "invalid use of incomplete type" "" { target *-*-* } 0 }
