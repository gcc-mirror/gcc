// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <utility>

// LWG 2296 std::addressof should be constexpr

#ifndef __cpp_lib_addressof_constexpr
# error "Feature-test macro for constexpr addressof missing"
#elif __cpp_lib_addressof_constexpr != 201603
# error "Feature-test macro for constexpr addressof has wrong value"
#endif

int i;
constexpr int* pi = std::addressof(i);
static_assert( pi == &i, "&i" );

struct X {
  void operator&();
  constexpr X* addr() { return this; }
  constexpr const X* addr() const { return this; }
};
X x;
constexpr X* px = std::addressof(x);
static_assert( px == x.addr(), "x.addr()" );
const X cx;
constexpr const X* pcx = std::addressof(cx);
static_assert( pcx == cx.addr(), "cx.addr()" );

void
test01()
{
  static int i2;
  static_assert( std::addressof(i2) == &i2, "&i2" );

  static X x2;
  static_assert( std::addressof(x2) == x2.addr(), "x2.addr()" );
}
