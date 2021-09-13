// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <vector>

// PR libstdc++/89164

struct X
{
  X() = default;
  X(const X&) = delete;
};

void test01()
{
  X x[1];
  // Should not be able to create vector using uninitialized_copy:
  std::vector<X> v1{x, x+1};	// { dg-error "here" }

  // Should not be able to create vector using uninitialized_fill_n:
  std::vector<X> v2{2u, X{}};	// { dg-error "here" }
}

void test02()
{
#if __cplusplus >= 201703L
  struct Y : X { };
  // Can create initializer_list<Y> with C++17 guaranteed copy elision,
  // but shouldn't be able to copy from it with uninitialized_copy:
  std::vector<Y> v3{Y{}, Y{}, Y{}};   // { dg-error "here" "" { target c++17 } }
#endif
}

// { dg-error "must be constructible from input type" "" { target *-*-* } 0 }
