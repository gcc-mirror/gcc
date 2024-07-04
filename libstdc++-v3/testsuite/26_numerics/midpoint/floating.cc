// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <numeric>
#include <limits>
#include <cfloat>
#include <testsuite_hooks.h>

void
test01()
{
  using lim = std::numeric_limits<double>;

  VERIFY( std::midpoint(2.0, 4.0) == 3.0 );
  VERIFY( std::midpoint(0.0, 0.4) == 0.2 );
  VERIFY( std::midpoint(0.0, -0.0) == 0.0 );
  VERIFY( std::midpoint(9e9, -9e9) == 0.0 );

  VERIFY( std::midpoint(lim::max(), lim::max()) == lim::max() );
}

void
test02()
{
  using lim = std::numeric_limits<float>;

  VERIFY( std::midpoint(2.0f, 4.0f) == 3.0f );
  VERIFY( std::midpoint(0.0f, 0.4f) == 0.2f );
  VERIFY( std::midpoint(0.0f, -0.0f) == 0.0f );
  VERIFY( std::midpoint(9e9f, -9e9f) == 0.0f );
}

void
test03()
{
  using lim = std::numeric_limits<long double>;

  VERIFY( std::midpoint(2.0l, 4.0l) == 3.0l );
  VERIFY( std::midpoint(0.0l, 0.4l) == 0.2l );
  VERIFY( std::midpoint(0.0l, -0.0l) == 0.0l );
  VERIFY( std::midpoint(9e9l, -9e9l) == 0.0l );
}

namespace test04
{
  // https://gcc.gnu.org/ml/libstdc++/2019-03/msg00065.html
  constexpr double d = DBL_MIN + DBL_TRUE_MIN;
  static_assert( std::midpoint(d, d) == d );

  constexpr float f = FLT_MIN + FLT_TRUE_MIN;
  static_assert( std::midpoint(f, f) == f );

  constexpr long double l = LDBL_MIN + LDBL_TRUE_MIN;
  static_assert( std::midpoint(l, l) == l );
}

int main()
{
  test01();
  test02();
  test03();
}
