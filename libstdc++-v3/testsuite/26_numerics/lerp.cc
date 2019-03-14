// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <cmath>

#ifndef __cpp_lib_interpolate
# error "Feature-test macro for midpoint and lerp missing"
#elif __cpp_lib_interpolate != 201902L
# error "Feature-test macro for midpoint and lerp has wrong value"
#endif

#include <limits>
#include <testsuite_hooks.h>

void
test01()
{
  using lim = std::numeric_limits<double>;

  VERIFY( std::lerp(0.0, 1.0, 0.0) == 0.0 );
  VERIFY( std::lerp(-2.0, 10.0, 1.0) == 10.0 );
  VERIFY( std::lerp(2.0, -10.0, 1.0) == -10.0 );
  VERIFY( std::lerp(-8.0, 10.0, 0.5) == 1.0 );
  VERIFY( std::lerp(-10.0, 10.0, 0.25) == -5.0 );
  VERIFY( std::lerp(10.0, -10.0, 0.375) == 2.5 );

  VERIFY( std::lerp(2.0, 2.0, 200.0) == 2.0 );
  VERIFY( std::lerp(2.0, 4.0, 200.0) == 402.0 );
  VERIFY( std::lerp(2.0, 4.0, -20.0) == -38.0 );

  VERIFY( std::lerp(1.1, 30201.1, 0) == 1.1 );
  VERIFY( std::lerp(1.1, 30201.1, 1) == 30201.1 );
  VERIFY( std::lerp(1.1, -30201.1, 0) == 1.1 );
  VERIFY( std::lerp(1.1, -30201.1, 1) == -30201.1 );

  VERIFY( std::lerp(1.1, 1.1, lim::infinity()) == 1.1 );
  VERIFY( std::isfinite(std::lerp(1.1, 1.1+lim::min(), lim::max())) );

  VERIFY( std::lerp(lim::max(), lim::max(), 1) == lim::max() );
  VERIFY( std::lerp(lim::max(), lim::max()/9e9, 0) == lim::max() );
  VERIFY( std::lerp(lim::max()/9e9, lim::max(), 1) == lim::max() );
}

void
test02()
{
  using lim = std::numeric_limits<float>;

  VERIFY( std::lerp(0.0f, 1.0f, 0.0f) == 0.0f );
  VERIFY( std::lerp(-2.0f, 10.0f, 1.0f) == 10.0f );
  VERIFY( std::lerp(2.0f, -10.0f, 1.0f) == -10.0f );
  VERIFY( std::lerp(-8.0f, 10.0f, 0.5f) == 1.0f );
  VERIFY( std::lerp(-10.0f, 10.0f, 0.25f) == -5.0f );
  VERIFY( std::lerp(10.0f, -10.0f, 0.375f) == 2.5f );

  VERIFY( std::lerp(2.0f, 2.0f, 200.0f) == 2.0f );
  VERIFY( std::lerp(2.0f, 4.0f, 200.0f) == 402.0f );
  VERIFY( std::lerp(2.0f, 4.0f, -20.0f) == -38.0f );

  VERIFY( std::lerp(1.1f, 30201.1f, 0) == 1.1f );
  VERIFY( std::lerp(1.1f, 30201.1f, 1) == 30201.1f );
  VERIFY( std::lerp(1.1f, -30201.1f, 0) == 1.1f );
  VERIFY( std::lerp(1.1f, -30201.1f, 1) == -30201.1f );

  VERIFY( std::lerp(1.1f, 1.1f, lim::infinity()) == 1.1f );
  VERIFY( std::isfinite(std::lerp(1.1f, 1.1f+lim::min(), lim::max())) );

  VERIFY( std::lerp(lim::max(), lim::max(), 1) == lim::max() );
  VERIFY( std::lerp(lim::max(), lim::max()/9e9f, 0) == lim::max() );
  VERIFY( std::lerp(lim::max()/9e9f, lim::max(), 1) == lim::max() );
}

void
test03()
{
  using lim = std::numeric_limits<long double>;

  VERIFY( std::lerp(0.0l, 1.0l, 0.0l) == 0.0l );
  VERIFY( std::lerp(-2.0l, 10.0l, 1.0l) == 10.0l );
  VERIFY( std::lerp(2.0l, -10.0l, 1.0l) == -10.0l );
  VERIFY( std::lerp(-8.0l, 10.0l, 0.5l) == 1.0l );
  VERIFY( std::lerp(-10.0l, 10.0l, 0.25l) == -5.0l );
  VERIFY( std::lerp(10.0l, -10.0l, 0.375l) == 2.5l );

  VERIFY( std::lerp(2.0l, 2.0l, 200.0l) == 2.0l );
  VERIFY( std::lerp(2.0l, 4.0l, 200.0l) == 402.0l );
  VERIFY( std::lerp(2.0l, 4.0l, -20.0l) == -38.0l );

  VERIFY( std::lerp(1.1l, 30201.1l, 0) == 1.1l );
  VERIFY( std::lerp(1.1l, 30201.1l, 1) == 30201.1l );
  VERIFY( std::lerp(1.1l, -30201.1l, 0) == 1.1l );
  VERIFY( std::lerp(1.1l, -30201.1l, 1) == -30201.1l );

  VERIFY( std::lerp(1.1l, 1.1l, lim::infinity()) == 1.1l );
  VERIFY( std::isfinite(std::lerp(1.1l, 1.1l+lim::min(), lim::max())) );

  VERIFY( std::lerp(lim::max(), lim::max(), 1) == lim::max() );
  VERIFY( std::lerp(lim::max(), lim::max()/9e9l, 0) == lim::max() );
  VERIFY( std::lerp(lim::max()/9e9l, lim::max(), 1) == lim::max() );
}

int main()
{
  test01();
  test02();
  test03();
}
