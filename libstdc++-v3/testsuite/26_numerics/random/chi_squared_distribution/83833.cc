// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-additional-options "-ffloat-store" { target { m68*-*-* || ia32 } } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::default_random_engine r1, r2;
  using chi = std::chi_squared_distribution<double>;
  chi::param_type p(5);
  chi d1(p);
  chi d2;
  d2.param(p);
  VERIFY( d1(r1) == d2(r2) ); // PR libstdc++/83833
}

int
main()
{
  test01();
}
