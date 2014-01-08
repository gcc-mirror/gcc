// { dg-options "-std=c++11" }
// { dg-require-cstdint "" }
//
// 2012-09-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

// 26.4.8.3.* Class template k_distribution [rand.dist.ext.k]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  __gnu_cxx::k_distribution<> u;
  VERIFY( u.lambda() == 1.0 );
  VERIFY( u.mu() == 1.0 );
  VERIFY( u.nu() == 1.0 );
  VERIFY( u.min() == 0.0 );
  typedef __gnu_cxx::k_distribution<>::result_type result_type;
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

int
main()
{
  test01();
  return 0;
}
