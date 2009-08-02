// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
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

// 26.4.8.4.4 Class template cauchy_distribution [rand.dist.norm.cauchy]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::cauchy_distribution<> u(5.0, 2.0);
  VERIFY( u.a() == 5.0 );
  VERIFY( u.b() == 2.0 );
  typedef std::cauchy_distribution<>::result_type result_type;
  VERIFY( u.min() == std::numeric_limits<result_type>::min() );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

int main()
{
  test01();
  return 0;
}
