// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

// 26.4.8.4.2 Class template lognormal_distribution [rand.dist.norm.lognormal]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::lognormal_distribution<> u(5.0, 2.0);
  VERIFY( u.m() == 5.0 );
  VERIFY( u.s() == 2.0 );
  typedef std::lognormal_distribution<>::result_type result_type;
  VERIFY( u.min() == 0.0 );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

void
test02()
{
  using param_type = std::lognormal_distribution<>::param_type;
  const param_type p(5.0, 2.0);
  std::lognormal_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );
  typedef std::lognormal_distribution<>::result_type result_type;
  VERIFY( u.min() == 0.0 );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

int main()
{
  test01();
  test02();
}
