// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

// 26.4.8.2.4 Class template negative_binomial_distribution [rand.dist.bern.negbin]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::negative_binomial_distribution<> u(3, 0.75);
  VERIFY( u.k() == 3 );
  VERIFY( u.p() == 0.75 );
  typedef std::negative_binomial_distribution<>::result_type result_type;
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

void
test02()
{
  using param_type = std::negative_binomial_distribution<>::param_type;
  const param_type p(3, 0.75);
  std::negative_binomial_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );
  typedef std::negative_binomial_distribution<>::result_type result_type;
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

int main()
{
  test01();
  test02();
}
