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

// 26.4.8.2.2 Class template binomial_distribution [rand.dist.bern.bin]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::binomial_distribution<> u(3, 0.75);
  VERIFY( u.t() == 3 );
  VERIFY( u.p() == 0.75 );
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == u.t() );
}

void
test02()
{
  using param_type = std::binomial_distribution<>::param_type;
  const param_type p(3, 0.75);
  std::binomial_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == u.t() );
}

int main()
{
  test01();
  test02();
}
