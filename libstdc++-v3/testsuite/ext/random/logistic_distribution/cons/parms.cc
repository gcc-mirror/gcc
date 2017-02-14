// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2014-07-11  Edward M. Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// Class template logistic_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_cxx::logistic_distribution<> u(1.5, 3.0);
  VERIFY( u.a() == 1.5 );
  VERIFY( u.b() == 3.0 );

  typedef __gnu_cxx::logistic_distribution<>::result_type result_type;
  VERIFY( u.min() == -std::numeric_limits<result_type>::max() );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

void
test02()
{
  using param_type = __gnu_cxx::logistic_distribution<>::param_type;
  const param_type p(1.5, 3.0);
  __gnu_cxx::logistic_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );

  typedef __gnu_cxx::logistic_distribution<>::result_type result_type;
  VERIFY( u.min() == -std::numeric_limits<result_type>::max() );
  VERIFY( u.max() == std::numeric_limits<result_type>::max() );
}

int main()
{
  test01();
  test02();
}
