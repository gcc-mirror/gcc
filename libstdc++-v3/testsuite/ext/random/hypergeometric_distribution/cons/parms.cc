// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2013-11-18  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// Class template hypergeometric_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  __gnu_cxx::hypergeometric_distribution<> u(15, 3, 2);
  VERIFY( u.total_size() == 15 );
  VERIFY( u.successful_size() == 3 );
  VERIFY( u.total_draws() == 2 );
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == 2 );
}

void
test02()
{
  using param_type = __gnu_cxx::hypergeometric_distribution<>::param_type;
  const param_type p(15, 3, 2);
  __gnu_cxx::hypergeometric_distribution<> u(p);
  VERIFY( u.param() == p );
  VERIFY( u.param() != param_type{} );
  VERIFY( u.min() == 0 );
  VERIFY( u.max() == 2 );
}

int main()
{
  test01();
  test02();
}
