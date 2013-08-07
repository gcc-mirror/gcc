// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
//
// 2008-11-24  Edward M. Smith-Rowland <3dw4rd@verizon.net>
// 2012-09-04  Ulrich Drepper <drepper@gmail.com>
//
// Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  __gnu_cxx::normal_mv_distribution<2> u({5.0, 4.0}, {4.0, 9.0});
  VERIFY( u.mean()[0] == 5.0 );
  VERIFY( u.mean()[1] == 4.0 );
  VERIFY( u.varcov()[0] == 2.0 );
  VERIFY( u.varcov()[1] == 0.0 );
  VERIFY( u.varcov()[2] == 3.0 );
  typedef __gnu_cxx::normal_mv_distribution<2>::result_type result_type;
  VERIFY( u.min()[0] == std::numeric_limits<result_type::value_type>::lowest() );
  VERIFY( u.max()[0] == std::numeric_limits<result_type::value_type>::max() );
  VERIFY( u.min()[1] == std::numeric_limits<result_type::value_type>::lowest() );
  VERIFY( u.max()[1] == std::numeric_limits<result_type::value_type>::max() );
}

int main()
{
  test01();
  return 0;
}
