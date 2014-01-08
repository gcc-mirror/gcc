// { dg-options "-std=gnu++11" }
// { dg-require-cstdint "" }
//
// 2013-11-18  Edward M. Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// 26.5.8.4.5 Class template rice_distribution [rand.dist.ext.hypergeometric]

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test [[gnu::unused]] = true;

  __gnu_cxx::hypergeometric_distribution<unsigned int> u(20, 3, 2), v, w;

  VERIFY( v == w );
  VERIFY( !(u == v) );
}

int
main()
{
  test01();
  return 0;
}
