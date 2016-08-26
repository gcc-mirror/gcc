// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2010-03-16  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

// 26.5.8.5.2 Class template piecewise_constant_distribution
// [rand.dist.samp.pconst]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::vector<double> x = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0 };
  std::vector<double> wt = { 0.5, 1.0, 2.5, 1.5, 0.5 };
  std::piecewise_constant_distribution<double>
    u(x.begin(), x.end(), wt.begin()), v, w;

  VERIFY( v == w );
  VERIFY( !(u == v) );
}

int main()
{
  test01();
  return 0;
}
