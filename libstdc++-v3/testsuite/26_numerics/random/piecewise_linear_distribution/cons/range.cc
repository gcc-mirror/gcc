// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-12-03  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// 26.4.8.5.3 Class template piecewise_linear_distribution [rand.dist.samp.plinear]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::vector<double> x = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0};
  std::vector<double> wt = {0.0, 1.0, 2.5, 1.5, 3.5, 0.0};
  std::piecewise_linear_distribution<> u(x.begin(), x.end(), wt.begin());
  std::vector<double> interval = u.intervals();
  std::vector<double> density = u.densities();
  VERIFY( interval.size() == 6 );
  VERIFY( interval[0] == 0.0 );
  VERIFY( interval[5] == 5.0 );
  VERIFY( density.size() == 6 );
  VERIFY( density[0] == 0.0 );
  VERIFY( density[4] == 3.5 / 8.5 );
}

int main()
{
  test01();
  return 0;
}
