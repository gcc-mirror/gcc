// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2009-08-14  Edward M. Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

// 26.4.8.4.1 Class template normal_distribution [rand.dist.norm.normal]
// 26.4.2.4 Concept RandomNumberDistribution [rand.concept.dist]

#include <random>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringstream str;
  std::normal_distribution<double> u(5.0, 2.0), v;
  std::minstd_rand0 rng;

  u(rng); // advance
  str << u;

  str >> v;
  VERIFY( u == v );
}

void
test_pr105502()
{
  // PR libstdc++/105502 std::normal_distribution deserialization issue
  std::stringstream str;
  std::normal_distribution<> d{1, 2}, d2;
  std::minstd_rand0 g;
  str << d;
  VERIFY( str );
  str >> d2;
  VERIFY( str );
  VERIFY( d == d2 );

  (void) d(g); // sets d._M_saved_available = true
  str.str("");
  str.clear();
  str << d;
  VERIFY( str );
  str >> d2;
  VERIFY( str );
  VERIFY( d == d2 );

  (void) d(g); // sets d._M_saved_available = false
  str.str("");
  str.clear();
  str << d;
  VERIFY( str );
  str >> d2;
  VERIFY( str );
  VERIFY( d == d2 );
}

int main()
{
  test01();
  test_pr105502();
}
