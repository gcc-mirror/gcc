// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2010-03-16  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

// 26.5.8.4.1 Class template normal_distribution [rand.dist.norm.normal]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::normal_distribution<double> u(5.0, 2.0), v, w;

  VERIFY( v == w );
  VERIFY( !(u == v) );
}

void
test02()
{
  std::normal_distribution<double> u(5.0, 2.0), v(u);
  VERIFY( u == v );
  u.reset();
  VERIFY( u == v );

  std::minstd_rand0 g1, g2;
  (void) u(g1); // u._M_saved_available = true
  VERIFY( !(u == v) );
  (void) v(g2); // v._M_saved_available = true
  VERIFY( u == v );
  u.reset();    // u._M_saved_available = false
  VERIFY( !(u == v) );
  v.reset();    // v._M_saved_available = false
  VERIFY( u == v );
}

int main()
{
  test01();
  test02();
  return 0;
}
