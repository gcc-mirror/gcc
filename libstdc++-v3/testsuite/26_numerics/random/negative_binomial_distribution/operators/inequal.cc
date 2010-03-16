// { dg-options "-std=c++0x" }
// { dg-require-cstdint "" }
//
// 2010-03-16  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010 Free Software Foundation, Inc.
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

// 26.5.8.2.4 Class template negative_binomial_distribution
// [rand.dist.bern.negbin]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::negative_binomial_distribution<int> u(3, 0.75), v, w;

  VERIFY( u != v );
  VERIFY( !(v != w) );  
}

int main()
{
  test01();
  return 0;
}
