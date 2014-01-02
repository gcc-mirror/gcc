// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// Tuple

#include <tuple>
#include <testsuite_hooks.h>

using namespace std;

bool test __attribute__((unused)) = true;

#define TEST1(x) VERIFY( x == x && !(x != x) && x <= x && !(x < x) )

int
main()
{
  int i=0;
  int j=0;
  int k=2;
  tuple<int, int, int> a(0, 0, 0);
  tuple<int, int, int> b(0, 0, 1);
  tuple<int& , int& , int&> c(i,j,k);
  tuple<const int&, const int&, const int&> d(c);
  TEST1(a);
  TEST1(b);
  TEST1(c);
  TEST1(d);
  VERIFY(!(a > a) && !(b > b));
  VERIFY(a >= a && b >= b);
  VERIFY(a < b && !(b < a) && a <= b && !(b <= a));
  VERIFY(b > a && !(a > b) && b >= a && !(a >= b));  
}
