// 2006-06-04  Stephen M. Webb <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

// 5.1.4.2 class template mersenne_twister [tr.rand.eng.mers]
// 5.1.1 Table 16 line 2 seed ctor
// 5.1.1 (4) point 2: Gen is a fundamental type.

#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::tr1;

  double seed = 2.0;
  mersenne_twister<
    unsigned long, 32, 624, 397, 31,
    0x9908b0dful, 11, 7,
    0x9d2c5680ul, 15,
    0xefc60000ul, 18> x(seed);
}

int main()
{
  test01();
  return 0;
}
