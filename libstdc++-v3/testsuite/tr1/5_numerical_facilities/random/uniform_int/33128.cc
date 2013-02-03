// Copyright (C) 2007-2013 Free Software Foundation, Inc.
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

// 5.1.7.1 Class template uniform_int
// 5.1.1 [7] Table 17

#include <tr1/random>
#include <testsuite_hooks.h>

// libstdc++/33128
void test01()
{
  bool test __attribute__((unused)) = true;

  std::tr1::mt19937 rng;
  std::tr1::uniform_int<> six(1,6);
  std::tr1::variate_generator<std::tr1::mt19937, std::tr1::uniform_int<> >
            die(rng, six);

  int val = die();
  VERIFY( val >= 1 && val <= 6 );
}

int main()
{
  test01();
  return 0;
}
