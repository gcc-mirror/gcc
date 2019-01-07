// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <ratio>
#include <testsuite_hooks.h>

typedef std::ratio<1, INTMAX_MAX> one_over_max;
typedef std::ratio<2, INTMAX_MAX> two_over_max;
typedef std::ratio<INTMAX_MAX, 1> max_over_one;
typedef std::ratio<INTMAX_MAX, 2> max_over_two;

void
test01()
{
  std::ratio_add<one_over_max, one_over_max>::type r1;

  VERIFY( r1.num == two_over_max::num);
  VERIFY( r1.den == two_over_max::den);

  std::ratio_add<
    std::ratio<INTMAX_MAX / 2, INTMAX_MAX / 2>,
    std::ratio<INTMAX_MAX / 2 , INTMAX_MAX / 2 + 1>>::type r2;
  
  VERIFY( r2.num == INTMAX_MAX );
  VERIFY( r2.den == (INTMAX_MAX / 2) + 1 );
}

void
test02()
{
  std::ratio_subtract<one_over_max, one_over_max>::type r1;

  VERIFY( r1.num == 0);
  VERIFY( r1.den == 1);

  std::ratio_subtract<
    std::ratio<INTMAX_MAX / 2, INTMAX_MAX / 2>,
    std::ratio<INTMAX_MAX / 2 , INTMAX_MAX / 2 + 1>>::type r2;
  
  VERIFY( r2.num == 1 );
  VERIFY( r2.den == (INTMAX_MAX / 2) + 1 );
}

int main()
{
  test01();
  test02();
  return 0;
}
