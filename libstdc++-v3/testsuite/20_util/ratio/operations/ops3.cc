// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008, 2009 Free Software Foundation
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

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::ratio_multiply<
    std::ratio<2, INTMAX_MAX>,
    std::ratio<INTMAX_MAX, 2>>::type r1;

  VERIFY( r1.num == 1 );
  VERIFY( r1.den == 1 );

  std::ratio_multiply<
    std::ratio<INTMAX_MAX, 2>,
    std::ratio<2 , INTMAX_MAX - 1>>::type r2;
  
  VERIFY( r2.num == INTMAX_MAX );
  VERIFY( r2.den == INTMAX_MAX - 1 );
}

void
test02()
{  
  bool test __attribute__((unused)) = true;
  
  std::ratio_divide<
    std::ratio<INTMAX_MAX, 2>,
    std::ratio<INTMAX_MAX, 2>>::type r1;

  VERIFY( r1.num == 1 );
  VERIFY( r1.den == 1 );

  std::ratio_divide<
    std::ratio<INTMAX_MAX-1, 2>,
    std::ratio<INTMAX_MAX, 2>>::type r2;
  
  VERIFY( r2.num == INTMAX_MAX - 1 );
  VERIFY( r2.den == INTMAX_MAX );
}

int main()
{
  test01();
  test02();
  return 0;
}
