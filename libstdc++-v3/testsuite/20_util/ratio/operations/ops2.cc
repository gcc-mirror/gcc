// { dg-options "-std=gnu++0x" }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

#include <ratio>
#include <testsuite_hooks.h>

#ifdef _GLIBCXX_USE_C99_STDINT_TR1

typedef std::ratio<1, INTMAX_MAX> one_over_max;
typedef std::ratio<2, INTMAX_MAX> two_over_max;
typedef std::ratio<INTMAX_MAX, 1> max_over_one;
typedef std::ratio<INTMAX_MAX, 2> max_over_two;

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::ratio_add<one_over_max, one_over_max>::type r1;

  VERIFY( r1.num == two_over_max::num);
  VERIFY( r1.den == two_over_max::den);

  std::ratio_add<
    std::ratio<INTMAX_MAX / 2, INTMAX_MAX / 2>,
    std::ratio<INTMAX_MAX / 2 , INTMAX_MAX / 2 + 1>>::type r2;
  
  VERIFY( r2.num == INTMAX_MAX);
  VERIFY( r2.den == (INTMAX_MAX / 2) + 1);
}

void
test02()
{  
  bool test __attribute__((unused)) = true;
  
  std::ratio_subtract<one_over_max, one_over_max>::type r1;

  VERIFY( r1.num == 0);
  VERIFY( r1.den == 1);

  std::ratio_subtract<
    std::ratio<INTMAX_MAX / 2, INTMAX_MAX / 2>,
    std::ratio<INTMAX_MAX / 2 , INTMAX_MAX / 2 + 1>>::type r2;
  
  VERIFY( r2.num == 1);
  VERIFY( r2.den == (INTMAX_MAX / 2) + 1);
}

#endif //_GLIBCXX_USE_C99_STDINT_TR1

int main()
{
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
  test01();
  test02();
#endif //_GLIBCXX_USE_C99_STDINT_TR1
  return 0;
}
