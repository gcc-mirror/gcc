// { dg-do run { target c++11 } }

// 2008-07-03 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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
  std::ratio_add<std::ratio<3,8>, std::ratio<5,12>>::type r;

  VERIFY( r.num == 19 );
  VERIFY( r.den == 24 );
}

void
test02()
{  
  std::ratio_subtract<std::ratio<3,8>, std::ratio<5,12>>::type r;

  VERIFY( r.num == -1 );
  VERIFY( r.den == 24 );
}

void
test03()
{
  std::ratio_multiply<std::ratio<3,8>, std::ratio<5,12>>::type r;

  VERIFY( r.num == 5 );
  VERIFY( r.den == 32 );
}

void
test04()
{
  std::ratio_divide<std::ratio<3,8>, std::ratio<5,12>>::type r;

  VERIFY( r.num == 9 );
  VERIFY( r.den == 10 );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
