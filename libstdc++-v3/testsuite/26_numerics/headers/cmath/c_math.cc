// 1999-06-05
// Gabriel Dos Reis <dosreis@cmla.ens-cachan.fr>

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
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

#include <cmath>
#include <testsuite_hooks.h>

// test compilation.
int
test01()
{
  float a = 1.f;
  float b;
  std::modf(a, &b);
  return 0;
}

// need more extravagant checks than this, of course, but this used to core...
int
test02()
{
  std::sin(static_cast<float>(0));
  return 0;
}

// as did this.
int
test03()
{
  double powtest __attribute__((unused)) = std::pow(2., 0);
  return 0;
}

// this used to abort.
int
test04()
{
  float x[2] = {1, 2};
  float y = 3.4;
  std::modf(y, &x[0]);
  VERIFY(x[1] == 2);
  return 0;
}

int 
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
