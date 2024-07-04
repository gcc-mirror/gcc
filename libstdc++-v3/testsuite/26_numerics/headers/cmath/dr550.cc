// { dg-do run { target c++11 } }
// 2008-05-26  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

#include <cmath>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

// DR 550. What should the return type of pow(float,int) be?
void test01()
{
  using __gnu_test::check_ret_type;

  const int          i1 = 1;
  const float        f1 = 1.0f;
  const double       d1 = 1.0;
  const long double ld1 = 1.0l;

  check_ret_type<double>(std::pow(f1, i1));
  VERIFY( std::pow(f1, i1) == std::pow(double(f1), double(i1)) );
  check_ret_type<double>(std::pow(d1, i1));
  check_ret_type<long double>(std::pow(ld1, i1));
}

int main()
{
  test01();
  return 0;
}
