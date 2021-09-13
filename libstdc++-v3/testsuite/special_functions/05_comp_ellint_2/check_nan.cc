// { dg-do run { target c++11 } }
// { dg-require-c-std "" }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }
// { dg-add-options ieee }

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// 8.1.5 comp_ellint_2

#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  float kf = std::numeric_limits<float>::quiet_NaN();
  double kd = std::numeric_limits<double>::quiet_NaN();
  long double kl = std::numeric_limits<long double>::quiet_NaN();

  float a = std::comp_ellint_2(kf);
  float b = std::comp_ellint_2f(kf);
  double c = std::comp_ellint_2(kd);
  long double d = std::comp_ellint_2(kl);
  long double e = std::comp_ellint_2l(kl);

  VERIFY(std::isnan(a));
  VERIFY(std::isnan(b));
  VERIFY(std::isnan(c));
  VERIFY(std::isnan(d));
  VERIFY(std::isnan(e));

  return;
}

int
main()
{
  test01();
  return 0;
}

