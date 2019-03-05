// { dg-do run { target c++11 } }
// { dg-require-c-std "" }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }
// { dg-add-options ieee }

// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

// 8.1.3 beta

#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  float xf = std::numeric_limits<float>::quiet_NaN();
  double xd = std::numeric_limits<double>::quiet_NaN();
  long double xl = std::numeric_limits<long double>::quiet_NaN();

  float yf = 0.0F;
  double yd = 0.0;
  long double yl = 0.0L;

  float a = std::beta(xf, yf);
  float b = std::betaf(xf, yf);
  double c = std::beta(xd, yd);
  long double d = std::beta(xl, yl);
  long double e = std::betal(xl, yl);

  VERIFY(std::isnan(a));
  VERIFY(std::isnan(b));
  VERIFY(std::isnan(c));
  VERIFY(std::isnan(d));
  VERIFY(std::isnan(e));

  return;
}

void
test02()
{
  float xf = 1.0F;
  double xd = 1.0;
  long double xl = 1.0L;

  float yf = std::numeric_limits<float>::quiet_NaN();
  double yd = std::numeric_limits<double>::quiet_NaN();
  long double yl = std::numeric_limits<long double>::quiet_NaN();

  float a = std::beta(xf, yf);
  float b = std::betaf(xf, yf);
  double c = std::beta(xd, yd);
  long double d = std::beta(xl, yl);
  long double e = std::betal(xl, yl);

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
  test02();
  return 0;
}

