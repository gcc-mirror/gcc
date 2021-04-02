// { dg-do run { target c++11 } }
// { dg-require-c-std "" }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }
// { dg-add-options ieee }
// { dg-skip-if "no extensions in strict dialects" { *-*-* } { "-std=c++*" } }

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

// conf_hyperg

#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  float af = std::numeric_limits<float>::quiet_NaN();
  double ad = std::numeric_limits<double>::quiet_NaN();
  long double al = std::numeric_limits<long double>::quiet_NaN();

  float cf = 3.0F;
  double cd = 3.0;
  long double cl = 3.0L;

  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  float a = __gnu_cxx::conf_hyperg(af, cf, xf);
  float b = __gnu_cxx::conf_hypergf(af, cf, xf);
  double c = __gnu_cxx::conf_hyperg(ad, cd, xd);
  long double d = __gnu_cxx::conf_hyperg(al, cl, xl);
  long double e = __gnu_cxx::conf_hypergl(al, cl, xl);

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
  float af = 2.0F;
  double ad = 2.0;
  long double al = 2.0L;

  float cf = std::numeric_limits<float>::quiet_NaN();
  double cd = std::numeric_limits<double>::quiet_NaN();
  long double cl = std::numeric_limits<long double>::quiet_NaN();

  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  float a = __gnu_cxx::conf_hyperg(af, cf, xf);
  float b = __gnu_cxx::conf_hypergf(af, cf, xf);
  double c = __gnu_cxx::conf_hyperg(ad, cd, xd);
  long double d = __gnu_cxx::conf_hyperg(al, cl, xl);
  long double e = __gnu_cxx::conf_hypergl(al, cl, xl);

  VERIFY(std::isnan(a));
  VERIFY(std::isnan(b));
  VERIFY(std::isnan(c));
  VERIFY(std::isnan(d));
  VERIFY(std::isnan(e));

  return;
}

void
test03()
{
  float af = 2.0F;
  double ad = 2.0;
  long double al = 2.0L;

  float cf = 3.0F;
  double cd = 3.0;
  long double cl = 3.0L;

  float xf = std::numeric_limits<float>::quiet_NaN();
  double xd = std::numeric_limits<double>::quiet_NaN();
  long double xl = std::numeric_limits<long double>::quiet_NaN();

  float a = __gnu_cxx::conf_hyperg(af, cf, xf);
  float b = __gnu_cxx::conf_hypergf(af, cf, xf);
  double c = __gnu_cxx::conf_hyperg(ad, cd, xd);
  long double d = __gnu_cxx::conf_hyperg(al, cl, xl);
  long double e = __gnu_cxx::conf_hypergl(al, cl, xl);

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
  test03();
  return 0;
}

