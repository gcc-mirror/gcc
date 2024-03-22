// { dg-require-c-std "" }
// { dg-add-options ieee }

// 2007-01-10  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

// 5.2.1.17 hyperg

#include <tr1/cmath>
#include <testsuite_hooks.h>

void
test01()
{
  float af = std::numeric_limits<float>::quiet_NaN();
  double ad = std::numeric_limits<double>::quiet_NaN();
  long double al = std::numeric_limits<long double>::quiet_NaN();

  float bf = 10.0F;
  double bd = 10.0;
  long double bl = 10.0L;

  float cf = 3.0F;
  double cd = 3.0;
  long double cl = 3.0L;

  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  float a = std::tr1::hyperg(af, bf, cf, xf);
  float b = std::tr1::hypergf(af, bf, cf, xf);
  double c = std::tr1::hyperg(ad, bd, cd, xd);
  long double d = std::tr1::hyperg(al, bl, cl, xl);
  long double e = std::tr1::hypergl(al, bl, cl, xl);

  VERIFY(std::tr1::isnan<float>(a));
  VERIFY(std::tr1::isnan<float>(b));
  VERIFY(std::tr1::isnan<double>(c));
  VERIFY(std::tr1::isnan<long double>(d));
  VERIFY(std::tr1::isnan<long double>(e));

  return;
}

void
test02()
{
  float af = 2.0F;
  double ad = 2.0;
  long double al = 2.0L;

  float bf = std::numeric_limits<float>::quiet_NaN();
  double bd = std::numeric_limits<double>::quiet_NaN();
  long double bl = std::numeric_limits<long double>::quiet_NaN();

  float cf = 3.0F;
  double cd = 3.0;
  long double cl = 3.0L;

  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  float a = std::tr1::hyperg(af, bf, cf, xf);
  float b = std::tr1::hypergf(af, bf, cf, xf);
  double c = std::tr1::hyperg(ad, bd, cd, xd);
  long double d = std::tr1::hyperg(al, bl, cl, xl);
  long double e = std::tr1::hypergl(al, bl, cl, xl);

  VERIFY(std::tr1::isnan<float>(a));
  VERIFY(std::tr1::isnan<float>(b));
  VERIFY(std::tr1::isnan<double>(c));
  VERIFY(std::tr1::isnan<long double>(d));
  VERIFY(std::tr1::isnan<long double>(e));

  return;
}

void
test03()
{
  float af = 2.0F;
  double ad = 2.0;
  long double al = 2.0L;

  float bf = 10.0F;
  double bd = 10.0;
  long double bl = 10.0L;

  float cf = std::numeric_limits<float>::quiet_NaN();
  double cd = std::numeric_limits<double>::quiet_NaN();
  long double cl = std::numeric_limits<long double>::quiet_NaN();

  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  float a = std::tr1::hyperg(af, bf, cf, xf);
  float b = std::tr1::hypergf(af, bf, cf, xf);
  double c = std::tr1::hyperg(ad, bd, cd, xd);
  long double d = std::tr1::hyperg(al, bl, cl, xl);
  long double e = std::tr1::hypergl(al, bl, cl, xl);

  VERIFY(std::tr1::isnan<float>(a));
  VERIFY(std::tr1::isnan<float>(b));
  VERIFY(std::tr1::isnan<double>(c));
  VERIFY(std::tr1::isnan<long double>(d));
  VERIFY(std::tr1::isnan<long double>(e));

  return;
}

void
test04()
{
  float af = 2.0F;
  double ad = 2.0;
  long double al = 2.0L;

  float bf = 10.0F;
  double bd = 10.0;
  long double bl = 10.0L;

  float cf = 3.0F;
  double cd = 3.0;
  long double cl = 3.0L;

  float xf = std::numeric_limits<float>::quiet_NaN();
  double xd = std::numeric_limits<double>::quiet_NaN();
  long double xl = std::numeric_limits<long double>::quiet_NaN();

  float a = std::tr1::hyperg(af, bf, cf, xf);
  float b = std::tr1::hypergf(af, bf, cf, xf);
  double c = std::tr1::hyperg(ad, bd, cd, xd);
  long double d = std::tr1::hyperg(al, bl, cl, xl);
  long double e = std::tr1::hypergl(al, bl, cl, xl);

  VERIFY(std::tr1::isnan<float>(a));
  VERIFY(std::tr1::isnan<float>(b));
  VERIFY(std::tr1::isnan<double>(c));
  VERIFY(std::tr1::isnan<long double>(d));
  VERIFY(std::tr1::isnan<long double>(e));

  return;
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

