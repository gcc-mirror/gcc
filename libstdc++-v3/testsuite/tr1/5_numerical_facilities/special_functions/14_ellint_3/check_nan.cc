// { dg-require-c-std "" }
// { dg-options "-mieee" { target sh*-*-* } }

// 2007-01-10  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

// 5.2.1.13 ellint_3

#include <tr1/cmath>
#include <testsuite_hooks.h>

void
test01()
{
  float kf = std::numeric_limits<float>::quiet_NaN();
  double kd = std::numeric_limits<double>::quiet_NaN();
  long double kl = std::numeric_limits<long double>::quiet_NaN();

  float nuf = 0.2F;
  double nud = 0.2;
  long double nul = 0.2L;

  float phif = std::atan2(1.0F, 1.0F);
  double phid = std::atan2(1.0, 1.0);
  long double phil = std::atan2(1.0L, 1.0L);

  float a = std::tr1::ellint_3(kf, nuf, phif);
  float b = std::tr1::ellint_3f(kf, nuf, phif);
  double c = std::tr1::ellint_3(kd, nud, phid);
  long double d = std::tr1::ellint_3(kl, nul, phil);
  long double e = std::tr1::ellint_3l(kl, nul, phil);

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
  float kf = 0.5F;
  double kd = 0.5;
  long double kl = 0.5L;

  float nuf = std::numeric_limits<float>::quiet_NaN();
  double nud = std::numeric_limits<double>::quiet_NaN();
  long double nul = std::numeric_limits<long double>::quiet_NaN();

  float phif = std::atan2(1.0F, 1.0F);
  double phid = std::atan2(1.0, 1.0);
  long double phil = std::atan2(1.0L, 1.0L);

  float a = std::tr1::ellint_3(kf, nuf, phif);
  float b = std::tr1::ellint_3f(kf, nuf, phif);
  double c = std::tr1::ellint_3(kd, nud, phid);
  long double d = std::tr1::ellint_3(kl, nul, phil);
  long double e = std::tr1::ellint_3l(kl, nul, phil);

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
  float kf = 0.5F;
  double kd = 0.5;
  long double kl = 0.5L;

  float nuf = 0.2F;
  double nud = 0.2;
  long double nul = 0.2L;

  float phif = std::numeric_limits<float>::quiet_NaN();
  double phid = std::numeric_limits<double>::quiet_NaN();
  long double phil = std::numeric_limits<long double>::quiet_NaN();

  float a = std::tr1::ellint_3(kf, nuf, phif);
  float b = std::tr1::ellint_3f(kf, nuf, phif);
  double c = std::tr1::ellint_3(kd, nud, phid);
  long double d = std::tr1::ellint_3(kl, nul, phil);
  long double e = std::tr1::ellint_3l(kl, nul, phil);

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
  return 0;
}

