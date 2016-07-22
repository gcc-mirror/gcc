// { dg-do compile }

// 2006-02-07  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

// 8.16 Additions to header <cmath>

#include <tr1/cmath>

void test01()
{
#if _GLIBCXX_USE_C99_MATH_TR1

  double d0 = 0.0;
  float f0 = 0.0f;
  long double ld0 = 0.0l;
  const char* str = "NAN";
  int* pquo = 0;
  long lex = 0l;
  int ex = 0;

  double ret;
  float fret;
  long double ldret;
  int iret;
  long lret;
  long long llret;

  ret = std::tr1::acosh(d0);
  fret = std::tr1::acoshf(f0);
  ldret = std::tr1::acoshl(ld0);

  ret = std::tr1::asinh(d0);
  fret = std::tr1::asinhf(f0);
  ldret = std::tr1::asinhl(ld0);

  ret = std::tr1::atanh(d0);
  fret = std::tr1::atanhf(f0);
  ldret = std::tr1::atanhl(ld0);

  ret = std::tr1::cbrt(d0);
  fret = std::tr1::cbrtf(f0);
  ldret = std::tr1::cbrtl(ld0);

  ret = std::tr1::copysign(d0, d0);
  fret = std::tr1::copysignf(f0, f0);
  ldret = std::tr1::copysignl(ld0, ld0);

  ret = std::tr1::erf(d0);
  fret = std::tr1::erff(f0);
  ldret = std::tr1::erfl(ld0);

  ret = std::tr1::erfc(d0);
  fret = std::tr1::erfcf(f0);
  ldret = std::tr1::erfcl(ld0);

  ret = std::tr1::exp2(d0);
  fret = std::tr1::exp2f(f0);
  ldret = std::tr1::exp2l(ld0);

  ret = std::tr1::expm1(d0);
  fret = std::tr1::expm1f(f0);
  ldret = std::tr1::expm1l(ld0);

  ret = std::tr1::fdim(d0, d0);
  fret = std::tr1::fdimf(f0, f0);
  ldret = std::tr1::fdiml(ld0, ld0);

  ret = std::tr1::fma(d0, d0, d0);
  fret = std::tr1::fmaf(f0, f0, f0);
  ldret = std::tr1::fmal(ld0, ld0, ld0);

  ret = std::tr1::fmax(d0, d0);
  fret = std::tr1::fmaxf(f0, f0);
  ldret = std::tr1::fmaxl(ld0, ld0);

  ret = std::tr1::fmin(d0, d0);
  fret = std::tr1::fminf(f0, f0);
  ldret = std::tr1::fminl(ld0, ld0);

  ret = std::tr1::hypot(d0, d0);
  fret = std::tr1::hypotf(f0, f0);
  ldret = std::tr1::hypotl(ld0, ld0);

  iret = std::tr1::ilogb(d0);
  iret = std::tr1::ilogbf(f0);
  iret = std::tr1::ilogbl(ld0);

  ret = std::tr1::lgamma(d0);
  fret = std::tr1::lgammaf(f0);
  ldret = std::tr1::lgammal(ld0);

  llret = std::tr1::llrint(d0);
  llret = std::tr1::llrintf(f0);
  llret = std::tr1::llrintl(ld0);

  llret = std::tr1::llround(d0);
  llret = std::tr1::llroundf(f0);
  llret = std::tr1::llroundl(ld0);

  ret = std::tr1::log1p(d0);
  fret = std::tr1::log1pf(f0);
  ldret = std::tr1::log1pl(ld0);

  ret = std::tr1::log2(d0);
  fret = std::tr1::log2f(f0);
  ldret = std::tr1::log2l(ld0);

  ret = std::tr1::logb(d0);
  fret = std::tr1::logbf(f0);
  ldret = std::tr1::logbl(ld0);

  lret = std::tr1::lrint(d0);
  lret = std::tr1::lrintf(f0);
  lret = std::tr1::lrintl(ld0);

  lret = std::tr1::lround(d0);
  lret = std::tr1::lroundf(f0);
  lret = std::tr1::lroundl(ld0);

  ret = std::tr1::nan(str);
  fret = std::tr1::nanf(str);
  ldret = std::tr1::nanl(str);

  ret = std::tr1::nearbyint(d0);
  fret = std::tr1::nearbyintf(f0);
  ldret = std::tr1::nearbyintl(ld0);

  ret = std::tr1::nextafter(d0, d0);
  fret = std::tr1::nextafterf(f0, f0);
  ldret = std::tr1::nextafterl(ld0, ld0);

  ret = std::tr1::nexttoward(d0, ld0);
  fret = std::tr1::nexttowardf(f0, ld0);
  ldret = std::tr1::nexttowardl(ld0, ld0);

  ret = std::tr1::remainder(d0, d0);
  fret = std::tr1::remainderf(f0, f0);
  ldret = std::tr1::remainderl(ld0, ld0);

  ret = std::tr1::remquo(d0, d0, pquo);
  fret = std::tr1::remquof(f0, f0, pquo);
  ldret = std::tr1::remquol(ld0, ld0, pquo);

  ret = std::tr1::rint(d0);
  fret = std::tr1::rintf(f0);
  ldret = std::tr1::rintl(ld0);

  ret = std::tr1::round(d0);
  fret = std::tr1::roundf(f0);
  ldret = std::tr1::roundl(ld0);

  ret = std::tr1::scalbln(d0, lex);
  fret = std::tr1::scalblnf(f0, lex);
  ldret = std::tr1::scalblnl(ld0, lex);

  ret = std::tr1::scalbn(d0, ex);
  fret = std::tr1::scalbnf(f0, ex);
  ldret = std::tr1::scalbnl(ld0, ex);

  ret = std::tr1::tgamma(d0);
  fret = std::tr1::tgammaf(f0);
  ldret = std::tr1::tgammal(ld0);

  ret = std::tr1::trunc(d0);
  fret = std::tr1::truncf(f0);
  ldret = std::tr1::truncl(ld0);

  ret = ret; // Suppress unused warnings.
  iret = iret;
  lret = lret;
  llret = llret;
  fret = fret;
  ldret = ldret;

#endif
}
