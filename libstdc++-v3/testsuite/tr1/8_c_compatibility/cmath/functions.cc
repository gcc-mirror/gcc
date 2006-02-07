// { dg-do compile }

// 2006-02-07  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

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

  ret = acosh(d0);
  fret = acoshf(f0);
  ldret = acoshl(ld0);

  ret = asinh(d0);
  fret = asinhf(f0);
  ldret = asinhl(ld0);

  ret = atanh(d0);
  fret = atanhf(f0);
  ldret = atanhl(ld0);

  ret = cbrt(d0);
  fret = cbrtf(f0);
  ldret = cbrtl(ld0);

  ret = copysign(d0, d0);
  fret = copysignf(f0, f0);
  ldret = copysignl(ld0, ld0);

  ret = erf(d0);
  fret = erff(f0);
  ldret = erfl(ld0);

  ret = erfc(d0);
  fret = erfcf(f0);
  ldret = erfcl(ld0);

  ret = exp2(d0);
  fret = exp2f(f0);
  ldret = exp2l(ld0);

  ret = expm1(d0);
  fret = expm1f(f0);
  ldret = expm1l(ld0);

  ret = fdim(d0, d0);
  fret = fdimf(f0, f0);
  ldret = fdiml(ld0, ld0);

  ret = fma(d0, d0, d0);
  fret = fmaf(f0, f0, f0);
  ldret = fmal(ld0, ld0, ld0);

  ret = fmax(d0, d0);
  fret = fmaxf(f0, f0);
  ldret = fmaxl(ld0, ld0);

  ret = fmin(d0, d0);
  fret = fminf(f0, f0);
  ldret = fminl(ld0, ld0);

  ret = hypot(d0, d0);
  fret = hypotf(f0, f0);
  ldret = hypotl(ld0, ld0);

  iret = ilogb(d0);
  iret = ilogbf(f0);
  iret = ilogbl(ld0);

  ret = lgamma(d0);
  fret = lgammaf(f0);
  ldret = lgammal(ld0);

  llret = llrint(d0);
  llret = llrintf(f0);
  llret = llrintl(ld0);

  llret = llround(d0);
  llret = llroundf(f0);
  llret = llroundl(ld0);

  ret = log1p(d0);
  fret = log1pf(f0);
  ldret = log1pl(ld0);

  ret = log2(d0);
  fret = log2f(f0);
  ldret = log2l(ld0);

  ret = logb(d0);
  fret = logbf(f0);
  ldret = logbl(ld0);

  lret = lrint(d0);
  lret = lrintf(f0);
  lret = lrintl(ld0);

  lret = lround(d0);
  lret = lroundf(f0);
  lret = lroundl(ld0);

  ret = nan(str);
  fret = nanf(str);
  ldret = nanl(str);

  ret = nearbyint(d0);
  fret = nearbyintf(f0);
  ldret = nearbyintl(ld0);

  ret = nextafter(d0, d0);
  fret = nextafterf(f0, f0);
  ldret = nextafterl(ld0, ld0);

  ret = nexttoward(d0, d0);
  fret = nexttowardf(f0, f0);
  ldret = nexttowardl(ld0, ld0);

  ret = remainder(d0, d0);
  fret = remainderf(f0, f0);
  ldret = remainderl(ld0, ld0);

  ret = remquo(d0, d0, pquo);
  fret = remquo(f0, f0, pquo);
  ldret = remquo(ld0, ld0, pquo);

  ret = rint(d0);
  fret = rintf(f0);
  ldret = rintl(ld0);

  ret = round(d0);
  fret = roundf(f0);
  ldret = roundl(ld0);

  ret = scalbln(d0, lex);
  fret = scalblnf(f0, lex);
  ldret = scalblnl(ld0, lex);

  ret = scalbn(d0, ex);
  fret = scalbnf(f0, ex);
  ldret = scalbnl(ld0, ex);

  ret = tgamma(d0);
  fret = tgammaf(f0);
  ldret = tgammal(ld0);

  ret = trunc(d0);
  fret = truncf(f0);
  ldret = truncl(ld0);
  
#endif
}
