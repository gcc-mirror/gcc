// { dg-do compile }

// 2006-03-07  Paolo Carlini  <pcarlini@suse.de>
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

// 8.16.4 Additional overloads

#include <tr1/cmath>
#include <testsuite_tr1.h>

void test01()
{
#if _GLIBCXX_USE_C99_MATH_TR1

  using namespace std::tr1;
  using namespace __gnu_test;

  double d0 = 0.0;
  float f0 = 0.0f;
  long double ld0 = 0.0l;
  int i0 = 0;  
  int* pquo = 0;
  long lex = 0l;
  int ex = 0;

  check_ret_type<double>(acosh(d0));
  check_ret_type<float>(acosh(f0));
  check_ret_type<long double>(acosh(ld0));
  check_ret_type<double>(acosh(i0));

  check_ret_type<double>(asinh(d0));
  check_ret_type<float>(asinh(f0));
  check_ret_type<long double>(asinh(ld0));
  check_ret_type<double>(asinh(i0));

  check_ret_type<double>(atanh(d0));
  check_ret_type<float>(atanh(f0));
  check_ret_type<long double>(atanh(ld0));
  check_ret_type<double>(atanh(i0));

  check_ret_type<double>(cbrt(d0));
  check_ret_type<float>(cbrt(f0));
  check_ret_type<long double>(cbrt(ld0));
  check_ret_type<double>(cbrt(i0));

  check_ret_type<double>(copysign(d0, d0));
  check_ret_type<double>(copysign(d0, f0));
  check_ret_type<float>(copysign(f0, f0));
  check_ret_type<long double>(copysign(ld0, ld0));
  check_ret_type<long double>(copysign(ld0, d0));
  check_ret_type<double>(copysign(i0, i0));  
  check_ret_type<double>(copysign(d0, i0));

  check_ret_type<double>(erf(d0));
  check_ret_type<float>(erf(f0));
  check_ret_type<long double>(erf(ld0));
  check_ret_type<double>(erf(i0));

  check_ret_type<double>(erfc(d0));
  check_ret_type<float>(erfc(f0));
  check_ret_type<long double>(erfc(ld0));
  check_ret_type<double>(erfc(i0));

  check_ret_type<double>(exp2(d0));
  check_ret_type<float>(exp2(f0));
  check_ret_type<long double>(exp2(ld0));
  check_ret_type<double>(exp2(i0));

  check_ret_type<double>(expm1(d0));
  check_ret_type<float>(expm1(f0));
  check_ret_type<long double>(expm1(ld0));
  check_ret_type<double>(expm1(i0));

  check_ret_type<double>(fdim(d0, d0));
  check_ret_type<double>(fdim(d0, f0));
  check_ret_type<float>(fdim(f0, f0));
  check_ret_type<long double>(fdim(ld0, ld0));
  check_ret_type<long double>(fdim(ld0, d0));
  check_ret_type<double>(fdim(i0, i0));
  check_ret_type<double>(fdim(d0, i0));

  check_ret_type<double>(fma(d0, d0, d0));
  check_ret_type<double>(fma(d0, f0, d0));
  check_ret_type<float>(fma(f0, f0, f0));
  check_ret_type<long double>(fma(ld0, ld0, ld0));
  check_ret_type<long double>(fma(ld0, d0, f0));
  check_ret_type<double>(fma(i0, i0, i0));
  check_ret_type<double>(fma(d0, i0, f0));

  check_ret_type<double>(fmax(d0, d0));
  check_ret_type<double>(fmax(d0, f0));
  check_ret_type<float>(fmax(f0, f0));
  check_ret_type<long double>(fmax(ld0, ld0));
  check_ret_type<long double>(fmax(ld0, d0));
  check_ret_type<double>(fmax(i0, i0));
  check_ret_type<double>(fmax(d0, i0));

  check_ret_type<double>(fmin(d0, d0));
  check_ret_type<double>(fmin(d0, f0));
  check_ret_type<float>(fmin(f0, f0));
  check_ret_type<long double>(fmin(ld0, ld0));
  check_ret_type<long double>(fmin(ld0, d0));
  check_ret_type<double>(fmin(i0, i0));
  check_ret_type<double>(fmin(d0, i0));

  check_ret_type<double>(hypot(d0, d0));
  check_ret_type<double>(hypot(d0, f0));
  check_ret_type<float>(hypot(f0, f0));
  check_ret_type<long double>(hypot(ld0, ld0));
  check_ret_type<long double>(hypot(ld0, d0));
  check_ret_type<double>(hypot(i0, i0));
  check_ret_type<double>(hypot(d0, i0));

  check_ret_type<int>(ilogb(d0));
  check_ret_type<int>(ilogb(f0));
  check_ret_type<int>(ilogb(ld0));
  check_ret_type<int>(ilogb(i0));

  check_ret_type<double>(lgamma(d0));
  check_ret_type<float>(lgamma(f0));
  check_ret_type<long double>(lgamma(ld0));
  check_ret_type<double>(lgamma(i0));

  check_ret_type<long long>(llrint(d0));
  check_ret_type<long long>(llrint(f0));
  check_ret_type<long long>(llrint(ld0));
  check_ret_type<long long>(llrint(i0));

  check_ret_type<long long>(llround(d0));
  check_ret_type<long long>(llround(f0));
  check_ret_type<long long>(llround(ld0));
  check_ret_type<long long>(llround(i0));

  check_ret_type<double>(log1p(d0));
  check_ret_type<float>(log1p(f0));
  check_ret_type<long double>(log1p(ld0));
  check_ret_type<double>(log1p(i0));

  check_ret_type<double>(log2(d0));
  check_ret_type<float>(log2(f0));
  check_ret_type<long double>(log2(ld0));
  check_ret_type<double>(log2(i0));

  check_ret_type<double>(logb(d0));
  check_ret_type<float>(logb(f0));
  check_ret_type<long double>(logb(ld0));
  check_ret_type<double>(logb(i0));

  check_ret_type<long>(lrint(d0));
  check_ret_type<long>(lrint(f0));
  check_ret_type<long>(lrint(ld0));
  check_ret_type<long>(lrint(i0));

  check_ret_type<long>(lround(d0));
  check_ret_type<long>(lround(f0));
  check_ret_type<long>(lround(ld0));
  check_ret_type<long>(lround(i0));

  check_ret_type<double>(nearbyint(d0));
  check_ret_type<float>(nearbyint(f0));
  check_ret_type<long double>(nearbyint(ld0));
  check_ret_type<double>(nearbyint(i0));

  check_ret_type<double>(nextafter(d0, d0));
  check_ret_type<double>(nextafter(d0, f0));
  check_ret_type<float>(nextafter(f0, f0));
  check_ret_type<long double>(nextafter(ld0, ld0));
  check_ret_type<long double>(nextafter(ld0, d0));
  check_ret_type<double>(nextafter(i0, i0));
  check_ret_type<double>(nextafter(d0, i0));

  check_ret_type<double>(nexttoward(d0, ld0));
  check_ret_type<float>(nexttoward(f0, ld0));
  check_ret_type<long double>(nexttoward(ld0, ld0));
  check_ret_type<double>(nexttoward(i0, ld0));

  check_ret_type<double>(remainder(d0, d0));
  check_ret_type<double>(remainder(d0, f0));
  check_ret_type<float>(remainder(f0, f0));
  check_ret_type<long double>(remainder(ld0, ld0));
  check_ret_type<long double>(remainder(ld0, d0));
  check_ret_type<double>(remainder(i0, i0));
  check_ret_type<double>(remainder(d0, i0));

  check_ret_type<double>(remquo(d0, d0, pquo));
  check_ret_type<double>(remquo(d0, f0, pquo));
  check_ret_type<float>(remquo(f0, f0, pquo));
  check_ret_type<long double>(remquo(ld0, ld0, pquo));
  check_ret_type<long double>(remquo(ld0, d0, pquo));
  check_ret_type<double>(remquo(i0, i0, pquo));
  check_ret_type<double>(remquo(d0, i0, pquo));

  check_ret_type<double>(rint(d0));
  check_ret_type<float>(rint(f0));
  check_ret_type<long double>(rint(ld0));
  check_ret_type<double>(rint(i0));

  check_ret_type<double>(round(d0));
  check_ret_type<float>(round(f0));
  check_ret_type<long double>(round(ld0));
  check_ret_type<double>(round(i0));

  check_ret_type<double>(scalbln(d0, lex));
  check_ret_type<float>(scalbln(f0, lex));
  check_ret_type<long double>(scalbln(ld0, lex));
  check_ret_type<double>(scalbln(i0, lex));

  check_ret_type<double>(scalbn(d0, ex));
  check_ret_type<float>(scalbn(f0, ex));
  check_ret_type<long double>(scalbn(ld0, ex));
  check_ret_type<double>(scalbn(i0, ex));

  check_ret_type<double>(tgamma(d0));
  check_ret_type<float>(tgamma(f0));
  check_ret_type<long double>(tgamma(ld0));
  check_ret_type<double>(tgamma(i0));

  check_ret_type<double>(trunc(d0));
  check_ret_type<float>(trunc(f0));
  check_ret_type<long double>(trunc(ld0));
  check_ret_type<double>(trunc(i0));
  
#endif
}
