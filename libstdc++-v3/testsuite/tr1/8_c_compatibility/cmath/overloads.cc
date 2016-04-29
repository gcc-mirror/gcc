// { dg-do compile }

// 2006-03-07  Paolo Carlini  <pcarlini@suse.de>
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

// 8.16.4 Additional overloads

#include <tr1/cmath>
#include <testsuite_tr1.h>

void test01()
{
#if _GLIBCXX_USE_C99_MATH_TR1

  using namespace __gnu_test;

  double d0 = 0.0;
  float f0 = 0.0f;
  long double ld0 = 0.0l;
  int i0 = 0;
  int* pquo = 0;
  long lex = 0l;
  int ex = 0;

  check_ret_type<double>(std::tr1::atan2(d0, d0));
  check_ret_type<double>(std::tr1::atan2(d0, f0));
  check_ret_type<float>(std::tr1::atan2(f0, f0));
  check_ret_type<long double>(std::tr1::atan2(ld0, ld0));
  check_ret_type<long double>(std::tr1::atan2(ld0, d0));
  check_ret_type<double>(std::tr1::atan2(i0, i0));
  check_ret_type<double>(std::tr1::atan2(d0, i0));

  check_ret_type<double>(std::tr1::acosh(d0));
  check_ret_type<float>(std::tr1::acosh(f0));
  check_ret_type<long double>(std::tr1::acosh(ld0));
  check_ret_type<double>(std::tr1::acosh(i0));

  check_ret_type<double>(std::tr1::asinh(d0));
  check_ret_type<float>(std::tr1::asinh(f0));
  check_ret_type<long double>(std::tr1::asinh(ld0));
  check_ret_type<double>(std::tr1::asinh(i0));

  check_ret_type<double>(std::tr1::atanh(d0));
  check_ret_type<float>(std::tr1::atanh(f0));
  check_ret_type<long double>(std::tr1::atanh(ld0));
  check_ret_type<double>(std::tr1::atanh(i0));

  check_ret_type<double>(std::tr1::cbrt(d0));
  check_ret_type<float>(std::tr1::cbrt(f0));
  check_ret_type<long double>(std::tr1::cbrt(ld0));
  check_ret_type<double>(std::tr1::cbrt(i0));

  check_ret_type<double>(std::tr1::copysign(d0, d0));
  check_ret_type<double>(std::tr1::copysign(d0, f0));
  check_ret_type<float>(std::tr1::copysign(f0, f0));
  check_ret_type<long double>(std::tr1::copysign(ld0, ld0));
  check_ret_type<long double>(std::tr1::copysign(ld0, d0));
  check_ret_type<double>(std::tr1::copysign(i0, i0));
  check_ret_type<double>(std::tr1::copysign(d0, i0));

  check_ret_type<double>(std::tr1::erf(d0));
  check_ret_type<float>(std::tr1::erf(f0));
  check_ret_type<long double>(std::tr1::erf(ld0));
  check_ret_type<double>(std::tr1::erf(i0));

  check_ret_type<double>(std::tr1::erfc(d0));
  check_ret_type<float>(std::tr1::erfc(f0));
  check_ret_type<long double>(std::tr1::erfc(ld0));
  check_ret_type<double>(std::tr1::erfc(i0));

  check_ret_type<double>(std::tr1::exp2(d0));
  check_ret_type<float>(std::tr1::exp2(f0));
  check_ret_type<long double>(std::tr1::exp2(ld0));
  check_ret_type<double>(std::tr1::exp2(i0));

  check_ret_type<double>(std::tr1::expm1(d0));
  check_ret_type<float>(std::tr1::expm1(f0));
  check_ret_type<long double>(std::tr1::expm1(ld0));
  check_ret_type<double>(std::tr1::expm1(i0));

  check_ret_type<double>(std::tr1::fdim(d0, d0));
  check_ret_type<double>(std::tr1::fdim(d0, f0));
  check_ret_type<float>(std::tr1::fdim(f0, f0));
  check_ret_type<long double>(std::tr1::fdim(ld0, ld0));
  check_ret_type<long double>(std::tr1::fdim(ld0, d0));
  check_ret_type<double>(std::tr1::fdim(i0, i0));
  check_ret_type<double>(std::tr1::fdim(d0, i0));

  check_ret_type<double>(std::tr1::fma(d0, d0, d0));
  check_ret_type<double>(std::tr1::fma(d0, f0, d0));
  check_ret_type<float>(std::tr1::fma(f0, f0, f0));
  check_ret_type<long double>(std::tr1::fma(ld0, ld0, ld0));
  check_ret_type<long double>(std::tr1::fma(ld0, d0, f0));
  check_ret_type<double>(std::tr1::fma(i0, i0, i0));
  check_ret_type<double>(std::tr1::fma(d0, i0, f0));

  check_ret_type<double>(std::tr1::fmax(d0, d0));
  check_ret_type<double>(std::tr1::fmax(d0, f0));
  check_ret_type<float>(std::tr1::fmax(f0, f0));
  check_ret_type<long double>(std::tr1::fmax(ld0, ld0));
  check_ret_type<long double>(std::tr1::fmax(ld0, d0));
  check_ret_type<double>(std::tr1::fmax(i0, i0));
  check_ret_type<double>(std::tr1::fmax(d0, i0));

  check_ret_type<double>(std::tr1::fmin(d0, d0));
  check_ret_type<double>(std::tr1::fmin(d0, f0));
  check_ret_type<float>(std::tr1::fmin(f0, f0));
  check_ret_type<long double>(std::tr1::fmin(ld0, ld0));
  check_ret_type<long double>(std::tr1::fmin(ld0, d0));
  check_ret_type<double>(std::tr1::fmin(i0, i0));
  check_ret_type<double>(std::tr1::fmin(d0, i0));

  check_ret_type<double>(std::tr1::hypot(d0, d0));
  check_ret_type<double>(std::tr1::hypot(d0, f0));
  check_ret_type<float>(std::tr1::hypot(f0, f0));
  check_ret_type<long double>(std::tr1::hypot(ld0, ld0));
  check_ret_type<long double>(std::tr1::hypot(ld0, d0));
  check_ret_type<double>(std::tr1::hypot(i0, i0));
  check_ret_type<double>(std::tr1::hypot(d0, i0));

  check_ret_type<int>(std::tr1::ilogb(d0));
  check_ret_type<int>(std::tr1::ilogb(f0));
  check_ret_type<int>(std::tr1::ilogb(ld0));
  check_ret_type<int>(std::tr1::ilogb(i0));

  check_ret_type<double>(std::tr1::lgamma(d0));
  check_ret_type<float>(std::tr1::lgamma(f0));
  check_ret_type<long double>(std::tr1::lgamma(ld0));
  check_ret_type<double>(std::tr1::lgamma(i0));

  check_ret_type<long long>(std::tr1::llrint(d0));
  check_ret_type<long long>(std::tr1::llrint(f0));
  check_ret_type<long long>(std::tr1::llrint(ld0));
  check_ret_type<long long>(std::tr1::llrint(i0));

  check_ret_type<long long>(std::tr1::llround(d0));
  check_ret_type<long long>(std::tr1::llround(f0));
  check_ret_type<long long>(std::tr1::llround(ld0));
  check_ret_type<long long>(std::tr1::llround(i0));

  check_ret_type<double>(std::tr1::log1p(d0));
  check_ret_type<float>(std::tr1::log1p(f0));
  check_ret_type<long double>(std::tr1::log1p(ld0));
  check_ret_type<double>(std::tr1::log1p(i0));

  // DR 568.
  check_ret_type<double>(std::tr1::log2(d0));
  check_ret_type<float>(std::tr1::log2(f0));
  check_ret_type<long double>(std::tr1::log2(ld0));
  check_ret_type<double>(std::tr1::log2(i0));

  check_ret_type<double>(std::tr1::logb(d0));
  check_ret_type<float>(std::tr1::logb(f0));
  check_ret_type<long double>(std::tr1::logb(ld0));
  check_ret_type<double>(std::tr1::logb(i0));

  check_ret_type<long>(std::tr1::lrint(d0));
  check_ret_type<long>(std::tr1::lrint(f0));
  check_ret_type<long>(std::tr1::lrint(ld0));
  check_ret_type<long>(std::tr1::lrint(i0));

  check_ret_type<long>(std::tr1::lround(d0));
  check_ret_type<long>(std::tr1::lround(f0));
  check_ret_type<long>(std::tr1::lround(ld0));
  check_ret_type<long>(std::tr1::lround(i0));

  check_ret_type<double>(std::tr1::nearbyint(d0));
  check_ret_type<float>(std::tr1::nearbyint(f0));
  check_ret_type<long double>(std::tr1::nearbyint(ld0));
  check_ret_type<double>(std::tr1::nearbyint(i0));

  check_ret_type<double>(std::tr1::nextafter(d0, d0));
  check_ret_type<double>(std::tr1::nextafter(d0, f0));
  check_ret_type<float>(std::tr1::nextafter(f0, f0));
  check_ret_type<long double>(std::tr1::nextafter(ld0, ld0));
  check_ret_type<long double>(std::tr1::nextafter(ld0, d0));
  check_ret_type<double>(std::tr1::nextafter(i0, i0));
  check_ret_type<double>(std::tr1::nextafter(d0, i0));

  check_ret_type<double>(std::tr1::nexttoward(d0, ld0));
  check_ret_type<float>(std::tr1::nexttoward(f0, ld0));
  check_ret_type<long double>(std::tr1::nexttoward(ld0, ld0));
  check_ret_type<double>(std::tr1::nexttoward(i0, ld0));

  check_ret_type<double>(std::tr1::pow(d0, d0));
  check_ret_type<double>(std::tr1::pow(d0, f0));
  check_ret_type<float>(std::tr1::pow(f0, f0));
  check_ret_type<long double>(std::tr1::pow(ld0, ld0));
  check_ret_type<long double>(std::tr1::pow(ld0, d0));
  check_ret_type<double>(std::tr1::pow(i0, i0));
  check_ret_type<double>(std::tr1::pow(d0, i0));
  check_ret_type<double>(std::tr1::pow(f0, i0));

  check_ret_type<double>(std::tr1::remainder(d0, d0));
  check_ret_type<double>(std::tr1::remainder(d0, f0));
  check_ret_type<float>(std::tr1::remainder(f0, f0));
  check_ret_type<long double>(std::tr1::remainder(ld0, ld0));
  check_ret_type<long double>(std::tr1::remainder(ld0, d0));
  check_ret_type<double>(std::tr1::remainder(i0, i0));
  check_ret_type<double>(std::tr1::remainder(d0, i0));

  check_ret_type<double>(std::tr1::remquo(d0, d0, pquo));
  check_ret_type<double>(std::tr1::remquo(d0, f0, pquo));
  check_ret_type<float>(std::tr1::remquo(f0, f0, pquo));
  check_ret_type<long double>(std::tr1::remquo(ld0, ld0, pquo));
  check_ret_type<long double>(std::tr1::remquo(ld0, d0, pquo));
  check_ret_type<double>(std::tr1::remquo(i0, i0, pquo));
  check_ret_type<double>(std::tr1::remquo(d0, i0, pquo));

  check_ret_type<double>(std::tr1::rint(d0));
  check_ret_type<float>(std::tr1::rint(f0));
  check_ret_type<long double>(std::tr1::rint(ld0));
  check_ret_type<double>(std::tr1::rint(i0));

  check_ret_type<double>(std::tr1::round(d0));
  check_ret_type<float>(std::tr1::round(f0));
  check_ret_type<long double>(std::tr1::round(ld0));
  check_ret_type<double>(std::tr1::round(i0));

  check_ret_type<double>(std::tr1::scalbln(d0, lex));
  check_ret_type<float>(std::tr1::scalbln(f0, lex));
  check_ret_type<long double>(std::tr1::scalbln(ld0, lex));
  check_ret_type<double>(std::tr1::scalbln(i0, lex));

  check_ret_type<double>(std::tr1::scalbn(d0, ex));
  check_ret_type<float>(std::tr1::scalbn(f0, ex));
  check_ret_type<long double>(std::tr1::scalbn(ld0, ex));
  check_ret_type<double>(std::tr1::scalbn(i0, ex));

  check_ret_type<double>(std::tr1::tgamma(d0));
  check_ret_type<float>(std::tr1::tgamma(f0));
  check_ret_type<long double>(std::tr1::tgamma(ld0));
  check_ret_type<double>(std::tr1::tgamma(i0));

  check_ret_type<double>(std::tr1::trunc(d0));
  check_ret_type<float>(std::tr1::trunc(f0));
  check_ret_type<long double>(std::tr1::trunc(ld0));
  check_ret_type<double>(std::tr1::trunc(i0));

#endif
}
