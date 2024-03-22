// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do link { target c++23 } }

#include <stdfloat>
#include <cmath>

template <typename T>
void
test_functions()
{
  constexpr T zero = 0;
  constexpr T one = 1;
  constexpr auto a = std::acos(one);
  constexpr auto b = std::asin(zero);
  constexpr auto c = std::atan(zero);
  constexpr auto d = std::cos(zero);
  constexpr auto e = std::sin(zero);
  constexpr auto f = std::tan(zero);
  constexpr auto g = std::acosh(one);
  constexpr auto h = std::asinh(zero);
  constexpr auto i = std::atanh(zero);
  constexpr auto j = std::cosh(zero);
  constexpr auto k = std::sinh(zero);
  constexpr auto l = std::tanh(zero);
  constexpr auto m = std::exp(zero);
  constexpr auto n = std::exp2(zero);
  constexpr auto o = std::expm1(one);
  constexpr auto p = std::log(one);
  constexpr auto q = std::log10(one);
  constexpr auto r = std::log1p(zero);
  constexpr auto s = std::log2(one);
  constexpr auto t = std::logb(one);
  constexpr auto u = std::cbrt(zero);
  constexpr auto v = std::fabs(zero);
  constexpr auto w = std::sqrt(one);
  constexpr auto x = std::erf(zero);
  constexpr auto y = std::erfc(zero);
//  constexpr auto z = std::lgamma(one);
  constexpr auto A = std::tgamma(one);
  constexpr auto B = std::ceil(zero);
  constexpr auto C = std::floor(zero);
//  constexpr auto D = std::nearbyint(zero);
//  constexpr auto E = std::rint(zero);
  constexpr auto F = std::round(zero);
  constexpr auto G = std::trunc(zero);
  constexpr auto H = std::atan2(zero, one);
  constexpr auto I = std::hypot(one, zero);
  constexpr auto J = std::pow(one, zero);
  constexpr auto K = std::fmod(zero, one);
  constexpr auto L = std::remainder(one, one);
  constexpr auto M = std::copysign(zero, zero);
  constexpr auto N = std::nextafter(zero, zero);
  constexpr auto O = std::fdim(zero, zero);
  constexpr auto P = std::fmax(zero, one);
  constexpr auto Q = std::fmin(zero, one);
  constexpr auto R = std::ilogb(one);
  constexpr auto S = std::ldexp(one, 0);
  constexpr auto U = std::scalbn(one, 1);
  constexpr auto V = std::scalbln(one, 1);
//  constexpr auto W = std::lrint(one);
//  constexpr auto X = std::llrint(one);
  constexpr auto Y = std::lround(one);
  constexpr auto Z = std::llround(one);
  constexpr auto a1 = std::fma(one, one, one);
  constexpr auto b1 = std::atan2(zero, 1);
  constexpr auto c1 = std::hypot(one, 0);
  constexpr auto d1 = std::pow(one, 0);
  constexpr auto e1 = std::fmod(zero, 1);
  constexpr auto f1 = std::remainder(one, 1);
  constexpr auto g1 = std::copysign(zero, 0);
  constexpr auto h1 = std::nextafter(zero, 0);
  constexpr auto i1 = std::fdim(zero, 0);
  constexpr auto j1 = std::fmax(zero, 1);
  constexpr auto k1 = std::fmin(zero, 1);
  constexpr auto l1 = std::fma(one, one, 1);
  constexpr auto n1 = std::atan2(0, one);
  constexpr auto o1 = std::hypot(1, zero);
  constexpr auto p1 = std::pow(1, zero);
  constexpr auto q1 = std::fmod(0, one);
  constexpr auto r1 = std::remainder(1, one);
  constexpr auto s1 = std::copysign(0, zero);
  constexpr auto t1 = std::nextafter(0, zero);
  constexpr auto u1 = std::fdim(0, zero);
  constexpr auto v1 = std::fmax(0, one);
  constexpr auto w1 = std::fmin(0, one);
  constexpr auto x1 = std::fma(one, 1, one);
  constexpr auto y1 = std::fma(1, one, one);
  constexpr auto z1 = std::fma(1, 1, one);
  constexpr auto A1 = std::fma(1, one, 1);
}

int
main()
{
#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test_functions<std::float16_t>();
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test_functions<std::float32_t>();
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
  test_functions<std::float64_t>();
#endif
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  test_functions<std::float128_t>();
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test_functions<std::bfloat16_t>();
#endif
}
