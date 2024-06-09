// Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++23 } }

#include <stdfloat>
#include <cmath>
#include <limits>
#include <testsuite_hooks.h>

template <typename T>
void
test ()
{
  using lim = std::numeric_limits<T>;
  T t0 = std::nextafter(T(-0.0), T(2.0));
  VERIFY( t0 == lim::denorm_min() );
  T t1 = std::nextafter(T(), T(1.0));
  VERIFY( t1 == lim::denorm_min() );
  T t2 = std::nextafter(T(), T());
  VERIFY( t2 == T() && !std::signbit(t2) );
  T t3 = std::nextafter(lim::denorm_min(), T(-2.0));
  VERIFY( t3 == T() && !std::signbit(t3) );
  T t4 = std::nextafter(lim::min(), T(-0.0));
  VERIFY( std::fpclassify(t4) == FP_SUBNORMAL && t4 > T() );
  T t5 = std::nextafter(t4, T(1.0));
  VERIFY( t5 == lim::min() );
  T t6 = std::nextafter(lim::min(), lim::infinity());
  VERIFY( std::fpclassify(t6) == FP_NORMAL && t6 > lim::min() );
  T t7 = std::nextafter(t6, -lim::infinity());
  VERIFY( t7 == lim::min() );
  T t8 = std::nextafter(T(16.0), T(16.5));
  VERIFY( t8 > t7 );
  T t9 = std::nextafter(t8, T(15.5));
  VERIFY( t9 == T(16.0) );
  T t10 = std::nextafter(lim::max(), T(-0.5));
  VERIFY( std::fpclassify(t10) == FP_NORMAL && t10 < lim::max() );
  T t11 = std::nextafter(t10, lim::infinity());
  VERIFY( t11 == lim::max() );
  T t12 = std::nextafter(t11, lim::infinity());
  VERIFY( std::fpclassify(t12) == FP_INFINITE && !std::signbit(t12) );
  T t13 = std::nextafter(lim::infinity(), t12);
  VERIFY( t13 == t12 );
  T t14 = std::nextafter(t13, T(1.0));
  VERIFY( t14 == lim::max() );
  T t15 = std::nextafter(lim::quiet_NaN(), T());
  VERIFY( std::fpclassify(t15) == FP_NAN );
  T t16 = std::nextafter(T(17.0), lim::quiet_NaN());
  VERIFY( std::fpclassify(t16) == FP_NAN );
  T t17 = std::nextafter(T(), T(-0.0));
  VERIFY( t17 == T() && std::signbit(t17) );
  T t20 = std::nextafter(T(-0.0), T(-2.0));
  VERIFY( t20 == -lim::denorm_min() );
  T t21 = std::nextafter(T(), T(-1.0));
  VERIFY( t21 == -lim::denorm_min() );
  T t22 = std::nextafter(T(-0.0), T(-0.0));
  VERIFY( t22 == T() && std::signbit(t22) );
  T t23 = std::nextafter(-lim::denorm_min(), T(2.0));
  VERIFY( t23 == T() && std::signbit(t23) );
  T t24 = std::nextafter(-lim::min(), T());
  VERIFY( std::fpclassify(t24) == FP_SUBNORMAL && t24 < T() );
  T t25 = std::nextafter(t24, T(-1.0));
  VERIFY( t25 == -lim::min() );
  T t26 = std::nextafter(-lim::min(), -lim::infinity());
  VERIFY( std::fpclassify(t26) == FP_NORMAL && t26 < -lim::min() );
  T t27 = std::nextafter(t26, lim::infinity());
  VERIFY( t27 == -lim::min() );
  T t28 = std::nextafter(T(-16.0), T(-16.5));
  VERIFY( t28 < t27 );
  T t29 = std::nextafter(t28, T(-15.5));
  VERIFY( t29 == T(-16.0) );
  T t30 = std::nextafter(-lim::max(), T(0.5));
  VERIFY( std::fpclassify(t30) == FP_NORMAL && t30 > -lim::max() );
  T t31 = std::nextafter(t30, -lim::infinity());
  VERIFY( t31 == -lim::max() );
  T t32 = std::nextafter(t31, -lim::infinity());
  VERIFY( std::fpclassify(t32) == FP_INFINITE && std::signbit(t32) );
  T t33 = std::nextafter(-lim::infinity(), t32);
  VERIFY( t33 == t32 );
  T t34 = std::nextafter(t33, T(-1.0));
  VERIFY( t34 == -lim::max() );
  T t35 = std::nextafter(-lim::quiet_NaN(), T());
  VERIFY( std::fpclassify(t35) == FP_NAN );
  T t36 = std::nextafter(T(-17.0), lim::quiet_NaN());
  VERIFY( std::fpclassify(t36) == FP_NAN );
  T t37 = std::nextafter(T(-0.0), T());
  VERIFY( t37 == T() && !std::signbit(t37) );
  static_assert(std::nextafter(T(1.0), T(2.0)) > T(1.0));
  static_assert(std::nextafter(std::nextafter(T(1.0), T(5.0)), T(0.0)) == T(1.0));
}

int
main ()
{
#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::float16_t>();
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::float32_t>();
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
  test <std::float64_t>();
#endif
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  test <std::float128_t>();
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::bfloat16_t>();
#endif
}
