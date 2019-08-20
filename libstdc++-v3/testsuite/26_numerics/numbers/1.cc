// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <numbers>

#ifndef __cpp_lib_math_constants
# error "Feature-test macro for math constants missing in <numbers>"
#elif __cpp_lib_math_constants != 201907L
# error "Feature-test macro for math constants has wrong value in <numbers>"
#endif

void
test01()
{
  const double* d1  = &std::numbers::e_v<double>;
  const double* d2  = &std::numbers::log2e_v<double>;
  const double* d3  = &std::numbers::log10e_v<double>;
  const double* d4  = &std::numbers::pi_v<double>;
  const double* d5  = &std::numbers::inv_pi_v<double>;
  const double* d6  = &std::numbers::inv_sqrtpi_v<double>;
  const double* d7  = &std::numbers::ln2_v<double>;
  const double* d8  = &std::numbers::ln10_v<double>;
  const double* d9  = &std::numbers::sqrt2_v<double>;
  const double* d10 = &std::numbers::sqrt3_v<double>;
  const double* d11 = &std::numbers::inv_sqrt3_v<double>;
  const double* d12 = &std::numbers::egamma_v<double>;
  const double* d13 = &std::numbers::phi_v<double>;
}

void
test02()
{
  const float* d1  = &std::numbers::e_v<float>;
  const float* d2  = &std::numbers::log2e_v<float>;
  const float* d3  = &std::numbers::log10e_v<float>;
  const float* d4  = &std::numbers::pi_v<float>;
  const float* d5  = &std::numbers::inv_pi_v<float>;
  const float* d6  = &std::numbers::inv_sqrtpi_v<float>;
  const float* d7  = &std::numbers::ln2_v<float>;
  const float* d8  = &std::numbers::ln10_v<float>;
  const float* d9  = &std::numbers::sqrt2_v<float>;
  const float* d10 = &std::numbers::sqrt3_v<float>;
  const float* d11 = &std::numbers::inv_sqrt3_v<float>;
  const float* d12 = &std::numbers::egamma_v<float>;
  const float* d13 = &std::numbers::phi_v<float>;
}

void
test03()
{
  const long double* d1  = &std::numbers::e_v<long double>;
  const long double* d2  = &std::numbers::log2e_v<long double>;
  const long double* d3  = &std::numbers::log10e_v<long double>;
  const long double* d4  = &std::numbers::pi_v<long double>;
  const long double* d5  = &std::numbers::inv_pi_v<long double>;
  const long double* d6  = &std::numbers::inv_sqrtpi_v<long double>;
  const long double* d7  = &std::numbers::ln2_v<long double>;
  const long double* d8  = &std::numbers::ln10_v<long double>;
  const long double* d9  = &std::numbers::sqrt2_v<long double>;
  const long double* d10 = &std::numbers::sqrt3_v<long double>;
  const long double* d11 = &std::numbers::inv_sqrt3_v<long double>;
  const long double* d12 = &std::numbers::egamma_v<long double>;
  const long double* d13 = &std::numbers::phi_v<long double>;
}

void
test04()
{
  static_assert(std::numbers::e == std::numbers::e_v<double>);
  static_assert(std::numbers::log2e == std::numbers::log2e_v<double>);
  static_assert(std::numbers::log10e == std::numbers::log10e_v<double>);
  static_assert(std::numbers::pi == std::numbers::pi_v<double>);
  static_assert(std::numbers::inv_pi == std::numbers::inv_pi_v<double>);
  static_assert(std::numbers::inv_sqrtpi == std::numbers::inv_sqrtpi_v<double>);
  static_assert(std::numbers::ln2 == std::numbers::ln2_v<double>);
  static_assert(std::numbers::ln10 == std::numbers::ln10_v<double>);
  static_assert(std::numbers::sqrt2 == std::numbers::sqrt2_v<double>);
  static_assert(std::numbers::sqrt3 == std::numbers::sqrt3_v<double>);
  static_assert(std::numbers::inv_sqrt3 == std::numbers::inv_sqrt3_v<double>);
  static_assert(std::numbers::egamma == std::numbers::egamma_v<double>);
  static_assert(std::numbers::phi == std::numbers::phi_v<double>);
}
