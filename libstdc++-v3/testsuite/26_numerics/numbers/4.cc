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

// { dg-do compile { target c++23 } }

#include <numbers>
#include <stdfloat>

#if defined(__STDCPP_FLOAT16_T__)
void
test01()
{
  const std::float16_t* d1  = &std::numbers::e_v<std::float16_t>;
  const std::float16_t* d2  = &std::numbers::log2e_v<std::float16_t>;
  const std::float16_t* d3  = &std::numbers::log10e_v<std::float16_t>;
  const std::float16_t* d4  = &std::numbers::pi_v<std::float16_t>;
  const std::float16_t* d5  = &std::numbers::inv_pi_v<std::float16_t>;
  const std::float16_t* d6  = &std::numbers::inv_sqrtpi_v<std::float16_t>;
  const std::float16_t* d7  = &std::numbers::ln2_v<std::float16_t>;
  const std::float16_t* d8  = &std::numbers::ln10_v<std::float16_t>;
  const std::float16_t* d9  = &std::numbers::sqrt2_v<std::float16_t>;
  const std::float16_t* d10 = &std::numbers::sqrt3_v<std::float16_t>;
  const std::float16_t* d11 = &std::numbers::inv_sqrt3_v<std::float16_t>;
  const std::float16_t* d12 = &std::numbers::egamma_v<std::float16_t>;
  const std::float16_t* d13 = &std::numbers::phi_v<std::float16_t>;
}
#endif

#if defined(__STDCPP_FLOAT32_T__)
void
test02()
{
  const std::float32_t* d1  = &std::numbers::e_v<std::float32_t>;
  const std::float32_t* d2  = &std::numbers::log2e_v<std::float32_t>;
  const std::float32_t* d3  = &std::numbers::log10e_v<std::float32_t>;
  const std::float32_t* d4  = &std::numbers::pi_v<std::float32_t>;
  const std::float32_t* d5  = &std::numbers::inv_pi_v<std::float32_t>;
  const std::float32_t* d6  = &std::numbers::inv_sqrtpi_v<std::float32_t>;
  const std::float32_t* d7  = &std::numbers::ln2_v<std::float32_t>;
  const std::float32_t* d8  = &std::numbers::ln10_v<std::float32_t>;
  const std::float32_t* d9  = &std::numbers::sqrt2_v<std::float32_t>;
  const std::float32_t* d10 = &std::numbers::sqrt3_v<std::float32_t>;
  const std::float32_t* d11 = &std::numbers::inv_sqrt3_v<std::float32_t>;
  const std::float32_t* d12 = &std::numbers::egamma_v<std::float32_t>;
  const std::float32_t* d13 = &std::numbers::phi_v<std::float32_t>;
}
#endif

#if defined(__STDCPP_FLOAT64_T__)
void
test03()
{
  const std::float64_t* d1  = &std::numbers::e_v<std::float64_t>;
  const std::float64_t* d2  = &std::numbers::log2e_v<std::float64_t>;
  const std::float64_t* d3  = &std::numbers::log10e_v<std::float64_t>;
  const std::float64_t* d4  = &std::numbers::pi_v<std::float64_t>;
  const std::float64_t* d5  = &std::numbers::inv_pi_v<std::float64_t>;
  const std::float64_t* d6  = &std::numbers::inv_sqrtpi_v<std::float64_t>;
  const std::float64_t* d7  = &std::numbers::ln2_v<std::float64_t>;
  const std::float64_t* d8  = &std::numbers::ln10_v<std::float64_t>;
  const std::float64_t* d9  = &std::numbers::sqrt2_v<std::float64_t>;
  const std::float64_t* d10 = &std::numbers::sqrt3_v<std::float64_t>;
  const std::float64_t* d11 = &std::numbers::inv_sqrt3_v<std::float64_t>;
  const std::float64_t* d12 = &std::numbers::egamma_v<std::float64_t>;
  const std::float64_t* d13 = &std::numbers::phi_v<std::float64_t>;
}
#endif

#if defined(__STDCPP_FLOAT128_T__)
void
test04()
{
  const std::float128_t* d1  = &std::numbers::e_v<std::float128_t>;
  const std::float128_t* d2  = &std::numbers::log2e_v<std::float128_t>;
  const std::float128_t* d3  = &std::numbers::log10e_v<std::float128_t>;
  const std::float128_t* d4  = &std::numbers::pi_v<std::float128_t>;
  const std::float128_t* d5  = &std::numbers::inv_pi_v<std::float128_t>;
  const std::float128_t* d6  = &std::numbers::inv_sqrtpi_v<std::float128_t>;
  const std::float128_t* d7  = &std::numbers::ln2_v<std::float128_t>;
  const std::float128_t* d8  = &std::numbers::ln10_v<std::float128_t>;
  const std::float128_t* d9  = &std::numbers::sqrt2_v<std::float128_t>;
  const std::float128_t* d10 = &std::numbers::sqrt3_v<std::float128_t>;
  const std::float128_t* d11 = &std::numbers::inv_sqrt3_v<std::float128_t>;
  const std::float128_t* d12 = &std::numbers::egamma_v<std::float128_t>;
  const std::float128_t* d13 = &std::numbers::phi_v<std::float128_t>;
}
#endif

#if defined(__STDCPP_BFLOAT16_T__)
void
test05()
{
  const std::bfloat16_t* d1  = &std::numbers::e_v<std::bfloat16_t>;
  const std::bfloat16_t* d2  = &std::numbers::log2e_v<std::bfloat16_t>;
  const std::bfloat16_t* d3  = &std::numbers::log10e_v<std::bfloat16_t>;
  const std::bfloat16_t* d4  = &std::numbers::pi_v<std::bfloat16_t>;
  const std::bfloat16_t* d5  = &std::numbers::inv_pi_v<std::bfloat16_t>;
  const std::bfloat16_t* d6  = &std::numbers::inv_sqrtpi_v<std::bfloat16_t>;
  const std::bfloat16_t* d7  = &std::numbers::ln2_v<std::bfloat16_t>;
  const std::bfloat16_t* d8  = &std::numbers::ln10_v<std::bfloat16_t>;
  const std::bfloat16_t* d9  = &std::numbers::sqrt2_v<std::bfloat16_t>;
  const std::bfloat16_t* d10 = &std::numbers::sqrt3_v<std::bfloat16_t>;
  const std::bfloat16_t* d11 = &std::numbers::inv_sqrt3_v<std::bfloat16_t>;
  const std::bfloat16_t* d12 = &std::numbers::egamma_v<std::bfloat16_t>;
  const std::bfloat16_t* d13 = &std::numbers::phi_v<std::bfloat16_t>;
}
#endif
