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

#if defined(_GLIBCXX_USE_FLOAT128)
void
test01()
{
  const __float128* d1  = &std::numbers::e_v<__float128>;
  const __float128* d2  = &std::numbers::log2e_v<__float128>;
  const __float128* d3  = &std::numbers::log10e_v<__float128>;
  const __float128* d4  = &std::numbers::pi_v<__float128>;
  const __float128* d5  = &std::numbers::inv_pi_v<__float128>;
  const __float128* d6  = &std::numbers::inv_sqrtpi_v<__float128>;
  const __float128* d7  = &std::numbers::ln2_v<__float128>;
  const __float128* d8  = &std::numbers::ln10_v<__float128>;
  const __float128* d9  = &std::numbers::sqrt2_v<__float128>;
  const __float128* d10 = &std::numbers::sqrt3_v<__float128>;
  const __float128* d11 = &std::numbers::inv_sqrt3_v<__float128>;
  const __float128* d12 = &std::numbers::egamma_v<__float128>;
  const __float128* d13 = &std::numbers::phi_v<__float128>;
}
#endif
