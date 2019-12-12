// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

#include <complex>
#include <testsuite_common_types.h>

namespace __gnu_test
{
  // Test constexpr symmetric complex @ real, real @ complex, complex @ complex.
  template<typename _Tp>
    void
    test_operators()
    {
      constexpr std::complex<_Tp> a{1, 2};
      constexpr std::complex<_Tp> b{3, 4};
      constexpr _Tp c = 5;

      constexpr auto w [[maybe_unused]] = +a;
      constexpr auto z [[maybe_unused]] = -a;

      constexpr auto apc [[maybe_unused]] = a + c;
      constexpr auto amc [[maybe_unused]] = a - c;
      constexpr auto atc [[maybe_unused]] = a * c;
      constexpr auto adc [[maybe_unused]] = a / c;

      constexpr auto cpa [[maybe_unused]] = c + a;
      constexpr auto cma [[maybe_unused]] = c - a;
      constexpr auto cta [[maybe_unused]] = c * a;
      constexpr auto cda [[maybe_unused]] = c / a;

      constexpr auto apb [[maybe_unused]] = a + b;
      constexpr auto amb [[maybe_unused]] = a - b;
      constexpr auto atb [[maybe_unused]] = a * b;
      constexpr auto adb [[maybe_unused]] = a / b;
    }
}

int main()
{
  __gnu_test::test_operators<float>();
  __gnu_test::test_operators<double>();
#ifndef __LONG_DOUBLE_IBM128__ // IBM128 format is not constexpr foldable
  __gnu_test::test_operators<long double>();
#endif

  return 0;
}
