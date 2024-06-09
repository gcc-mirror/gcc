// { dg-do compile { target c++20 } }

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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
  // Test constexpr real(val) imag(val).
  template<typename _Tp, const int _Val = 42>
    inline void
    set_real(std::complex<_Tp>& a)
    { a.real(_Val); }

  template<typename _Tp, const int _Val = 42>
    inline void
    set_imag(std::complex<_Tp>& a)
    { a.imag(_Val); }

  template<typename _Tp>
    void
    test_members()
    {
      constexpr std::complex<_Tp> a{1.1, 2.2};

      std::complex<_Tp> z = a;

      set_real<_Tp, 33>(z);
      set_imag<_Tp, 44>(z);
    }

  // Test operators @=complex and @=real.
  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    sum(const std::complex<_Tp>& z, const std::complex<_Up>& w)
    {
      std::complex<_Tp> x = z;
      x += w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    sum(const std::complex<_Tp>& z, _Up w)
    {
      std::complex<_Tp> x = z;
      x += w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    dif(const std::complex<_Tp>& z, const std::complex<_Up>& w)
    {
      std::complex<_Tp> x = z;
      x -= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    dif(const std::complex<_Tp>& z, _Up w)
    {
      std::complex<_Tp> x = z;
      x -= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    prod(const std::complex<_Tp>& z, const std::complex<_Up>& w)
    {
      std::complex<_Tp> x = z;
      x *= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    prod(const std::complex<_Tp>& z, _Up w)
    {
      std::complex<_Tp> x = z;
      x *= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    quot(const std::complex<_Tp>& z, const std::complex<_Up>& w)
    {
      std::complex<_Tp> x = z;
      x /= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    constexpr std::complex<_Tp>
    quot(const std::complex<_Tp>& z, _Up w)
    {
      std::complex<_Tp> x = z;
      x /= w;
      return x;
    }

  template<typename _Tp, typename _Up>
    void
    test_operator_members()
    {
      constexpr std::complex<_Tp> a{10, 20};
      constexpr std::complex<_Up> b{6, 8};
      constexpr _Up c{10};

      constexpr auto apc = sum(a, c);
      static_assert(apc == std::complex<_Tp>{20, 20});
      constexpr auto amc = dif(a, c);
      static_assert(amc == std::complex<_Tp>{0, 20});
      constexpr auto atc = prod(a, c);
      static_assert(atc == std::complex<_Tp>{100, 200});
      constexpr auto adc = quot(a, c);
      static_assert(adc == std::complex<_Tp>{1, 2});

      constexpr auto apb = sum(a, b);
      static_assert(apb == std::complex<_Tp>{16, 28});
      constexpr auto amb = dif(a, b);
      static_assert(amb == std::complex<_Tp>{4, 12});
      constexpr auto atb = prod(a, b);
      static_assert(atb == std::complex<_Tp>{-100, 200});
      constexpr auto adb = quot(a, b);
      static_assert(adb == std::complex<_Tp>{11/_Tp{5}, 2/_Tp{5}});
    }
}

int main()
{
  __gnu_test::test_members<float>();
  __gnu_test::test_members<double>();
  __gnu_test::test_members<long double>();

  __gnu_test::test_operator_members<float, float>();
  __gnu_test::test_operator_members<float, double>();
  __gnu_test::test_operator_members<float, long double>();
  __gnu_test::test_operator_members<double, float>();
  __gnu_test::test_operator_members<double, double>();
  __gnu_test::test_operator_members<double, long double>();
#ifndef __LONG_DOUBLE_IBM128__ // IBM128 format is not constexpr foldable
  __gnu_test::test_operator_members<long double, float>();
  __gnu_test::test_operator_members<long double, double>();
  __gnu_test::test_operator_members<long double, long double>();
#endif

#if defined(_GLIBCXX_USE_FLOAT128)
  // Test primary template.
  __gnu_test::test_operator_members<__float128, __float128>();
#endif

  return 0;
}
