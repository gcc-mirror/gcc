// { dg-options "-std=c++17 -fno-fast-math" }
// { dg-do compile { target c++17 } }
// { dg-require-cmath "" }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <experimental/simd>

template <typename V>
  void
  is_usable()
  {
    static_assert(std::is_default_constructible_v<V>);
    static_assert(std::is_destructible_v         <V>);
    static_assert(std::is_default_constructible_v<typename V::mask_type>);
    static_assert(std::is_destructible_v         <typename V::mask_type>);
  }

template <typename T>
  void
  test01()
  {
    namespace stdx = std::experimental;
    is_usable<stdx::simd<T>>();
    is_usable<stdx::native_simd<T>>();
    is_usable<stdx::fixed_size_simd<T, 3>>();
    is_usable<stdx::fixed_size_simd<T, stdx::simd_abi::max_fixed_size<T>>>();
  }

int main()
{
  test01<char>();
  test01<wchar_t>();
  test01<char16_t>();
  test01<char32_t>();

  test01<signed char>();
  test01<unsigned char>();
  test01<short>();
  test01<unsigned short>();
  test01<int>();
  test01<unsigned int>();
  test01<long>();
  test01<unsigned long>();
  test01<long long>();
  test01<unsigned long long>();
  test01<float>();
  test01<double>();
  test01<long double>();
}
