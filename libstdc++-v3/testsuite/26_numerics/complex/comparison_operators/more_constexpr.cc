// { dg-do compile { target c++20 } }

// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

template<bool ok>
  struct thing
  { };

// 
template<typename _Tp>
  void
  test_comparison()
  {
    constexpr std::complex<_Tp> a{1.1, 2.2};
    constexpr std::complex<_Tp> b{3.3, 4.4};
    if constexpr (a == b)
      auto c [[maybe_unused]] = a + b;
    if constexpr (a != b)
      auto c [[maybe_unused]] = a - b;

    thing<a == b> thing1 [[maybe_unused]];
    thing<a != b> thing2 [[maybe_unused]];
  }

int
main()
{
  test_comparison<float>();
  test_comparison<double>();
  test_comparison<long double>();

  return 0;
}
