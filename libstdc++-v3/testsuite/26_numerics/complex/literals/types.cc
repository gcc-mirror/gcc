// Use -std=c++14 explicitly, because -std=gnu++14 enables GNU extension for
// complex literals, so 1.0if is __complex__ float not std::complex<float>.
// { dg-options "-std=c++14" }
// { dg-do compile }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
#include <type_traits>

void
test02()
{
  using namespace std::literals::complex_literals;

  static_assert(std::is_same<decltype(1.0if), std::complex<float>>::value,
		"1.0if is std::complex<float>");

  static_assert(std::is_same<decltype(1if), std::complex<float>>::value,
		"1if is std::complex<float>");

  static_assert(std::is_same<decltype(1.0i), std::complex<double>>::value,
		"1.0i is std::complex<double>");

  static_assert(std::is_same<decltype(1i), std::complex<double>>::value,
		"1i is std::complex<double>");

  static_assert(std::is_same<decltype(1.0il), std::complex<long double>>::value,
		"1.0il is std::complex<long double>");

  static_assert(std::is_same<decltype(1il), std::complex<long double>>::value,
		"1il is std::complex<long double>");
}
