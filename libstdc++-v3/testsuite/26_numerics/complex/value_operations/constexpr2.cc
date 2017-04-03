// { dg-do compile { target c++11 } }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

int main()
{
  constexpr std::complex<int> c{};
  constexpr auto r __attribute__((unused)) = real(c);
  constexpr auto i __attribute__((unused)) = imag(c);
  constexpr double r2 __attribute__((unused)) = std::real(0.0);
  constexpr double i2 __attribute__((unused)) = std::imag(0.0);
}
