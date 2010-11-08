// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

int main()
{
  __gnu_test::constexpr_comparison_eq_ne test;
  test.operator()<std::complex<float>>();
  test.operator()<std::complex<float>, float>();
  test.operator()<float,std::complex<float>>();

  test.operator()<std::complex<double>>();
  test.operator()<std::complex<double>, double>();
  test.operator()<double,std::complex<double>>();

  test.operator()<std::complex<long double>>();
  test.operator()<std::complex<long double>, long double>();
  test.operator()<long double,std::complex<long double>>();
  return 0;
}
