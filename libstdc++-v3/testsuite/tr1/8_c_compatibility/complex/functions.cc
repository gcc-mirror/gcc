// { dg-do compile }

// 2006-01-10  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.1 Additions to header <complex>

#include <tr1/complex>

template<typename T>
  void test01_do()
  {
    typedef std::complex<T> cmplx_type;

    cmplx_type ans;

    ans = std::tr1::acos(cmplx_type(1.0, 1.0));
    ans = std::tr1::asin(cmplx_type(1.0, 1.0));
    ans = std::tr1::atan(cmplx_type(1.0, 1.0));

    ans = std::tr1::acosh(cmplx_type(1.0, 1.0));
    ans = std::tr1::asinh(cmplx_type(1.0, 1.0));
    ans = std::tr1::atanh(cmplx_type(1.0, 1.0));
    ans = std::tr1::fabs(cmplx_type(1.0, 1.0));
  }

void test01()
{
  test01_do<float>();
  test01_do<double>();
  test01_do<long double>();
}
