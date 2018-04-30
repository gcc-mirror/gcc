// { dg-do compile { target c++11 } }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// airy_bi

#include <cmath>

void
test01()
{
  float xf = 0.5F;
  double xd = 0.5;
  long double xl = 0.5L;

  __gnu_cxx::airy_bi(xf);
  __gnu_cxx::airy_bif(xf);
  __gnu_cxx::airy_bi(xd);
  __gnu_cxx::airy_bi(xl);
  __gnu_cxx::airy_bil(xl);

  return;
}

