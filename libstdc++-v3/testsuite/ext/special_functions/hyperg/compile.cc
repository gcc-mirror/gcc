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

// hyperg

#include <cmath>

void
test01()
{
  float af = 2.0F, bf = 10.0F, cf = 3.0F, xf = 0.5F;
  double ad = 2.0, bd = 10.0, cd = 3.0, xd = 0.5;
  long double al = 2.0L, bl = 10.0L, cl = 3.0L, xl = 0.5L;

  __gnu_cxx::hyperg(af, bf, cf, xf);
  __gnu_cxx::hypergf(af, bf, cf, xf);
  __gnu_cxx::hyperg(ad, bd, cd, xd);
  __gnu_cxx::hyperg(al, bl, cl, xl);
  __gnu_cxx::hypergl(al, bl, cl, xl);

  return;
}

