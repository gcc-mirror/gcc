// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=c++98" }

#include <complex.h>

// Should be equivalent to C99 <complex>, not C++ <complex>
namespace std
{
  struct complex;
}

#if _GLIBCXX_HAVE_COMPLEX_H
namespace test
{
  using ::cacos;
  using ::casin;
  using ::catan;
  using ::ccos;
  using ::csin;
  using ::ctan;
  using ::ccosh;
  using ::csinh;
  using ::ctanh;
  using ::cexp;
  using ::clog;
  using ::cabs;
  using ::cpow;
  using ::csqrt;
  using ::carg;
  using ::cimag;
  using ::conj;
  using ::cproj;
  using ::creal;
}
#endif

#ifndef complex
# error "'complex' is not defined as a macro by <complex.h> for -std=c++98"
#endif
