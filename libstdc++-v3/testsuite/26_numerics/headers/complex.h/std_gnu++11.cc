// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// { dg-add-options no_pch }

#include <complex.h>

// Should be equivalent to #include <complex>
template class std::complex<double>;

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

#ifdef complex
# error "'complex' is defined as a macro by <complex.h> for -std=gnu++11"
#endif
