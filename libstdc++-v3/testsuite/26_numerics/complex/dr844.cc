// { dg-do run { target c++11 } }
// 2008-06-12  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

#include <complex>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

// In C++11 mode we used to implement the resolution of
// DR 844. complex pow return type is ambiguous.
// However, doing that causes all sorts of issues, see, for example:
//   http://gcc.gnu.org/ml/libstdc++/2013-01/msg00058.html
// and also PR57974.
void test01()
{
  using __gnu_test::check_ret_type;

  typedef std::complex<float>       cmplx_f_type;
  typedef std::complex<double>      cmplx_d_type;
  typedef std::complex<long double> cmplx_ld_type;

  const int          i1 = 1;
  const float        f1 = 1.0f;
  const double       d1 = 1.0;
  const long double ld1 = 1.0l;

  check_ret_type<cmplx_f_type>(std::pow(cmplx_f_type(f1, f1), i1));
  check_ret_type<cmplx_d_type>(std::pow(cmplx_d_type(d1, d1), i1));
  check_ret_type<cmplx_ld_type>(std::pow(cmplx_ld_type(ld1, ld1), i1));
}

int main()
{
  test01();
  return 0;
}
