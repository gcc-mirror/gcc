// { dg-options "-std=gnu++0x" }
// 2008-05-22  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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

// DR 781. std::complex should add missing C99 functions.
// DR 1137. Return type of conj and proj.
void test01()
{
  bool test __attribute__((unused)) = true;
  using __gnu_test::check_ret_type;

  typedef std::complex<float>       cmplx_f_type;
  typedef std::complex<double>      cmplx_d_type;
  typedef std::complex<long double> cmplx_ld_type;

  const int          i1 = 1;
  const float        f1 = 1.0f;
  const double       d1 = 1.0;
  const long double ld1 = 1.0l;

  const cmplx_f_type  c_f1(f1, f1);
  const cmplx_d_type  c_d1(d1, d1);
  const cmplx_ld_type c_ld1(ld1, ld1);

  check_ret_type<cmplx_f_type>(std::proj(c_f1));
  check_ret_type<cmplx_d_type>(std::proj(c_d1));
  check_ret_type<cmplx_ld_type>(std::proj(c_ld1));

  check_ret_type<float>(std::proj(f1));
  check_ret_type<double>(std::proj(d1));
  check_ret_type<double>(std::proj(i1));
  VERIFY( std::proj(i1) == std::proj(double(i1)) );
  check_ret_type<long double>(std::proj(ld1));

  check_ret_type<cmplx_f_type>(std::conj(c_f1));
  check_ret_type<cmplx_d_type>(std::conj(c_d1));
  check_ret_type<cmplx_ld_type>(std::conj(c_ld1));

  check_ret_type<float>(std::conj(f1));
  check_ret_type<double>(std::conj(d1));
  check_ret_type<double>(std::conj(i1));
  VERIFY( std::conj(i1) == std::conj(double(i1)) );
  check_ret_type<long double>(std::conj(ld1));
}

int main()
{
  test01();
  return 0;
}
