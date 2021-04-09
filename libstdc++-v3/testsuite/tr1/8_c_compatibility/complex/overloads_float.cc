// { dg-do compile }

// 2006-01-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2021 Free Software Foundation, Inc.
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
#include <testsuite_tr1.h>

void test01()
{
  using __gnu_test::check_ret_type;

  typedef std::complex<float>       cmplx_f_type;
  typedef std::complex<double>      cmplx_d_type;
  typedef std::complex<long double> cmplx_ld_type;

  const float        f1 = 1.0f;
  const double       d1 = 1.0;
  const long double ld1 = 1.0l;

  const cmplx_f_type  c_f1(f1, f1);
  const cmplx_d_type  c_d1(d1, d1);
  const cmplx_ld_type c_ld1(ld1, ld1);

  check_ret_type<float>(std::tr1::arg(f1));
  check_ret_type<double>(std::tr1::arg(d1));
  check_ret_type<long double>(std::tr1::arg(ld1));

  check_ret_type<cmplx_f_type>(std::tr1::conj(f1));
  check_ret_type<cmplx_d_type>(std::tr1::conj(d1));
  check_ret_type<cmplx_ld_type>(std::tr1::conj(ld1));

  check_ret_type<float>(std::tr1::imag(f1));
  check_ret_type<double>(std::tr1::imag(d1));
  check_ret_type<long double>(std::tr1::imag(ld1));

  check_ret_type<float>(std::tr1::norm(f1));
  check_ret_type<double>(std::tr1::norm(d1));
  check_ret_type<long double>(std::tr1::norm(ld1));

  check_ret_type<cmplx_f_type>(std::tr1::polar(f1, f1));
  check_ret_type<cmplx_d_type>(std::tr1::polar(d1, f1));
  check_ret_type<cmplx_d_type>(std::tr1::polar(f1, d1));
  check_ret_type<cmplx_d_type>(std::tr1::polar(d1, d1));
  check_ret_type<cmplx_ld_type>(std::tr1::polar(ld1, d1));
  check_ret_type<cmplx_ld_type>(std::tr1::polar(d1, ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::polar(ld1, f1));
  check_ret_type<cmplx_ld_type>(std::tr1::polar(f1, ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::polar(ld1, ld1));

  check_ret_type<cmplx_f_type>(std::tr1::pow(c_f1, f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_d1, f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_f1, d1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_d1, d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_d1, ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, f1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_f1, ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, ld1));

  check_ret_type<cmplx_f_type>(std::tr1::pow(f1, c_f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(d1, c_f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(f1, c_d1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(d1, c_d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(ld1, c_d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(d1, c_ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(ld1, c_f1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(f1, c_ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(ld1, c_ld1));

  check_ret_type<cmplx_f_type>(std::tr1::pow(c_f1, c_f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_d1, c_f1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_f1, c_d1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(c_d1, c_d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, c_d1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_d1, c_ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, c_f1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_f1, c_ld1));
  check_ret_type<cmplx_ld_type>(std::tr1::pow(c_ld1, c_ld1));

  check_ret_type<float>(std::tr1::real(f1));
  check_ret_type<double>(std::tr1::real(d1));
  check_ret_type<long double>(std::tr1::real(ld1));
}
