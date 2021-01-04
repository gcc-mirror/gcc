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
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using __gnu_test::check_ret_type;

  typedef std::complex<float>  cmplx_f_type;
  typedef std::complex<double> cmplx_d_type;

  const int       i1 = 1;
  const unsigned  u1 = 1;
  const long      l1 = 1;
  const double    f1 = 1.0f;
  const double    d1 = 1.0;

  check_ret_type<double>(std::tr1::arg(i1));
  VERIFY( std::tr1::arg(i1) == std::tr1::arg(double(i1)) );
  VERIFY( std::tr1::arg(i1) == std::tr1::arg(cmplx_d_type(double(i1))) );

  check_ret_type<cmplx_d_type>(std::tr1::conj(i1));
  VERIFY( std::tr1::conj(i1) == std::tr1::conj(double(i1)) );
  VERIFY( std::tr1::conj(i1) == std::tr1::conj(cmplx_d_type(double(i1))) );

  check_ret_type<double>(std::tr1::imag(i1));
  VERIFY( std::tr1::imag(i1) == std::tr1::imag(double(i1)) );
  VERIFY( std::tr1::imag(i1) == std::tr1::imag(cmplx_d_type(double(i1))) );

  check_ret_type<double>(std::tr1::norm(i1));
  VERIFY( std::tr1::norm(i1) == std::tr1::norm(double(i1)) );
  // std::norm<const complex<>&) is mathematically equivalent to just
  // this for a real, but the general algorithm goes through std::abs
  // and a multiplication.
  VERIFY( std::tr1::norm(i1) == double(i1) * double(i1) );

  // NB: The existing std::polar wins and a cmplx_i_type is returned.
  // check_ret_type<cmplx_d_type>(std::tr1::polar(i1, i1));
  // VERIFY( std::tr1::polar(i1, i1)
  //         == std::tr1::polar(double(i1), double(i1)) );
  typedef std::complex<int> cmplx_i_type;
  check_ret_type<cmplx_i_type>(std::tr1::polar(i1, i1));

  check_ret_type<cmplx_d_type>(std::tr1::pow(cmplx_f_type(f1, f1), i1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(cmplx_f_type(f1, f1), u1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(cmplx_f_type(f1, f1), l1));
  check_ret_type<cmplx_d_type>(std::tr1::pow(cmplx_d_type(d1, d1), i1));

  VERIFY( std::tr1::pow(cmplx_d_type(d1, d1), i1)
	  == std::tr1::pow(cmplx_d_type(d1, d1), double(i1)) );
  VERIFY( std::tr1::pow(cmplx_d_type(d1, d1), u1)
	  == std::tr1::pow(cmplx_d_type(d1, d1), double(u1)) );
  VERIFY( std::tr1::pow(cmplx_d_type(d1, d1), l1)
	  == std::tr1::pow(cmplx_d_type(d1, d1), double(l1)) );

  check_ret_type<cmplx_d_type>(std::tr1::pow(i1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(std::tr1::pow(u1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(std::tr1::pow(l1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(std::tr1::pow(i1, cmplx_d_type(d1, d1)));
  VERIFY( std::tr1::pow(i1, cmplx_d_type(d1, d1))
	  == std::tr1::pow(double(i1), cmplx_d_type(d1, d1)) );
  VERIFY( std::tr1::pow(u1, cmplx_d_type(d1, d1))
	  == std::tr1::pow(double(u1), cmplx_d_type(d1, d1)) );
  VERIFY( std::tr1::pow(l1, cmplx_d_type(d1, d1))
	  == std::tr1::pow(double(l1), cmplx_d_type(d1, d1)) );

  check_ret_type<double>(std::tr1::real(i1));
  VERIFY( std::tr1::real(i1) == std::tr1::real(double(i1)) );
  VERIFY( std::tr1::real(i1) == std::tr1::real(cmplx_d_type(double(i1))) );
}

int main()
{
  test01();
  return 0;
}
