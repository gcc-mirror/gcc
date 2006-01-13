// 2006-01-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 8.1 Additions to header <complex>

#include <tr1/complex>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using namespace std::tr1;
  using namespace __gnu_test;

  typedef std::complex<float>       cmplx_f_type;
  typedef std::complex<double>      cmplx_d_type;
  typedef std::complex<long double> cmplx_ld_type;

  const float        f1 = 1.0f;
  const double       d1 = 1.0;
  const long double ld1 = 1.0l;

  const cmplx_f_type  c_f1(f1, f1);
  const cmplx_d_type  c_d1(d1, d1);
  const cmplx_ld_type c_ld1(ld1, ld1);

  check_ret_type<float>(arg(f1));
  check_ret_type<double>(arg(d1));
  check_ret_type<long double>(arg(ld1));

  check_ret_type<cmplx_f_type>(conj(f1));
  check_ret_type<cmplx_d_type>(conj(d1));
  check_ret_type<cmplx_ld_type>(conj(ld1));
  
  check_ret_type<float>(imag(f1));
  check_ret_type<double>(imag(d1));
  check_ret_type<long double>(imag(ld1));
  
  check_ret_type<float>(norm(f1));
  check_ret_type<double>(norm(d1));
  check_ret_type<long double>(norm(ld1));

  check_ret_type<cmplx_f_type>(polar(f1, f1));
  check_ret_type<cmplx_d_type>(polar(d1, f1));
  check_ret_type<cmplx_d_type>(polar(f1, d1));
  check_ret_type<cmplx_d_type>(polar(d1, d1));
  check_ret_type<cmplx_ld_type>(polar(ld1, d1));
  check_ret_type<cmplx_ld_type>(polar(d1, ld1));
  check_ret_type<cmplx_ld_type>(polar(ld1, f1));
  check_ret_type<cmplx_ld_type>(polar(f1, ld1));
  check_ret_type<cmplx_ld_type>(polar(ld1, ld1));

  check_ret_type<cmplx_f_type>(pow(c_f1, f1));
  check_ret_type<cmplx_d_type>(pow(c_d1, f1));
  check_ret_type<cmplx_d_type>(pow(c_f1, d1));
  check_ret_type<cmplx_d_type>(pow(c_d1, d1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, d1));
  check_ret_type<cmplx_ld_type>(pow(c_d1, ld1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, f1));
  check_ret_type<cmplx_ld_type>(pow(c_f1, ld1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, ld1));

  check_ret_type<cmplx_f_type>(pow(f1, c_f1));
  check_ret_type<cmplx_d_type>(pow(d1, c_f1));
  check_ret_type<cmplx_d_type>(pow(f1, c_d1));
  check_ret_type<cmplx_d_type>(pow(d1, c_d1));
  check_ret_type<cmplx_ld_type>(pow(ld1, c_d1));
  check_ret_type<cmplx_ld_type>(pow(d1, c_ld1));
  check_ret_type<cmplx_ld_type>(pow(ld1, c_f1));
  check_ret_type<cmplx_ld_type>(pow(f1, c_ld1));
  check_ret_type<cmplx_ld_type>(pow(ld1, c_ld1));

  check_ret_type<cmplx_f_type>(pow(c_f1, c_f1));
  check_ret_type<cmplx_d_type>(pow(c_d1, c_f1));
  check_ret_type<cmplx_d_type>(pow(c_f1, c_d1));
  check_ret_type<cmplx_d_type>(pow(c_d1, c_d1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, c_d1));
  check_ret_type<cmplx_ld_type>(pow(c_d1, c_ld1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, c_f1));
  check_ret_type<cmplx_ld_type>(pow(c_f1, c_ld1));
  check_ret_type<cmplx_ld_type>(pow(c_ld1, c_ld1));

  check_ret_type<float>(real(f1));
  check_ret_type<double>(real(d1));
  check_ret_type<long double>(real(ld1));
}
    
int main()
{
  test01();
  return 0;
}
