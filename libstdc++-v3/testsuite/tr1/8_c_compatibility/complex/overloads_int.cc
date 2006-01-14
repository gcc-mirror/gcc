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
  bool test __attribute__((unused)) = true;
  using namespace std::tr1;
  using namespace __gnu_test;

  typedef std::complex<float>  cmplx_f_type;
  typedef std::complex<double> cmplx_d_type;

  const int       i1 = 1;
  const unsigned  u1 = 1;
  const long      l1 = 1;
  const double    f1 = 1.0f;
  const double    d0 = 0.0;
  const double    d1 = 1.0;
    
  check_ret_type<double>(arg(i1));
  VERIFY( arg(i1) == arg(d1) );
  VERIFY( arg(i1) == arg(cmplx_d_type(d1, d0)) );

  check_ret_type<cmplx_d_type>(conj(i1));
  VERIFY( conj(i1) == conj(d1) );
  VERIFY( conj(i1) == conj(cmplx_d_type(d1, d0)) );

  check_ret_type<double>(imag(i1));
  VERIFY( imag(i1) == imag(d1) );
  VERIFY( imag(i1) == imag(cmplx_d_type(d1, d0)) );

  check_ret_type<double>(norm(i1));
  VERIFY( norm(i1) == norm(d1) );
  VERIFY( norm(i1) == norm(cmplx_d_type(d1, d0)) );

  check_ret_type<cmplx_d_type>(polar(i1, i1));
  VERIFY( polar(i1, i1) == polar(d1, d1) );

  // NB: According to the letter of 8.1.9/3 the return type
  //     should be a cmplx_d_type, but the existing overload
  //     std::pow(const complex<>&, int) wins.
  check_ret_type<cmplx_f_type>(pow(cmplx_f_type(f1, f1), i1));

  check_ret_type<cmplx_d_type>(pow(cmplx_f_type(f1, f1), u1));
  check_ret_type<cmplx_d_type>(pow(cmplx_f_type(f1, f1), l1));
  check_ret_type<cmplx_d_type>(pow(cmplx_d_type(d1, d1), i1));

  // See above comment.
  // VERIFY( pow(cmplx_d_type(d1, d1), i1) == pow(cmplx_d_type(d1, d1), d1) );
  VERIFY( pow(cmplx_d_type(d1, d1), u1) == pow(cmplx_d_type(d1, d1), d1) );
  VERIFY( pow(cmplx_d_type(d1, d1), l1) == pow(cmplx_d_type(d1, d1), d1) );

  check_ret_type<cmplx_d_type>(pow(i1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(pow(u1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(pow(l1, cmplx_f_type(f1, f1)));
  check_ret_type<cmplx_d_type>(pow(i1, cmplx_d_type(d1, d1)));
  VERIFY( pow(i1, cmplx_d_type(d1, d1)) == pow(d1, cmplx_d_type(d1, d1)) );
  VERIFY( pow(u1, cmplx_d_type(d1, d1)) == pow(d1, cmplx_d_type(d1, d1)) );
  VERIFY( pow(l1, cmplx_d_type(d1, d1)) == pow(d1, cmplx_d_type(d1, d1)) );

  check_ret_type<double>(real(i1));
  VERIFY( real(i1) == real(d1) );
  VERIFY( real(i1) == real(cmplx_d_type(d1, d1)) );
}

int main()
{
  test01();
  return 0;
}
