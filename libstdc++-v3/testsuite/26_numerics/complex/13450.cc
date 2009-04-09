// { dg-do run { xfail broken_cplxf_arg } }

// Copyright (C) 2004, 2009 Free Software Foundation
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

// 26.2.8 complex transcendentals

#include <complex>
#include <limits>
#include <testsuite_hooks.h>

template<typename T>
  void test01_do(T a, T b)
  {
    using namespace std;
    bool test __attribute__((unused)) = true;
    typedef complex<T> cplx;

    T eps = numeric_limits<T>::epsilon() * 100;
    
    cplx ref = pow(cplx(a, T()), cplx(b, T()));
    cplx res1 = pow(a, cplx(b, T()));
    cplx res2 = pow(cplx(a, T()), b);
    
    VERIFY( abs(ref - res1) < eps );
    VERIFY( abs(ref - res2) < eps );
    VERIFY( abs(res1 - res2) < eps );
  }

// libstdc++/13450
void test01()
{
  float f1 = -1.0f;
  float f2 = 0.5f;
  test01_do(f1, f2);

  f1 = -3.2f;
  f2 = 1.4f;
  test01_do(f1, f2);

  double d1 = -1.0;
  double d2 = 0.5;
  test01_do(d1, d2);

  d1 = -3.2;
  d2 = 1.4;
  test01_do(d1, d2);

#if __LDBL_MANT_DIG__ != 106
  /* For IBM long double, epsilon is too small (since 1.0 plus any
     double is representable) to be able to expect results within
     epsilon * 100 (which may be much less than 1ulp for a particular
     long double value).  */
  long double ld1 = -1.0l;
  long double ld2 = 0.5l;
  test01_do(ld1, ld2);

  ld1 = -3.2l;
  ld2 = 1.4l;
  test01_do(ld1, ld2);
#endif
}

int main()
{
  test01();
  return 0;
}
