// 2006-01-10  Paolo Carlini  <pcarlini@suse.de>
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

template<typename T>
  void test01_do()
  {
    using namespace std::tr1;
    typedef std::complex<T> cmplx_type;

    cmplx_type ans;

    ans = acos(cmplx_type(1.0, 1.0));
    ans = asin(cmplx_type(1.0, 1.0));
    ans = atan(cmplx_type(1.0, 1.0));

    ans = acosh(cmplx_type(1.0, 1.0));
    ans = asinh(cmplx_type(1.0, 1.0));
    ans = atanh(cmplx_type(1.0, 1.0));
    ans = fabs(cmplx_type(1.0, 1.0));
  }
    
void test01()
{
  test01_do<float>();
  test01_do<double>();
  test01_do<long double>();
}

int main()
{
  test01();
  return 0;
}
