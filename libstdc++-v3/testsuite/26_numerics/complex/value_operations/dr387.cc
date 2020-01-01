// 2008-05-22  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

// DR 387. std::complex over-encapsulated.
template<typename T>
  void
  do_test()
  {
    const T r = 1.0;
    const T i = -1.0;
    const T v = 0.0;

    std::complex<T> z1(r, i);
    z1.real(v);
    VERIFY( z1.real() == v );
    VERIFY( z1.imag() == i );

    std::complex<T> z2(r, i);
    z2.imag(v);
    VERIFY( z2.real() == r );
    VERIFY( z2.imag() == v );
  }

int main()
{
  do_test<float>();
  do_test<double>();
  do_test<long double>();
  return 0;
}
