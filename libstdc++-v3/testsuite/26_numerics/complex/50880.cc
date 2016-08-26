// { dg-do run { target c++11 } }
//
// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

template<typename T>
  void test01_do()
  {
    bool test __attribute__((unused)) = true;

    const std::complex<T> ca(T(-2), T(2));
    const std::complex<T> cb(T(-2), T(0));
    const std::complex<T> cc(T(-2), T(-2));

    std::complex<T> cra = std::acosh(ca);
    std::complex<T> crb = std::acosh(cb);
    std::complex<T> crc = std::acosh(cc);

    VERIFY( cra.real() > T(0) );
    VERIFY( crb.real() > T(0) );
    VERIFY( crc.real() > T(0) );
  }

// libstdc++/50880
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
