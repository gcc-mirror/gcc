// { dg-do run { target c++11 } }
// { dg-require-c-std "" }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <complex>
#include <limits>
#include <testsuite_hooks.h>

template<typename T>
  void do_test01()
  {
    if (std::numeric_limits<T>::has_quiet_NaN)
      {
	std::complex<T> c1(T(0), std::numeric_limits<T>::quiet_NaN());
	VERIFY( c1.real() == T(0) );
	VERIFY( std::isnan(c1.imag()) );

	std::complex<T> c2(std::numeric_limits<T>::quiet_NaN(), T(0));
	VERIFY( std::isnan(c2.real()) );
	VERIFY( c2.imag() == T(0) );

	std::complex<T> c3(std::numeric_limits<T>::quiet_NaN(),
			   std::numeric_limits<T>::quiet_NaN());
	VERIFY( std::isnan(c3.real()) );
	VERIFY( std::isnan(c3.imag()) );
      }
  }

// libstdc++/48760
void test01()
{
  do_test01<float>();
  do_test01<double>();
  do_test01<long double>();
}

int main()
{
  test01();
  return 0;
}
