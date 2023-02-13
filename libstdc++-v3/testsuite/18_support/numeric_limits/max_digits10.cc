// { dg-do run { target c++11 } }
// { dg-add-options ieee }

// 2010-02-25  Ed Smith-Rowland

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <cmath>
#include <testsuite_hooks.h>

void
test01()
{
  VERIFY( std::numeric_limits<bool>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<char>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<signed char>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned char>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<wchar_t>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<short>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned short>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<int>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned int>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<long>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned long>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<long long>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned long long>::max_digits10 == 0 );
#ifdef _GLIBCXX_USE_CHAR8_T
  VERIFY( std::numeric_limits<char8_t>::max_digits10 == 0 );
#endif
  VERIFY( std::numeric_limits<char16_t>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<char32_t>::max_digits10 == 0 );

  // GNU Extensions.
#ifdef __SIZEOF_INT128__
  VERIFY( std::numeric_limits<__int128>::max_digits10 == 0 );
  VERIFY( std::numeric_limits<unsigned __int128>::max_digits10 == 0 );
#endif

  const int f_max_digits10 = (2 + std::numeric_limits<float>::digits
			      * 643 / 2136);
  VERIFY( std::numeric_limits<float>::max_digits10 == f_max_digits10 );

  const int d_max_digits10 = (2 + std::numeric_limits<double>::digits
			      * 643 / 2136);
  VERIFY( std::numeric_limits<double>::max_digits10 == d_max_digits10 );

  const int ld_max_digits10 = (2 + std::numeric_limits<long double>::digits
			       * 643 / 2136);
  VERIFY( std::numeric_limits<long double>::max_digits10 == ld_max_digits10 );
}

int main()
{
  test01();
  return 0;
}
