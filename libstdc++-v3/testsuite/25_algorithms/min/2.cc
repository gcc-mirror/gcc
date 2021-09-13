// 2000-03-29 sss/bkoz

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

template<typename T> 
  struct A { static const T a; };

template<typename T>
const T A<T>::a = T(3);

void test02()
{
  VERIFY( 2 == std::min(A<int>::a, 2) );
  VERIFY( 3 == std::min(A<int>::a, 4) );

  VERIFY( 2u == std::min(A<unsigned int>::a, 2u) );
  VERIFY( 3u == std::min(A<unsigned int>::a, 4u) );

  VERIFY( 2l == std::min(A<long>::a, 2l) );
  VERIFY( 3l == std::min(A<long>::a, 4l) );

  VERIFY( 2ul == std::min(A<unsigned long>::a, 2ul) );
  VERIFY( 3ul == std::min(A<unsigned long>::a, 4ul) );

#ifdef _GLIBCXX_USE_LONG_LONG
  VERIFY( 2ll == std::min(A<long long>::a, 2ll) );
  VERIFY( 3ll == std::min(A<long long>::a, 4ll) );

  VERIFY( 2ull == std::min(A<unsigned long long>::a, 2ull) );
  VERIFY( 3ull == std::min(A<unsigned long long>::a, 4ull) );
#endif

  VERIFY( short(2) == std::min(A<short>::a, short(2)) );
  VERIFY( short(3) == std::min(A<short>::a, short(4)) );

  VERIFY( (unsigned short)2 == std::min(A<unsigned short>::a, (unsigned short)2) );
  VERIFY( (unsigned short)3 == std::min(A<unsigned short>::a, (unsigned short)4) );

  VERIFY( (char)2 == std::min(A<char>::a, (char)2) );
  VERIFY( (char)3 == std::min(A<char>::a, (char)4) );

  VERIFY( (signed char)2 == std::min(A<signed char>::a, (signed char)2) );
  VERIFY( (signed char)3 == std::min(A<signed char>::a, (signed char)4) );

  VERIFY( (unsigned char)2 == std::min(A<unsigned char>::a, (unsigned char)2) );
  VERIFY( (unsigned char)3 == std::min(A<unsigned char>::a, (unsigned char)4) );

  VERIFY( (wchar_t)2 == std::min(A<wchar_t>::a, (wchar_t)2) );
  VERIFY( (wchar_t)3 == std::min(A<wchar_t>::a, (wchar_t)4) );

  VERIFY( 2.0 == std::min(A<double>::a, 2.0) );
  VERIFY( 3.0 == std::min(A<double>::a, 4.0) );

  VERIFY( float(2) == std::min(A<float>::a, float(2)) );
  VERIFY( float(3) == std::min(A<float>::a, float(4)) );

  VERIFY( (long double)2 == std::min(A<long double>::a, (long double)2) );
  VERIFY( (long double)3 == std::min(A<long double>::a, (long double)4) );
}

int main()
{
  test02();
  return 0;
}
