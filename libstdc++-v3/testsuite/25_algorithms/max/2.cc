// 2000-03-29 sss/bkoz

// Copyright (C) 2000-2018 Free Software Foundation, Inc.
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
  VERIFY( 3 == std::max(A<int>::a, 2) );
  VERIFY( 4 == std::max(A<int>::a, 4) );

  VERIFY( 3u == std::max(A<unsigned int>::a, 2u) );
  VERIFY( 4u == std::max(A<unsigned int>::a, 4u) );

  VERIFY( 3l == std::max(A<long>::a, 2l) );
  VERIFY( 4l == std::max(A<long>::a, 4l) );

  VERIFY( 3ul == std::max(A<unsigned long>::a, 2ul) );
  VERIFY( 4ul == std::max(A<unsigned long>::a, 4ul) );

#ifdef _GLIBCXX_USE_LONG_LONG
  VERIFY( 3ll == std::max(A<long long>::a, 2ll) );
  VERIFY( 4ll == std::max(A<long long>::a, 4ll) );

  VERIFY( 3ull == std::max(A<unsigned long long>::a, 2ull) );
  VERIFY( 4ull == std::max(A<unsigned long long>::a, 4ull) );
#endif

  VERIFY( short(3) == std::max(A<short>::a, short(2)) );
  VERIFY( short(4) == std::max(A<short>::a, short(4)) );

  VERIFY( (unsigned short)3 == std::max(A<unsigned short>::a, (unsigned short)2) );
  VERIFY( (unsigned short)4 == std::max(A<unsigned short>::a, (unsigned short)4) );

  VERIFY( (char)3 == std::max(A<char>::a, (char)2) );
  VERIFY( (char)4 == std::max(A<char>::a, (char)4) );

  VERIFY( (signed char)3 == std::max(A<signed char>::a, (signed char)2) );
  VERIFY( (signed char)4 == std::max(A<signed char>::a, (signed char)4) );

  VERIFY( (unsigned char)3 == std::max(A<unsigned char>::a, (unsigned char)2) );
  VERIFY( (unsigned char)4 == std::max(A<unsigned char>::a, (unsigned char)4) );

  VERIFY( (wchar_t)3 == std::max(A<wchar_t>::a, (wchar_t)2) );
  VERIFY( (wchar_t)4 == std::max(A<wchar_t>::a, (wchar_t)4) );
}

int main()
{
  test02();
  return 0;
}
