// { dg-do run { target c++17 } }

// Copyright (C) 2000-2025 Free Software Foundation, Inc.
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
  VERIFY( 3 == std::clamp(A<int>::a, 2, 4) );
  VERIFY( 2 == std::clamp(A<int>::a, 1, 2) );
  VERIFY( 4 == std::clamp(A<int>::a, 4, 6) );

  VERIFY( 3u == std::clamp(A<unsigned int>::a, 2u, 4u) );
  VERIFY( 2u == std::clamp(A<unsigned int>::a, 1u, 2u) );
  VERIFY( 4u == std::clamp(A<unsigned int>::a, 4u, 6u) );

  VERIFY( 3l == std::clamp(A<long>::a, 2l, 4l) );
  VERIFY( 2l == std::clamp(A<long>::a, 1l, 2l) );
  VERIFY( 4l == std::clamp(A<long>::a, 4l, 6l) );

  VERIFY( 3ul == std::clamp(A<unsigned long>::a, 2ul, 4ul) );
  VERIFY( 2ul == std::clamp(A<unsigned long>::a, 1ul, 2ul) );
  VERIFY( 4ul == std::clamp(A<unsigned long>::a, 4ul, 6ul) );

#ifdef _GLIBCXX_USE_LONG_LONG
  VERIFY( 3ll == std::clamp(A<long long>::a, 2ll, 4ll) );
  VERIFY( 2ll == std::clamp(A<long long>::a, 1ll, 2ll) );
  VERIFY( 4ll == std::clamp(A<long long>::a, 4ll, 6ll) );

  VERIFY( 3ull == std::clamp(A<unsigned long long>::a, 2ull, 4ull) );
  VERIFY( 2ull == std::clamp(A<unsigned long long>::a, 1ull, 2ull) );
  VERIFY( 4ull == std::clamp(A<unsigned long long>::a, 4ull, 6ull) );
#endif

  VERIFY( (short)3 == std::clamp(A<short>::a, (short)2, (short)4) );
  VERIFY( (short)2 == std::clamp(A<short>::a, (short)1, (short)2) );
  VERIFY( (short)4 == std::clamp(A<short>::a, (short)4, (short)6) );

  VERIFY( (unsigned short)3 == std::clamp(A<unsigned short>::a, (unsigned short)2, (unsigned short)4) );
  VERIFY( (unsigned short)2 == std::clamp(A<unsigned short>::a, (unsigned short)1, (unsigned short)2) );
  VERIFY( (unsigned short)4 == std::clamp(A<unsigned short>::a, (unsigned short)4, (unsigned short)6) );

  VERIFY( (char)3 == std::clamp(A<char>::a, (char)2, (char)4) );
  VERIFY( (char)2 == std::clamp(A<char>::a, (char)1, (char)2) );
  VERIFY( (char)4 == std::clamp(A<char>::a, (char)4, (char)6) );

  VERIFY( (signed char)3 == std::clamp(A<signed char>::a, (signed char)2, (signed char)4) );
  VERIFY( (signed char)2 == std::clamp(A<signed char>::a, (signed char)1, (signed char)2) );
  VERIFY( (signed char)4 == std::clamp(A<signed char>::a, (signed char)4, (signed char)6) );

  VERIFY( (unsigned char)3 == std::clamp(A<unsigned char>::a, (unsigned char)2, (unsigned char)4) );
  VERIFY( (unsigned char)2 == std::clamp(A<unsigned char>::a, (unsigned char)1, (unsigned char)2) );
  VERIFY( (unsigned char)4 == std::clamp(A<unsigned char>::a, (unsigned char)4, (unsigned char)6) );

  VERIFY( (wchar_t)3 == std::clamp(A<wchar_t>::a, (wchar_t)2, (wchar_t)4) );
  VERIFY( (wchar_t)2 == std::clamp(A<wchar_t>::a, (wchar_t)1, (wchar_t)2) );
  VERIFY( (wchar_t)4 == std::clamp(A<wchar_t>::a, (wchar_t)4, (wchar_t)6) );

  VERIFY( 3.0 == std::clamp(A<double>::a, 2.0, 4.0) );
  VERIFY( 2.0 == std::clamp(A<double>::a, 1.0, 2.0) );
  VERIFY( 4.0 == std::clamp(A<double>::a, 4.0, 6.0) );

  VERIFY( 3.0f == std::clamp(A<float>::a, 2.0f, 4.0f) );
  VERIFY( 2.0f == std::clamp(A<float>::a, 1.0f, 2.0f) );
  VERIFY( 4.0f == std::clamp(A<float>::a, 4.0f, 6.0f) );

  VERIFY( 3.0l == std::clamp(A<long double>::a, 2.0l, 4.0l) );
  VERIFY( 2.0l == std::clamp(A<long double>::a, 1.0l, 2.0l) );
  VERIFY( 4.0l == std::clamp(A<long double>::a, 4.0l, 6.0l) );
}

int
main()
{
  test02();
  return 0;
}
