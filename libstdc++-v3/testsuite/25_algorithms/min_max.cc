// 2000-03-29 sss/bkoz

// Copyright (C) 2000, 2003, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  const int& x = std::max(1, 2);
  const int& y = std::max(4, 3);
  VERIFY( x == 2 );
  VERIFY( y == 4 );

  const int& xc = std::max(1, 2, std::greater<int>());
  const int& yc = std::max(4, 3, std::greater<int>());
  VERIFY( xc == 1 );
  VERIFY( yc == 3 );

  const int& z = std::min(1, 2);
  const int& w = std::min(4, 3);
  VERIFY( z == 1 );
  VERIFY( w == 3 );

  const int& zc = std::min(1, 2, std::greater<int>());
  const int& wc = std::min(4, 3, std::greater<int>());
  VERIFY( zc == 2 );
  VERIFY( wc == 4 );
}

template<typename T> 
  struct A { static const T a; };

template<typename T>
const T A<T>::a = T(3);

#if !__GXX_WEAK__
// Explicitly instantiate for systems with no COMDAT or weak support.
template int A<int>::a;
template int A<unsigned int>::a;
template int A<short>::a;
template int A<unsigned short>::a;
template int A<long>::a;
template int A<unsigned long>::a;
template int A<long long>::a;
template int A<unsigned long long>::a;
template int A<char>::a;
template int A<signed char>::a;
template int A<unsigned char>::a;
template int A<wchar_t>::a;
template int A<float>::a;
template int A<double>::a;
template int A<long double>::a;
#endif

void test02()
{
  bool test __attribute__((unused)) = true;

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
  test01();
  test02();
  return 0;
}
