// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

// 27.6.1.2.3 character extractors

#include <istream>
#include <sstream>
#include <testsuite_hooks.h>

void test01() 
{
  std::wstring str_01;
  const std::wstring str_02(L"coltrane playing 'softly as a morning sunrise'");
  const std::wstring str_03(L"coltrane");

  std::wstringbuf isbuf_01(std::ios_base::in);
  std::wstringbuf isbuf_02(str_02, std::ios_base::in);
  std::wistream is_01(0);
  std::wistream is_02(&isbuf_02);

  std::ios_base::iostate state1, state2, statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, _CharT*)
  const int n = 20;
  wchar_t array1[n];
  array1[0] = L'\0';
  typedef std::wios::traits_type ctraits_type;
  ctraits_type::int_type i1, i2;

  state1 = is_01.rdstate();
  i1 = ctraits_type::length(array1);
  is_01 >> array1;   // should snake 0 characters, not alter stream state
  i2 = ctraits_type::length(array1);
  state2 = is_01.rdstate();
  VERIFY( i1 == i2 );
  VERIFY( state1 != state2 );
  VERIFY( static_cast<bool>(state2 & statefail) );

  state1 = is_02.rdstate();
  is_02 >> array1;   // should snake L"coltrane"
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( !static_cast<bool>(state2 & statefail) );
  VERIFY( array1[str_03.size() - 1] == L'e' );
  array1[str_03.size()] = L'\0';
  VERIFY( !str_03.compare(0, str_03.size(), array1) );
  std::wistream::int_type int1 = is_02.peek(); // should be L' '
  VERIFY( int1 == L' ' );

  state1 = is_02.rdstate();
  is_02 >> array1;   // should snake L"playing" as sentry "eats" ws
  state2 = is_02.rdstate();
  int1 = is_02.peek(); // should be L' '
  VERIFY( int1 == L' ' );
  VERIFY( state1 == state2 );
  VERIFY( !static_cast<bool>(state2 & statefail) );
}

int main()
{
  test01();
  return 0;
}
