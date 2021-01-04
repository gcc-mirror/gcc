// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

void test02() 
{
  std::wstring str_01;
  const std::wstring str_02(L"or coltrane playing tunji with jimmy garrison");
  const std::wstring str_03(L"coltrane");

  std::wstringbuf isbuf_01(std::ios_base::in);
  std::wstringbuf isbuf_02(str_02, std::ios_base::in);
  std::wistream is_01(0);
  std::wistream is_02(&isbuf_02);
  std::ios_base::iostate state1, state2, statefail;
  statefail = std::ios_base::failbit;

  // template<_CharT, _Traits>
  //  basic_istream& operator>>(istream&, _CharT&)
  wchar_t c1 = L'c', c2 = L'c';
  state1 = is_01.rdstate();
  is_01 >> c1;   
  state2 = is_01.rdstate();
  VERIFY( state1 != state2 );
  VERIFY( c1 == c2 );
  VERIFY( static_cast<bool>(state2 & statefail) );

  state1 = is_02.rdstate();
  is_02 >> c1;   
  state2 = is_02.rdstate();
  VERIFY( state1 == state2 );
  VERIFY( c1 == L'o' );
  is_02 >> c1;   
  is_02 >> c1;   
  VERIFY( c1 == L'c' );
  VERIFY( !static_cast<bool>(state2 & statefail) );
}

int main()
{
  test02();
  return 0;
}
