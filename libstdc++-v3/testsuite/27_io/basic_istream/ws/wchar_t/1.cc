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

// 27.6.1.4 standard basic_istream manipulators

#include <istream>
#include <sstream>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01(void)
{
  const wchar_t str_lit01[] = L"  venice ";
  const std::wstring str01(L" santa barbara ");
  std::wstring str02(str_lit01);
  std::wstring str04;
  std::wstring str05;

  // template<_CharT, _Traits>
  //  basic_istream<_CharT, _Traits>& ws(basic_istream<_Char, _Traits>& is)
  std::wistringstream iss01(str01);
  std::wistringstream iss02(str01);
  
  iss01 >> str04;
  VERIFY( str04.size() != str01.size() );
  VERIFY( str04 == L"santa" );

  iss02 >> std::ws;
  iss02 >> str05;
  VERIFY( str05.size() != str01.size() );
  VERIFY( str05 == L"santa" );
  VERIFY( str05 == str04 );

  iss01 >> str04;
  VERIFY( str04.size() != str01.size() );
  VERIFY( str04 == L"barbara" );
  
  iss02 >> std::ws;
  iss02 >> str05;
  VERIFY( str05.size() != str01.size() );
  VERIFY( str05 == L"barbara" );
  VERIFY( str05 == str04 );

  VERIFY( !iss01.fail() );
  VERIFY( !iss02.fail() );
  VERIFY( !iss01.eof() );
  VERIFY( !iss02.eof() );

  iss01 >> std::ws;
  VERIFY( !iss01.fail() );
  VERIFY( iss01.eof() );
}

int main()
{ 
  test01();
  return 0;
}
