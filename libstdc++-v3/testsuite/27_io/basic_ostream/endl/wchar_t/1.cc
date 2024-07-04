// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 27.6.2.7 standard basic_ostream manipulators

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

void test01(void)
{
  const std::wstring str01(L" santa barbara ");
  std::wstring str04;
  std::wstring str05;

  std::wostringstream oss01(str01);
  std::wostringstream oss02;
  typedef std::wostringstream::traits_type traits_type;

  // template<_CharT, _Traits>
  //  basic_ostream<_CharT, _Traits>& ends(basic_ostream<_Char, _Traits>& os)
  oss01 << std::endl;
  str04 = oss01.str();
  VERIFY( str04.size() == str01.size() );

  oss02 << std::endl;
  str05 = oss02.str();
  VERIFY( str05.size() == 1 );
}

int main()
{ 
  test01();
  return 0;
}
