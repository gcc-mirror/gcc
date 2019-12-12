// 2004-01-26  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  std::wstring str01(L"Valle Del Salto");
  str01.replace(0, 5, str01.data() + 10, 5);
  VERIFY( str01 == L"Salto Del Salto" );
  
  std::wstring str02(L"Colle di Val d'Elsa");
  str02.replace(0, 9, str02.data() + 10, 0);
  VERIFY( str02 == L"Val d'Elsa" );

  std::wstring str03(L"Novi Ligure");
  str03.replace(11, 0, str03.data() + 4, 7);
  VERIFY( str03 == L"Novi Ligure Ligure");

  std::wstring str04(L"Trebisacce");
  str04.replace(3, 4, str04.data(), 0);
  VERIFY( str04 == L"Trecce" );

  std::wstring str05(L"Altopiano della Sila");
  str05.replace(1, 18, str05.data() + 19, 1);
  VERIFY( str05 == L"Aaa" );
}

int main()
{
  test01();
  return 0;
}
