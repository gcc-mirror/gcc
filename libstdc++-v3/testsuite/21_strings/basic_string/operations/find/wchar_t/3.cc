// 2003-05-04  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

// 21.3.6.5 basic_string find_first_not_of

#include <string>
#include <testsuite_hooks.h>

void test03(void)
{
  typedef std::wstring::size_type csize_type;
  csize_type npos = std::wstring::npos;
  csize_type csz01;

  const std::wstring str01(L"Bob Rock, per me");
  const wchar_t str_lit01[] = L"Bob Rock";
  std::wstring str02(L"ovvero Trivi");
  std::wstring str03(str_lit01);
  std::wstring str04;

  // size_type find_first_not_of(const string&, size_type pos = 0) const;
  csz01 = str01.find_first_not_of(str01);
  VERIFY( csz01 == npos );
  csz01 = str01.find_first_not_of(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_not_of(str02, 10);
  VERIFY( csz01 == 10 );
  csz01 = str01.find_first_not_of(str02, 12);
  VERIFY( csz01 == 14 );
  csz01 = str01.find_first_not_of(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find_first_not_of(str03, 15);
  VERIFY( csz01 == 15 );
  csz01 = str01.find_first_not_of(str03, 16);
  VERIFY( csz01 == npos );
  csz01 = str01.find_first_not_of(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_not_of(str04, 12);
  VERIFY( csz01 == 12 );
  csz01 = str03.find_first_not_of(str01, 0);
  VERIFY( csz01 == npos );
  csz01 = str04.find_first_not_of(str02, 0);
  VERIFY( csz01 == npos );

  // size_type find_first_not_of(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find_first_not_of(str_lit01, 0, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find_first_not_of(str_lit01, 0, 8);
  VERIFY( csz01 == 8 );
  csz01 = str01.find_first_not_of(str_lit01, 10, 0);
  VERIFY( csz01 == 10 );

  // size_type find_first_not_of(const char* s, size_type pos = 0) const;
  csz01 = str01.find_first_not_of(str_lit01);
  VERIFY( csz01 == 8 );
  csz01 = str02.find_first_not_of(str_lit01, 2);
  VERIFY( csz01 == 2 );

  // size_type find_first_not_of(char c, size_type pos = 0) const;
  csz01 = str01.find_first_not_of(L'B');
  VERIFY( csz01 == 1 );
  csz01 = str01.find_first_not_of(L'o', 1);
  VERIFY( csz01 == 2 );
  csz01 = str02.find_first_not_of(L'z');
  VERIFY( csz01 == 0 );
  csz01 = str04.find_first_not_of(L'S');
  VERIFY( csz01 == npos );
}

int main()
{ 
  test03();
  return 0;
}
