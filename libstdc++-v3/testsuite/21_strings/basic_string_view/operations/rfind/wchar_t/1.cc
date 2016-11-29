// { dg-options "-std=gnu++17" }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

#include <string_view>
#include <testsuite_hooks.h>

// basic_string_view rfind

void
test01()
{
  typedef std::wstring_view::size_type csize_type;
  typedef std::wstring_view::const_reference cref;
  typedef std::wstring_view::reference ref;
  csize_type npos = std::wstring_view::npos;
  csize_type csz01, csz02;

  const wchar_t str_lit01[] = L"mave";
  const std::wstring_view str01(L"mavericks, santa cruz");
  std::wstring_view str02(str_lit01);
  std::wstring_view str03(L"s, s");
  std::wstring_view str04;

  // size_type rfind(const wstring_view&, size_type pos = 0) const;
  csz01 = str01.rfind(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str01, 4);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str02,3);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str02);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str03);
  VERIFY( csz01 == 8 );
  csz01 = str01.rfind(str03, 3);
  VERIFY( csz01 == npos );
  csz01 = str01.rfind(str03, 12);
  VERIFY( csz01 == 8 );

  // An empty string_view consists of no characters
  // therefore it should be found at every point in a string_view,
  // except beyond the end
  csz01 = str01.rfind(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str04, 5);
  VERIFY( csz01 == 5 );
  csz01 = str01.rfind(str04, str01.size());
  VERIFY( csz01 == str01.size() );
  csz01 = str01.rfind(str04, str01.size()+1);
  VERIFY( csz01 == str01.size() );

  // size_type rfind(const wchar_t* s, size_type pos, size_type n) const;
  csz01 = str01.rfind(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str_lit01, 3, 0);
  VERIFY( csz01 == 3 );

  // size_type rfind(const wchar_t* s, size_type pos = 0) const;
  csz01 = str01.rfind(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.rfind(str_lit01, 3);
  VERIFY( csz01 == 0 );

  // size_type rfind(wchar_t c, size_type pos = 0) const;
  csz01 = str01.rfind(L'z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );
  csz01 = str01.rfind(L'/');
  VERIFY( csz01 == npos );
}

int
main()
{
  test01();

  return 0;
}
