// { dg-do run { target c++14 } }

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

// basic_string_view constructors.

#include <experimental/string_view>
#include <string>
#include <cstring>
#include <testsuite_hooks.h>

void
test01()
{
  typedef std::experimental::string_view::size_type csize_type;

  // basic_string_view()
  const std::experimental::string_view str00{};
  VERIFY( str00.length() == 0 );
  VERIFY( str00.data() == nullptr );

  // basic_string_view(const char*)
  const char str_lit01[] = "rodeo beach, marin";
  const std::experimental::string_view str01{str_lit01};
  VERIFY( str01.length() == 18 );
  VERIFY( str01.data() == str_lit01 );
  const std::experimental::string_view str02{"baker beach, san francisco"};
  VERIFY( str02.length() == 26 );

  // basic_string_view(const string_view&)
  std::experimental::string_view str04{str01};
  VERIFY( str04.length() == str01.length() );
  VERIFY( str04.data() == str01.data() );

  // basic_string_view(const char* s)
  csize_type len_lit01 = strlen(str_lit01);
  std::experimental::string_view str05{str_lit01, len_lit01};
  VERIFY( str05.length() == len_lit01 );
  VERIFY( str05.data() == str_lit01 );

  // basic_string_view(basic_string& s)
  std::string istr07(10, 'z');
  std::experimental::string_view str07{istr07};
  VERIFY( str07.length() == 10 );
}

int
main()
{ 
  test01();

  return 0;
}
