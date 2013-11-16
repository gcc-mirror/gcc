// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013 Free Software Foundation, Inc.
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

// basic_string_view find

#include <experimental/string_view>
#include <testsuite_hooks.h>

bool
test01()
{
  bool test [[gnu::unused]] = true;

  typedef std::experimental::string_view::size_type csize_type;
  typedef std::experimental::string_view::const_reference cref;
  typedef std::experimental::string_view::reference ref;
  csize_type npos = std::experimental::string_view::npos;
  csize_type csz01, csz02;

  const char str_lit01[] = "mave";
  const std::experimental::string_view str01("mavericks, santa cruz");
  std::experimental::string_view str02(str_lit01);
  std::experimental::string_view str03("s, s");
  std::experimental::string_view str04;

  // size_type find(const string_view&, size_type pos = 0) const;
  csz01 = str01.find(str01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str01, 4);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str02, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str02, 3);
  VERIFY( csz01 == npos );
  csz01 = str01.find(str03, 0);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 3);
  VERIFY( csz01 == 8 );
  csz01 = str01.find(str03, 12);
  VERIFY( csz01 == npos );

  // An empty string_view consists of no characters
  // therefore it should be found at every point in a string_view,
  // except beyond the end
  csz01 = str01.find(str04, 0);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str04, 5);
  VERIFY( csz01 == 5 );
  csz01 = str01.find(str04, str01.size());
  VERIFY( csz01 == str01.size() ); 
  csz01 = str01.find(str04, str01.size()+1);
  VERIFY( csz01 == npos ); 
  
  // size_type find(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find(str_lit01, 0, 3);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3, 0);
  VERIFY( csz01 == 3 );

  // size_type find(const char* s, size_type pos = 0) const;
  csz01 = str01.find(str_lit01);
  VERIFY( csz01 == 0 );
  csz01 = str01.find(str_lit01, 3);
  VERIFY( csz01 == npos );

  // size_type find(char c, size_type pos = 0) const;
  csz01 = str01.find('z');
  csz02 = str01.size() - 1;
  VERIFY( csz01 == csz02 );
  csz01 = str01.find('/');
  VERIFY( csz01 == npos );

  return test;
}

int
main()
{ 
  test01();

  return 0;
}
