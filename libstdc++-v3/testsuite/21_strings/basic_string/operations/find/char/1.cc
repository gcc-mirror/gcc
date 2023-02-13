// 1999-06-09 bkoz

// Copyright (C) 1994-2023 Free Software Foundation, Inc.
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

// 21.3.6.1 basic_string find

#include <testsuite_string.h>
#include <testsuite_hooks.h>

void test01(void)
{
  typedef __gnu_test::string::size_type csize_type;
  typedef __gnu_test::string::const_reference cref;
  typedef __gnu_test::string::reference ref;
  csize_type npos = __gnu_test::string::npos;
  csize_type csz01, csz02;

  const char str_lit01[] = "mave";
  const __gnu_test::string str01("mavericks, santa cruz");
  __gnu_test::string str02(str_lit01);
  __gnu_test::string str03("s, s");
  __gnu_test::string str04;

  // size_type find(const string&, size_type pos = 0) const;
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

  // An empty string consists of no characters
  // therefore it should be found at every point in a string,
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
}

int main()
{ 
  test01();
  return 0;
}
