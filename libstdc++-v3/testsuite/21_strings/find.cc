// 1999-06-09 bkoz

// Copyright (C) 1994, 1999 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 21.3.6.1 basic_string find

#include <string>
#include <stdexcept>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

bool test01(void)
{
  bool test = true;
  typedef std::string::size_type csize_type;
  typedef std::string::const_reference cref;
  typedef std::string::reference ref;
  csize_type npos = std::string::npos;
  csize_type csz01, csz02;

  const char str_lit01[] = "mave";
  const std::string str01("mavericks, santa cruz");
  std::string str02(str_lit01);
  std::string str03("s, s");
  std::string str04;

  // size_type find(const string&, size_type pos = 0) const;
  csz01 = str01.find(str01);
  test &= csz01 == 0;
  csz01 = str01.find(str01, 4);
  test &= csz01 == npos;
  csz01 = str01.find(str02, 0);
  test &= csz01 == 0;
  csz01 = str01.find(str02, 3);
  test &= csz01 == npos;
  csz01 = str01.find(str03, 0);
  test &= csz01 == 8;
  csz01 = str01.find(str03, 3);
  test &= csz01 == 8;
  csz01 = str01.find(str03, 12);
  test &= csz01 == npos;
  // It is implementation-defined if a given string contains an empty
  // string. The only two times a char_type() (== empty string) ending
  // element is required to be part of the string is on c_str() and
  // operator[size()] const: the indeterminate, stored state of the
  // string can vary, and not include a terminal char_type().
  csz01 = str01.find(str04, 0);
  test &= csz01 == npos || csz01 == str01.size();
  csz01 = str01.find(str04, 5);
  test &= csz01 == npos || csz01 == str01.size();
  
  // size_type find(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find(str_lit01, 0, 3);
  test &= csz01 == 0;
  csz01 = str01.find(str_lit01, 3, 0);
  test &= csz01 == npos;

  // size_type find(const char* s, size_type pos = 0) const;
  csz01 = str01.find(str_lit01);
  test &= csz01 == 0;
  csz01 = str01.find(str_lit01, 3);
  test &= csz01 == npos;

  // size_type find(char c, size_type pos = 0) const;
  csz01 = str01.find('z');
  csz02 = str01.size() - 1;
  test &= csz01 == csz02;
  csz01 = str01.find('/');
  test &= csz01 == npos;
   

  // size_type find_first_of(const string&, size_type pos = 0) const;
  std::string str05("xena rulez");
  csz01 = str01.find_first_of(str01);
  test &= csz01 == 0;
  csz01 = str01.find_first_of(str01, 4);
  test &= csz01 == 4;
  csz01 = str01.find_first_of(str02, 0);
  test &= csz01 == 0;
  csz01 = str01.find_first_of(str02, 3);
  test &= csz01 == 3;
  csz01 = str01.find_first_of(str03, 0);
  test &= csz01 == 8;
  csz01 = str01.find_first_of(str03, 3);
  test &= csz01 == 8;
  csz01 = str01.find_first_of(str03, 12);
  test &= csz01 == 16;
  csz01 = str01.find_first_of(str05, 0);
  test &= csz01 == 1;
  csz01 = str01.find_first_of(str05, 4);
  test &= csz01 == 4;

  // It is implementation-defined if a given string contains an empty
  // string. The only two times a char_type() (== empty string) ending
  // element is required to be part of the string is on c_str() and
  // operator[size()] const: the indeterminate, stored state of the
  // string can vary, and not include a terminal char_type().
  csz01 = str01.find_first_of(str04, 0);
  test &= csz01 == npos || csz01 == str01.size();
  csz01 = str01.find_first_of(str04, 5);
  test &= csz01 == npos || csz01 == str01.size();
  
  // size_type find_first_of(const char* s, size_type pos, size_type n) const;
  csz01 = str01.find_first_of(str_lit01, 0, 3);
  test &= csz01 == 0;
  csz01 = str01.find_first_of(str_lit01, 3, 0);
  test &= csz01 == npos;

  // size_type find_first_of(const char* s, size_type pos = 0) const;
  csz01 = str01.find_first_of(str_lit01);
  test &= csz01 == 0;
  csz01 = str01.find_first_of(str_lit01, 3);
  test &= csz01 == 3;

  // size_type find_first_of(char c, size_type pos = 0) const;
  csz01 = str01.find_first_of('z');
  csz02 = str01.size() - 1;
  test &= csz01 == csz02;

  // size_type find_last_of(const string& str, size_type pos = 0) const;
  // size_type find_last_of(const char* s, size_type pos, size_type n) const;
  // size_type find_last_of(const char* s, size_type pos = 0) const;
  // size_type find_last_of(char c, size_type pos = 0) const;

#if 1
// from tstring.cc, from jason merrill, et. al.
  std::string x;
  std::string::size_type pos;
  pos = x.find_last_not_of('X');
  test &= pos == npos;
  pos = x.find_last_not_of("XYZ");
  test &= pos == npos;

  std::string y("a");
  pos = y.find_last_not_of('X');
  test &= pos == 0;
  pos = y.find_last_not_of('a');
  test &= pos == npos;
  pos = y.find_last_not_of("XYZ");
  test &= pos == 0;
  pos = y.find_last_not_of("a");
  test &= pos == npos;

  std::string z("ab");
  pos = z.find_last_not_of('X');
  test &= pos == 1;
  pos = z.find_last_not_of("XYZ");
  test &= pos == 1;
  pos = z.find_last_not_of('b');
  test &= pos == 0;
  pos = z.find_last_not_of("Xb");
  test &= pos == 0;
  pos = z.find_last_not_of("Xa");
  test &= pos == 1;
  pos = z.find_last_of("ab");
  test &= pos == 1;
  pos = z.find_last_of("Xa");
  test &= pos == 0;
  pos = z.find_last_of("Xb");
  test &= pos == 1;
  pos = z.find_last_of("XYZ");
  test &= pos == std::string::npos;
  pos = z.find_last_of('a');
  test &= pos == 0;
  pos = z.find_last_of('b');
  test &= pos == 1;
  pos = z.find_last_of('X');
  test &= pos == std::string::npos;
#endif

#ifdef DEBUG_ASSERT
  assert(test);
#endif
  return test;
}

int main()
{ 
  test01();
}





