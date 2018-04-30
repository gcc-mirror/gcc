// 1999-06-10 bkoz

// Copyright (C) 1994-2018 Free Software Foundation, Inc.
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
#include <algorithm> // for std::find
#include <testsuite_hooks.h>

void test01(void)
{
  typedef std::string::size_type csize_type;
  typedef std::string::const_reference cref;
  typedef std::string::reference ref;

  const char str_lit01[] = "ventura, california";
  const std::string str01(str_lit01);
  std::string str02("del mar, california");
  std::string str03(" and ");
  std::string str05;

  // string& replace(size_type pos, size_type n, const string& string)
  // string& replace(size_type pos1, size_type n1, const string& string,
  //                 size_type pos2, size_type n2)
  // string& replace(size_type pos, size_type n1, const char* s, size_type n2)
  // string& replace(size_type pos, size_type n1, const char* s)
  // string& replace(size_type pos, size_type n1, size_type n2, char c)
  // string& replace(iterator it1, iterator it2, const string& str)
  // string& replace(iterator it1, iterator it2, const chat* s, size_type n)
  // string& replace(iterator it1, iterator it2, const chat* s)
  // string& replace(iterator it1, iterator it2, size_type n, char c)
  // template<typename InputIter>
  //   string& replace(iterator it1, iterator it2, InputIter j1, InputIter j2)

  // with mods, from tstring.cc, from jason merrill, et. al.
  std::string X = "Hello";
  std::string x = X;

  char ch = x[0];
  VERIFY( ch == 'H' );

  std::string z = x.substr(2, 3);
  VERIFY( z == "llo" );

  x.replace(2, 2, "r");
  VERIFY( x == "Hero" );

  x = X;
  x.replace(0, 1, "j");
  VERIFY( x == "jello" );

  int ar[] = { 'H', 'e', 'l', 'l', 'o' };
  x.replace(std::find(x.begin(), x.end(), 'l'), 
	    std::find(x.rbegin(), x.rend(), 'l').base(), ar, 
	    ar + sizeof(ar) / sizeof(ar[0]));
  VERIFY( x == "jeHelloo" );
}

int main()
{ 
  test01();
  return 0;
}
