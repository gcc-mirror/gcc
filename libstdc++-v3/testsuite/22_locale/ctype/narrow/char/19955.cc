// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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


// 22.2.1.3.2 ctype<char> members

#include <locale>
#include <cstring>
#include <testsuite_hooks.h>

class Ctype1
: public std::ctype<char> 
{
protected:
  const char*
  do_narrow(const char* lo, const char* hi,
	    char, char* to) const 
  {
    for (int i = 0; lo != hi; ++lo, ++to, ++i)
      *to = *lo + i;
    return hi;
  }
};

class Ctype2
: public std::ctype<char> 
{
protected:
  const char*
  do_narrow(const char* lo, const char* hi,
	    char dflt, char* to) const 
  {
    for (int i = 0; lo != hi; ++lo, ++to, ++i)
      if (*lo == '\000')
	*to = dflt;
      else
	*to = *lo;
    return hi;
  }
};

// libstdc++/19955
void test01() 
{
  using namespace std;

  const char src[] = "abcd";

  locale mylocale1(locale::classic(), new Ctype1);
  const ctype<char>& mc1 = use_facet<ctype<char> >(mylocale1);

  char dst1[sizeof(src)];
  memset(dst1, 0, sizeof(src));
  char dst2[sizeof(src)];
  memset(dst2, 0, sizeof(src));
  
  mc1.narrow(src, src + sizeof(src), '*', dst1);
  mc1.narrow(src, src + sizeof(src), '*', dst2);

  VERIFY( !memcmp(dst1, "aceg\004", 5) );
  VERIFY( !memcmp(dst1, dst2, 5) );

  locale mylocale2(locale::classic(), new Ctype2);
  const ctype<char>& mc2 = use_facet<ctype<char> >(mylocale2);

  char dst3[sizeof(src)];
  memset(dst3, 0, sizeof(src));
  char dst4[sizeof(src)];
  memset(dst4, 0, sizeof(src));
  
  mc2.narrow(src, src + sizeof(src), '*', dst3);
  mc2.narrow(src, src + sizeof(src), '*', dst4);

  VERIFY( !memcmp(dst3, "abcd*", 5) );
  VERIFY( !memcmp(dst3, dst4, 5) );
}

int main()
{
  test01();
  return 0;
}
