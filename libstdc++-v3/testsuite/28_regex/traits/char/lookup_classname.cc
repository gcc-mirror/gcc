// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2010-06-23  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.3 Requirements [re.req]
// 28.2(4) Table 127 - Regular expression traits class requirements
// 28.7(9) Class template regex_traits [re.traits]

#include <regex>
#include <forward_list>
#include <string.h>
#include <ctype.h>
#include <testsuite_hooks.h>

void
test01()
{
  typedef char CharT;
  typedef std::regex_traits<CharT> traits;

  char n1[] = "lower";
  char n2[] = "alpha";
  traits t;

  traits::char_class_type c1 = t.lookup_classname(n1, n1+sizeof(n1)-1);
  VERIFY( c1 != 0 );

  traits::char_class_type c2 = t.lookup_classname(n1, n1+sizeof(n1)-1, true);
  traits::char_class_type c3 = t.lookup_classname(n2, n2+sizeof(n2)-1, true);
  VERIFY( c2 == c3 );
}

// Test forward iterator
void
test02()
{
  const char strlit[] = "upper";
  std::forward_list<char> s(strlit, strlit + strlen(strlit));
  std::regex_traits<char> traits;
  VERIFY(traits.isctype('C', traits.lookup_classname(s.begin(), s.end(), false)));
}

// icase
void
test03()
{
  std::string s("lower");
  std::regex_traits<char> traits;
  VERIFY(traits.isctype('C', traits.lookup_classname(s.begin(), s.end(), true)));
}

void
test04()
{
  // Transform "string" into "StRiNg"
  auto mix_case = [](std::string s) {
    int i = 0;
    for (auto& ch : s)
      if (++i % 2)
	ch = toupper(ch);
    return s;
  };

  typedef char CharT;
  typedef std::regex_traits<CharT> traits;
  traits t;

  auto lookup = [&t](const std::string& s, bool icase) {
    return t.lookup_classname(s.begin(), s.end(), icase);
  };

  VERIFY( lookup("", false) == 0 );
  VERIFY( lookup(":::not a valid classname:::", false) == 0 );
  VERIFY( lookup(":::not a valid classname:::", true) == 0 );
  VERIFY( lookup("alnu", false) == 0 );
  VERIFY( lookup("alnumb", false) == 0 );
  VERIFY( lookup("x", false) == 0 );
  VERIFY( lookup("di", false) == 0 );
  VERIFY( lookup("digi", false) == 0 );
  VERIFY( lookup("digix", false) == 0 );
  VERIFY( lookup(std::string{"d\0i", 3}, false) == 0 );
  VERIFY( lookup(std::string{"digit\0", 6}, false) == 0 );
  VERIFY( lookup(std::string{"digit\0bad", 9}, false) == 0 );

  for (std::string cls : { "alnum", "alpha", "blank", "cntrl", "digit",
			   "graph", "lower", "print", "punct", "space",
			   "upper", "xdigit", "d", "s", "w" })
    {
      traits::char_class_type val = lookup(cls, false);
      // val should be non-zero:
      VERIFY( val != 0 );
      // val is independent of the case of the name,
      // i.e. "alpha" and "ALPHA" and "AlPhA" give same result:
      VERIFY( lookup(mix_case(cls), false) == val );

      // Repeat same checks for icase=true.
      traits::char_class_type ival = lookup(cls, true);
      VERIFY( ival != 0 );
      VERIFY( lookup(mix_case(cls), true) == ival );

      if (cls == "lower" || cls == "upper") // icase=true should affect value
	{
	  VERIFY( ival != val );
	  VERIFY( ival == lookup("alpha", false) );
	}
      else
	VERIFY( ival == val );

      if (cls == "d")
	VERIFY( val == lookup("digit", false) );
      else if (cls == "s")
	VERIFY( val == lookup("space", false) );
    }
}

int main()
{
	test01();
	test02();
	test03();
	test04();
	return 0;
}
