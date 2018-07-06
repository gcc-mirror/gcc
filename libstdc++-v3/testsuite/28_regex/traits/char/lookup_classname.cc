// { dg-do run { target c++11 } }

//
// 2010-06-23  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

int main()
{
	test01();
	test02();
	test03();
	return 0;
}
