// { dg-do run }
// { dg-options "-std=c++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  typedef wchar_t CharT;
  typedef std::regex_traits<CharT> traits;

  wchar_t n1[] = L"lower";
  wchar_t n2[] = L"alpha";
  traits t;

#define range(s) s, s+sizeof(s)/sizeof(s[0])-1
  traits::char_class_type c1 = t.lookup_classname(range(n1));
  VERIFY( c1 != 0 );

  traits::char_class_type c2 = t.lookup_classname(range(n1), true);
  traits::char_class_type c3 = t.lookup_classname(range(n2), true);
  VERIFY( c2 == c3 );
#undef range
}

int main()
{
  test01();
  return 0;
}
