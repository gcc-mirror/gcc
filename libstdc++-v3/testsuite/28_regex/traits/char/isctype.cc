// { dg-do run }
// { dg-options "-std=c++0x" }

//
// 2010-06-23  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
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
// 28.7(11) Class template regex_traits [re.traits]

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  typedef char CharT;
  typedef std::regex_traits<CharT> traits;

  const CharT lower[]   = "lOWer";
  const CharT upper[]   = "UPPER";
  const CharT nothing[] = "nothing";
  const CharT word[]    = "w";
  const CharT blank[]   = "blank";
  const CharT digit[]   = "digit";
  traits t;

#define range(s) s, s+sizeof(s)/sizeof(s[0])-1
  VERIFY( t.isctype('_', t.lookup_classname(range(word))));
  VERIFY( t.isctype('A', t.lookup_classname(range(word))));
  VERIFY(!t.isctype('~', t.lookup_classname(range(word))));
  VERIFY(!t.isctype('e', t.lookup_classname(range(upper))));
  VERIFY( t.isctype('e', t.lookup_classname(range(lower))));
  VERIFY(!t.isctype('e', t.lookup_classname(range(nothing))));
  VERIFY(!t.isctype('_', t.lookup_classname(range(digit))));
  VERIFY( t.isctype(' ', t.lookup_classname(range(blank))));
  VERIFY( t.isctype('\t', t.lookup_classname(range(blank))));
  VERIFY(!t.isctype('\n', t.lookup_classname(range(blank))));
  VERIFY( t.isctype('t', t.lookup_classname(range(upper), true)));
  VERIFY( t.isctype('T', t.lookup_classname(range(lower), true)));
#undef range
}

int main()
{
	test01();
	return 0;
}
