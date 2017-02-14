// { dg_do run }
// { dg-do run { target c++11 } }

//
// 2010-02-17  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2017 Free Software Foundation, Inc.
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
// 28.2 (4) Table 127 - Regular expression traits class requirements
// 28.7 Class template regex_traits [re.traits]
// 28.7 (3) static std::size_t length(const char_type* p);

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  typedef char CharT;

  std::regex_traits<CharT> t;
  CharT c = 'a';
  CharT d = 'a';

  VERIFY( t.translate(c) == t.translate(d) );
}

int main()
{
	test01();
	return 0;
}
