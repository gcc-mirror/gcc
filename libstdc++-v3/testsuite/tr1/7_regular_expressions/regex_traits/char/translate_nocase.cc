// { dg-do run }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

// tr1 [7.7] class template regex_traits (5) translate_nocase

#include <tr1/regex>
#include <testsuite_hooks.h>

// Tests default constructor of the regex_traits class.  There is only the
// default constructor.
void test01()
{
  std::tr1::regex_traits<char> t;
  VERIFY( t.translate_nocase('A') == 'a' );
}

int
main()
{
  test01();
  return 0;
}
