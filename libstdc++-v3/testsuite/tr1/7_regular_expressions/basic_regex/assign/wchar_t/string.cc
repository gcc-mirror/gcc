// { dg-do compile }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2007-2016 Free Software Foundation, Inc.
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

// tr1 [7.8.3] class template basic_regex assign()

#include <string>
#include <tr1/regex>
#include <testsuite_hooks.h>

// Tests C++ string assignment of the basic_regex class.
void test01()
{
  typedef std::tr1::basic_regex<wchar_t> test_type;

  std::wstring s(L"a*b");
  test_type re;
  re.assign(s);
}

int
main()
{
  test01();
  return 0;
}
