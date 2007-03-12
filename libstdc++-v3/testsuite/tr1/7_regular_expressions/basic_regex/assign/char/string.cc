// { dg-do link }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2007 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// tr1 [7.8.3] class template basic_regex assign()

#include <string>
#include <tr1/regex>
#include <testsuite_hooks.h>

// Tests C++ string assignment of the basic_regex class.  
void test01()
{
  typedef std::tr1::basic_regex<char> test_type;

  std::string s("a*b");
  test_type re;
  re.assign(s);
}

int
main()
{ 
  test01();
  return 0;
};
