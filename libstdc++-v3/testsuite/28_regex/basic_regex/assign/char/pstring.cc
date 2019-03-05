// { dg-do compile { target c++11 } }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

// [28.8.3] class template basic_regex assign()

#include <regex>

// Tests assign operation from a Pascal-style counted-string.  
void test01()
{
  typedef std::basic_regex<char> test_type;

  const char cs[] = "aab";
  test_type re;
  re.assign(cs, sizeof(cs)-1, std::regex_constants::basic);
}

int
main()
{ 
  test01();
  return 0;
}
