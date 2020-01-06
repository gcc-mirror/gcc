// { dg-do run { target c++11 } }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

// [28.8.2] class template basic_regex constructor

#include <regex>
#include <testsuite_hooks.h>

// Tests Pascal-style counted-string constructor of the basic_regex class.  
void test01()
{
  const char* cs = "aab";
	std::regex re(cs, 3, std::regex::basic);

  VERIFY( re.flags() & std::regex_constants::basic );
}

int
main()
{ 
  test01();
  return 0;
}
