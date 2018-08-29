// { dg-do run { target c++11 } }

// 2010-07-07  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// [28.8.2](12-14) class template basic_regex constructors

#include <regex>
#include <testsuite_hooks.h>
#include <utility>

// Tests move constructor of the basic_regex class.  
void test01()
{
  typedef std::basic_regex<char> test_type;

  test_type src_re("aaba");
  const unsigned mark_count = src_re.mark_count();
  const test_type::flag_type flags = src_re.flags();

  test_type target_re = std::move(src_re);
  
  VERIFY( target_re.flags() == flags );
  VERIFY( target_re.mark_count() == mark_count );
}

int
main()
{ 
  test01();
  return 0;
}
