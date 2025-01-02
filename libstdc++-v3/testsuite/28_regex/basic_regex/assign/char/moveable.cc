// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// 2010-07-07  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

// [28.8.3](9-11) class template basic_regex assign

#include <regex>
#include <testsuite_hooks.h>
#include <utility>

// Tests assign operator of the basic_regex class for moveable rvalues.  
void test01()
{
  std::regex src_re("aaba");
  const unsigned mark_count = src_re.mark_count();
  const std::regex::flag_type flags = src_re.flags();

  std::regex target_re;
  
  target_re.assign(std::move(src_re));
  
  VERIFY( target_re.flags() == flags );
  VERIFY( target_re.mark_count() == mark_count );
}

int
main()
{ 
  test01();
  return 0;
}
