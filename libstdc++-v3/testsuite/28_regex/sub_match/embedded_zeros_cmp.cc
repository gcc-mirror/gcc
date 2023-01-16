// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2014-11-13  Daniel Kruegler <daniel.kruegler@gmail.com>
//
// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// 28.9.2 [re.submatch.op] sub_match members, [DR 2217]

#include <regex>
#include <string>
#include <testsuite_hooks.h>

int main()
{
  typedef char                          value_type;
  typedef std::basic_string<value_type> string_type;
  typedef std::sub_match<value_type*>   sub_match_type;
  const string_type test_data1("abc\0d", 4);
  value_type test_data2[] = {'a', 'b', 'c'};
  const string_type test_data3("abc");

  sub_match_type sm;
  sm.first = std::begin(test_data2);
  sm.second  = std::end(test_data2);
  sm.matched = true;

  VERIFY( test_data1 != sm );
  VERIFY( sm != test_data1 );
  VERIFY( sm < test_data1 );
  VERIFY( !(test_data1 < sm) );
  VERIFY( test_data1 > sm );

  VERIFY( test_data3 == sm );
  VERIFY( sm == test_data3 );
  VERIFY( !(sm < test_data3) );
  VERIFY( !(test_data3 < sm) );
}
