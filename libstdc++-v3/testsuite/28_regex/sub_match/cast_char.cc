// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2010-06-09  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

// 28.9.1 [re.submatch.members] sub_match members

#include <regex>
#include <string>
#include <testsuite_hooks.h>

int main()
{
  typedef char                          value_type;
  typedef std::basic_string<value_type> string_type;
  typedef std::sub_match<value_type*>   sub_match_type;
  value_type test_data[] = "cabbage";

  sub_match_type sm;
  sm.first = std::begin(test_data);
  sm.second  = std::end(test_data) - 1;
  sm.matched = true;

  string_type sm_string = sm;

  VERIFY( sm_string == string_type(test_data) );
}
