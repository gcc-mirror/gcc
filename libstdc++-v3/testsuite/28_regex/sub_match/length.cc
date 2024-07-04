// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2010-06-09  Stephen M. Webb <stephen.webb@bregmasoft.ca>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>


void
test01()
{
  typedef std::sub_match<const char*> sm_t;
  const char*           test_data = "cabbage";
  sm_t::difference_type test_len = 3;

  sm_t sm1;
  sm1.first   = test_data + 0;
  sm1.second  = test_data + test_len;
  sm1.matched = true;

  sm_t sm2;
  sm2.matched = false;

  VERIFY( sm1.length() == test_len );
  VERIFY( sm2.length() == 0 );
}

int main()
{
  test01();
}
