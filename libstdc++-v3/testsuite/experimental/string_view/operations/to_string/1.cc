// { dg-do run { target c++14 } }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// basic_string_view::to_string

#include <experimental/string_view>
#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test01()
{
  const char str_lit[] = "123456789A";
  const std::experimental::string_view sv(str_lit);
  char buffer[4] = { 0 };

  auto s1 = sv.to_string();
  VERIFY( s1 == str_lit );
  using test_alloc = __gnu_test::tracker_allocator<char>;
  auto s2 = sv.to_string( test_alloc{} );
  static_assert( std::is_same<decltype(s2)::allocator_type, test_alloc>::value,
                 "to_string() uses custom allocator" );
  VERIFY( std::equal(s1.begin(), s1.end(), s2.begin(), s2.end()) );
  auto s3 = static_cast<std::string>(sv);
  VERIFY( s3 == s1 );
}

int
main()
{
  test01();
}
