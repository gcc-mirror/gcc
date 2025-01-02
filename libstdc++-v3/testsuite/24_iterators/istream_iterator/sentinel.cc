// { dg-do run { target c++20 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <iterator>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  static_assert( std::sentinel_for<std::default_sentinel_t,
				   std::istream_iterator<std::string>> );
  static_assert( std::sentinel_for<std::default_sentinel_t,
				   std::istream_iterator<int>> );

  std::istream_iterator<int> i = std::default_sentinel;
  VERIFY( i == std::default_sentinel );
  VERIFY( std::default_sentinel == i );
}

void
test02()
{
  std::istringstream in("1 2 3");
  std::istream_iterator<int> iter(in);
  VERIFY( iter != std::default_sentinel );
  VERIFY( std::default_sentinel != iter );

  (void) *iter++;
  (void) *iter++;
  (void) *iter++;
  VERIFY( iter == std::default_sentinel );
  VERIFY( std::default_sentinel == iter );
}

int main()
{
  test01();
  test02();
}
