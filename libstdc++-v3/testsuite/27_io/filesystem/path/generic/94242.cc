// { dg-do run { target c++17 } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// C++17 30.10.7.4.7 path generic format observers [fs.path.generic.obs]

#include <filesystem>
#include <testsuite_allocator.h>

using std::filesystem::path;
using __gnu_test::SimpleAllocator;

void
test01()
{
  path p = "//foo//bar//.";
  using C = path::value_type;
  auto g = p.generic_string<C, std::char_traits<C>, SimpleAllocator<C>>();
  VERIFY( g == path("/foo/bar/.").c_str() );
}

void
test02()
{
  path p = "//foo//bar//.";
  using C = char16_t;
  auto g = p.generic_string<C, std::char_traits<C>, SimpleAllocator<C>>();
  VERIFY( g == u"/foo/bar/." );
}

int
main()
{
  test01();
  test02();
}
