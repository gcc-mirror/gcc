// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// string_view operations

#include <experimental/string_view>
#include <testsuite_hooks.h>

int
test01()
{
  bool test [[gnu::unused]] = true;

  std::experimental::string_view empty;

  VERIFY( empty.size() == 0 );
  const std::experimental::string_view::value_type* p = empty.data();
  VERIFY( p == nullptr );

  return 0;
}

int
main()
{ 
  test01();

  return 0;
}
