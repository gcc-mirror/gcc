// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  using C = char;
  using string_type = std::basic_string<C>;
  using view_type = std::basic_string_view<C>;

  std::allocator<C> alloc;
  VERIFY( string_type(view_type("string")) == "string" );
  VERIFY( string_type(view_type("string"), alloc) == "string" );

  // LWG 2742
  VERIFY( string_type("substring", 3, 6) == "string" );
  VERIFY( string_type("substring", 3, 6, alloc) == "string" );
  VERIFY( string_type(view_type("substring"), 3, 6) == "string" );
  VERIFY( string_type(view_type("substring"), 3, 6, alloc) == "string" );
}

int
main()
{
  test01();
}
