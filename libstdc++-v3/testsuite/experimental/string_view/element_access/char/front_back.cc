// { dg-do run { target c++14 } }
// { dg-require-string-conversions "" }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

#include <experimental/string_view>
#include <testsuite_hooks.h>

void
test01()
{
  std::experimental::string_view str("ramifications");
  const std::experimental::string_view cstr("melodien");

  VERIFY( str.front() == 'r' );
  VERIFY( str.back() == 's' );
  VERIFY( cstr.front() == 'm' );
  VERIFY( cstr.back() == 'n' );
}

int
main()
{
  test01();

  return 0;
}
