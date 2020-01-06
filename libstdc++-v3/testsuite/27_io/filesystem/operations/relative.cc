// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  auto p = __gnu_test::nonexistent_path();
  auto q = __gnu_test::nonexistent_path();

  auto r = relative(p, q);
  VERIFY( r == ".."/p );

  r = relative(p, p/q);
  VERIFY( r == ".." );

  r = relative(p/q, p);
  VERIFY( r == q );

  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  ec = bad_ec;
  r = relative(p, q, ec);
  VERIFY( !ec );
  VERIFY( r == ".."/p );

  ec = bad_ec;
  r = relative(p, p/q, ec);
  VERIFY( !ec );
  VERIFY( r == ".." );

  ec = bad_ec;
  r = relative(p/q, p, ec);
  VERIFY( !ec );
  VERIFY( r == q );
}

int
main()
{
  test01();
}
