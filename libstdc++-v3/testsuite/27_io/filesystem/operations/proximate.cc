// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>

using std::filesystem::proximate;

void
test01()
{
  VERIFY( proximate("/a/d", "/a/b/c") == "../../d" );
  VERIFY( proximate("/a/b/c", "/a/d") == "../b/c" );
  VERIFY( proximate("a/b/c", "a") == "b/c" );
  VERIFY( proximate("a/b/c", "a/b/c/x/y") == "../.." );
  VERIFY( proximate("a/b/c", "a/b/c") == "." );
  VERIFY( proximate("a/b", "c/d") == "../../a/b" );
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec = bad_ec;
  VERIFY( proximate("/a/d", "/a/b/c", ec) == "../../d" );
  VERIFY( !ec );
  ec = bad_ec;
  VERIFY( proximate("/a/b/c", "/a/d", ec) == "../b/c" );
  VERIFY( !ec );
  ec = bad_ec;
  VERIFY( proximate("a/b/c", "a", ec) == "b/c" );
  VERIFY( !ec );
  ec = bad_ec;
  VERIFY( proximate("a/b/c", "a/b/c/x/y", ec) == "../.." );
  VERIFY( !ec );
  ec = bad_ec;
  VERIFY( proximate("a/b/c", "a/b/c", ec) == "." );
  VERIFY( !ec );
  ec = bad_ec;
  VERIFY( proximate("a/b", "c/d", ec) == "../../a/b" );
  VERIFY( !ec );
}

int
main()
{
  test01();
  test02();
}
