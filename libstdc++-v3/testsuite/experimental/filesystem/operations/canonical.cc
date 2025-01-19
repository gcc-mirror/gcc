// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::experimental::filesystem;

void
test01()
{
  std::error_code ec;
  auto p = __gnu_test::nonexistent_path();
  (void) canonical( p, ec );
  VERIFY( ec );

  p = fs::current_path();
  (void) canonical( p, ec );
  VERIFY( !ec );

  const auto root = fs::absolute("/");

  p = "/";
  p = canonical( p, ec );
  VERIFY( p == root );
  VERIFY( !ec );

  p = "/.";
  p = canonical( p, ec );
  VERIFY( p == root );
  VERIFY( !ec );

  p = "/..";
  p = canonical( p, ec );
  VERIFY( p == root );
  VERIFY( !ec );

  p = "/../.././.";
  p = canonical( p, ec );
  VERIFY( p == root );
  VERIFY( !ec );
}

void
test02()
{
#if __cpp_exceptions
  fs::path p = "rel", base = __gnu_test::nonexistent_path();
  fs::path e1, e2;
  try {
    (void) canonical(p, base);
  } catch (const fs::filesystem_error& e) {
    e1 = e.path1();
    e2 = e.path2();
  }
  VERIFY( e1 == p );
  VERIFY( e2 == base );
#endif
}

int
main()
{
  test01();
  test02();
}
