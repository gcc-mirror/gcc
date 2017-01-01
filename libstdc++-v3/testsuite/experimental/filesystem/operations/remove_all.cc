// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
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
  std::uintmax_t n;

  n = fs::remove_all("", ec);
  VERIFY( ec );
  VERIFY( n == std::uintmax_t(-1) );

  auto p = __gnu_test::nonexistent_path();
  ec.clear();
  n = remove_all(p, ec);
  VERIFY( ec );
  VERIFY( n == std::uintmax_t(-1) );

  const auto bad_ec = ec;
  auto link = __gnu_test::nonexistent_path();
  create_symlink(p, link);  // dangling symlink
  ec = bad_ec;
  n = remove_all(link, ec);
  VERIFY( !ec );
  VERIFY( n == 1 );
  VERIFY( !exists(symlink_status(link)) ); // DR 2721

  __gnu_test::scoped_file f(p);
  create_symlink(p, link);
  ec = bad_ec;
  n = remove_all(link, ec);
  VERIFY( !ec );
  VERIFY( n == 1 );
  VERIFY( !exists(symlink_status(link)) );  // The symlink is removed, but
  VERIFY( exists(p) );                      // its target is not.

  auto dir = __gnu_test::nonexistent_path();
  create_directories(dir/"a/b/c");
  ec = bad_ec;
  n = remove_all(dir/"a", ec);
  VERIFY( !ec );
  VERIFY( n == 3 );
  VERIFY( exists(dir) );
  VERIFY( !exists(dir/"a") );

  create_directories(dir/"a/b/c");
  __gnu_test::scoped_file a1(dir/"a/1");
  __gnu_test::scoped_file a2(dir/"a/2");
  __gnu_test::scoped_file b1(dir/"a/b/1");
  __gnu_test::scoped_file b2(dir/"a/b/2");
  ec = bad_ec;
  n = remove_all(dir, ec);
  VERIFY( !ec );
  VERIFY( n == 8 );
  VERIFY( !exists(dir) );

  a1.path.clear();
  a2.path.clear();
  b1.path.clear();
  b2.path.clear();
}

int
main()
{
  test01();
}
