// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::experimental::filesystem;

void
test01()
{
  std::error_code ec;
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  bool n;

  n = fs::remove("", ec);
  VERIFY( !ec ); // This seems odd, but is what the standard requires.
  VERIFY( !n );

  auto p = __gnu_test::nonexistent_path();
  ec = bad_ec;
  n = remove(p, ec);
  VERIFY( !ec );
  VERIFY( !n );

#ifndef NO_SYMLINKS
  auto link = __gnu_test::nonexistent_path();
  create_symlink(p, link);  // dangling symlink
  ec = bad_ec;
  n = remove(link, ec);
  VERIFY( !ec );
  VERIFY( n );
  VERIFY( !exists(symlink_status(link)) );

  __gnu_test::scoped_file f(p);
  create_symlink(p, link);
  ec = bad_ec;
  n = remove(link, ec);
  VERIFY( !ec );
  VERIFY( n );
  VERIFY( !exists(symlink_status(link)) );  // The symlink is removed, but
  VERIFY( exists(p) );                      // its target is not.

  ec = bad_ec;
  n = remove(p, ec);
  VERIFY( !ec );
  VERIFY( n );
  VERIFY( !exists(symlink_status(p)) );
#endif

  const auto dir = __gnu_test::nonexistent_path();
  create_directories(dir/"a/b");
  ec.clear();
  n = remove(dir/"a", ec);
  VERIFY( ec );
  VERIFY( !n );
  VERIFY( exists(dir/"a/b") );

  if (__gnu_test::permissions_are_testable())
  {
    permissions(dir, fs::perms::none, ec);
    if (!ec)
    {
      ec.clear();
      n = remove(dir/"a/b", ec);
      VERIFY( ec );
      VERIFY( !n );
      permissions(dir, fs::perms::owner_all, ec);
    }
  }

  ec = bad_ec;
  n = remove(dir/"a/b", ec);
  VERIFY( !ec );
  VERIFY( n );
  VERIFY( !exists(dir/"a/b") );

  remove(dir/"a", ec);
  remove(dir, ec);
}

int
main()
{
  test01();
}
