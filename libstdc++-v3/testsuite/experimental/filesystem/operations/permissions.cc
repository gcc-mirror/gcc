// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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
// { dg-xfail-if "permissions not supported" { *-*-mingw* } }

// 15.26 Permissions [fs.op.permissions]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  using perms = std::experimental::filesystem::perms;

  auto p = __gnu_test::nonexistent_path();

  __gnu_test::scoped_file f(p);
  VERIFY( exists(p) );
  permissions(p, perms::owner_all);
  VERIFY( status(p).permissions() == perms::owner_all );
  permissions(p, perms::group_read | perms::add_perms);
  VERIFY( status(p).permissions() == (perms::owner_all | perms::group_read) );
  permissions(p, perms::group_read | perms::remove_perms);
  VERIFY( status(p).permissions() == perms::owner_all );
}

void
test02()
{
  using perms = std::experimental::filesystem::perms;

  auto p = __gnu_test::nonexistent_path();

  std::error_code ec;
  permissions(p, perms::owner_all, ec);
  VERIFY( ec );

  __gnu_test::scoped_file f(p);
  VERIFY( exists(p) );

  ec = std::make_error_code(std::errc::invalid_argument);
  permissions(p, perms::owner_all, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == perms::owner_all );
  permissions(p, perms::group_read | perms::add_perms, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == (perms::owner_all | perms::group_read) );
  permissions(p, perms::group_read | perms::remove_perms, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == perms::owner_all );
}

void
test03()
{
  using perms = std::experimental::filesystem::perms;

  __gnu_test::scoped_file f;
  VERIFY( exists(f.path) );

  auto p = __gnu_test::nonexistent_path();
  create_symlink(f.path, p);

  std::error_code ec, ec2;
  permissions(p, perms::owner_all | perms::symlink_nofollow, ec);
  try
  {
    permissions(p, perms::owner_all | perms::symlink_nofollow);
  }
  catch (const std::experimental::filesystem::filesystem_error& ex)
  {
    ec2 = ex.code();
    VERIFY( ex.path1() == p );
  }
  // Both calls should succeed, or both should fail with same error:
  VERIFY( ec == ec2 );

  remove(p);
}

void
test04()
{
  using perms = std::experimental::filesystem::perms;

  auto p = __gnu_test::nonexistent_path();
  create_symlink(__gnu_test::nonexistent_path(), p);

  std::error_code ec, ec2;
  permissions(p, perms::owner_all, ec);
  VERIFY( ec );
  try
  {
    permissions(p, perms::owner_all);
  }
  catch (const std::experimental::filesystem::filesystem_error& ex)
  {
    ec2 = ex.code();
    VERIFY( ex.path1() == p );
  }
  VERIFY( ec == ec2 );

  remove(p);
}

void
test05()
{
  using perms = std::experimental::filesystem::perms;
  std::error_code ec;

  __gnu_test::scoped_file f;
  auto p = perms::owner_write;

  // symlink_nofollow should not give an error for non-symlinks
  permissions(f.path, p|perms::symlink_nofollow, ec);
  VERIFY( !ec );
  auto st = status(f.path);
  VERIFY( st.permissions() == p );
  p |= perms::owner_read;
  permissions(f.path, p|perms::symlink_nofollow, ec);
  st = status(f.path);
  VERIFY( st.permissions() == p );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
