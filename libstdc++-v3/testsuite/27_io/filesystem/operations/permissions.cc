// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
// { dg-require-filesystem-ts "" }
// { dg-xfail-if "permissions not supported" { *-*-mingw* } }

// C++17 30.10.14.26 Permissions [fs.op.permissions]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  using std::filesystem::perms;
  using std::filesystem::perm_options;

  auto p = __gnu_test::nonexistent_path();

  __gnu_test::scoped_file f(p);
  VERIFY( exists(p) );
  permissions(p, perms::owner_all);
  VERIFY( status(p).permissions() == perms::owner_all );
  permissions(p, perms::group_read, perm_options::add);
  VERIFY( status(p).permissions() == (perms::owner_all | perms::group_read) );
  permissions(p, perms::group_read, perm_options::remove);
  VERIFY( status(p).permissions() == perms::owner_all );
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  using std::filesystem::perms;
  using std::filesystem::perm_options;

  auto p = __gnu_test::nonexistent_path();

  std::error_code ec;
  permissions(p, perms::owner_all, ec);
  VERIFY( ec );

  __gnu_test::scoped_file f(p);
  VERIFY( exists(p) );

  ec = bad_ec;
  permissions(p, perms::owner_all, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == perms::owner_all );
  ec = bad_ec;
  permissions(p, perms::group_read, perm_options::add, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == (perms::owner_all | perms::group_read) );
  ec = bad_ec;
  permissions(p, perms::group_read, perm_options::remove, ec);
  VERIFY( !ec );
  VERIFY( status(p).permissions() == perms::owner_all );
}

void
test03()
{
#ifndef NO_SYMLINKS
  using std::filesystem::perms;
  using std::filesystem::perm_options;

  __gnu_test::scoped_file f;
  VERIFY( exists(f.path) );

  auto p = __gnu_test::nonexistent_path();
  create_symlink(f.path, p);

  std::error_code ec = make_error_code(std::errc::no_such_file_or_directory);
  permissions(p, perms::owner_all,
	      perm_options::replace|perm_options::nofollow, ec);
  bool caught = false;
  std::error_code ec2;
  try
  {
    permissions(p, perms::owner_all,
		perm_options::replace|perm_options::nofollow);
  }
  catch (const std::filesystem::filesystem_error& ex)
  {
    caught = true;
    ec2 = ex.code();
    VERIFY( ex.path1() == p );
  }
  // Both calls should succeed, or both should fail with same error:
  if (ec)
  {
    VERIFY( caught );
    VERIFY( ec == ec2 );
  }
  else
    VERIFY( !caught );

  remove(p);
#endif
}

void
test04()
{
#ifndef NO_SYMLINKS
  using perms = std::filesystem::perms;

  auto p = __gnu_test::nonexistent_path();
  create_symlink(__gnu_test::nonexistent_path(), p);

  std::error_code ec = make_error_code(std::errc::no_such_file_or_directory);
  permissions(p, perms::owner_all, ec);
  VERIFY( ec );
  std::error_code ec2;
  try
  {
    permissions(p, perms::owner_all);
  }
  catch (const std::filesystem::filesystem_error& ex)
  {
    ec2 = ex.code();
    VERIFY( ex.path1() == p );
  }
  VERIFY( ec == ec2 );

  remove(p);
#endif
}

void
test05()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  using std::filesystem::perms;
  using std::filesystem::perm_options;
  std::error_code ec;

  __gnu_test::scoped_file f;
  auto p = perms::owner_write;

  // symlink_nofollow should not give an error for non-symlinks
  ec = bad_ec;
  permissions(f.path, p, perm_options::replace|perm_options::nofollow, ec);
  VERIFY( !ec );
  auto st = status(f.path);
  VERIFY( st.permissions() == p );
  p |= perms::owner_read;
  ec = bad_ec;
  permissions(f.path, p, perm_options::replace|perm_options::nofollow, ec);
  VERIFY( !ec );
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
