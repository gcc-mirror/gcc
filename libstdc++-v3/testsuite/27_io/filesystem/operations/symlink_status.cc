// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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
// { dg-xfail-if "symlinks not supported" { *-*-mingw* } }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec = bad_ec;
  fs::path dot = ".";

  fs::file_status st1 = fs::symlink_status(dot, ec);
  VERIFY( !ec );
  VERIFY( st1.type() == fs::file_type::directory );

  fs::file_status st2 = fs::symlink_status(dot);
  VERIFY( st2.type() == fs::file_type::directory );

  fs::path link = __gnu_test::nonexistent_path();
  create_directory_symlink(dot, link);
  __gnu_test::scoped_file l(link, __gnu_test::scoped_file::adopt_file);

  st1 = fs::symlink_status(link);
  VERIFY( st1.type() == fs::file_type::symlink );
  ec = bad_ec;
  st2 = fs::symlink_status(link, ec);
  VERIFY( !ec );
  VERIFY( st2.type() == fs::file_type::symlink );
}

void
test02()
{
  fs::path p = __gnu_test::nonexistent_path();

  std::error_code ec;
  fs::file_status st1 = fs::symlink_status(p, ec);
  VERIFY( ec );
  VERIFY( st1.type() == fs::file_type::not_found );

  fs::file_status st2 = fs::symlink_status(p);
  VERIFY( st2.type() == fs::file_type::not_found );
}

void
test03()
{
  if (!__gnu_test::permissions_are_testable())
    return;

  fs::path dir = __gnu_test::nonexistent_path();
  fs::create_directory(dir);
  __gnu_test::scoped_file d(dir, __gnu_test::scoped_file::adopt_file);
  __gnu_test::scoped_file f(dir / "file");
  fs::permissions(dir, fs::perms::none);
  auto link = __gnu_test::nonexistent_path();
  fs::create_symlink(f.path, link);
  __gnu_test::scoped_file l(link, __gnu_test::scoped_file::adopt_file);

  std::error_code ec;
  fs::file_status st = fs::symlink_status(f.path, ec);
  VERIFY( ec.value() == (int)std::errc::permission_denied );
  VERIFY( st.type() == fs::file_type::none );

  st = fs::symlink_status(link, ec);
  VERIFY( !ec );
  VERIFY( st.type() == fs::file_type::symlink );

#if __cpp_exceptions
  bool caught = false;
  std::error_code ec2;
  fs::path p, p2;
  try {
    fs::symlink_status(f.path);
  } catch (const fs::filesystem_error& e) {
    caught = true;
    p = e.path1();
    p2 = e.path2();
    ec2 = e.code();
  }
  VERIFY( caught );
  VERIFY( ec2.value() == (int)std::errc::permission_denied );
  VERIFY( p == f.path );
  VERIFY( p2.empty() );
#endif

  fs::file_status st2 = symlink_status(link);
  VERIFY( st2.type() == fs::file_type::symlink );

  fs::permissions(dir, fs::perms::owner_all, ec);
}

void
test04()
{
  // PR libstdc++/88881
  fs::path p = "./";
  auto st = symlink_status(p);
  VERIFY( is_directory(st) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
