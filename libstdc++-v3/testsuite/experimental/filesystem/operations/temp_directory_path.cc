// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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
#include <stdlib.h>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
clean_env()
{
  ::unsetenv("TMPDIR");
  ::unsetenv("TMP");
  ::unsetenv("TEMPDIR");
  ::unsetenv("TEMP");
}

namespace fs = std::experimental::filesystem;

void
test01()
{
  clean_env();

  if (!fs::exists("/tmp"))
    return; // just give up

  std::error_code ec;
  fs::path p1 = fs::temp_directory_path(ec);
  VERIFY( !ec );
  VERIFY( exists(p1) );

  fs::path p2 = fs::temp_directory_path();
  VERIFY( p1 == p2 );
}

void
test02()
{
  clean_env();

  if (::setenv("TMPDIR", __gnu_test::nonexistent_path().string().c_str(), 1))
    return; // just give up

  std::error_code ec;
  fs::path p = fs::temp_directory_path(ec);
  VERIFY( ec );
  VERIFY( p == fs::path() );

  std::error_code ec2;
  try {
    p = fs::temp_directory_path();
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );
}

void
test03()
{
  auto p = __gnu_test::nonexistent_path();
  create_directories(p/"tmp");
  permissions(p, fs::perms::none);
  setenv("TMPDIR", (p/"tmp").c_str(), 1);
  std::error_code ec;
  auto r = fs::temp_directory_path(ec); // libstdc++/PR71337
  VERIFY( ec == std::make_error_code(std::errc::permission_denied) );
  VERIFY( r == fs::path() );

  std::error_code ec2;
  try {
    fs::temp_directory_path();
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );

  permissions(p, fs::perms::owner_all, ec);
  remove_all(p, ec);
}

void
test04()
{
  __gnu_test::scoped_file f;
  setenv("TMPDIR", f.path.c_str(), 1);
  std::error_code ec;
  auto r = fs::temp_directory_path(ec);
  VERIFY( ec == std::make_error_code(std::errc::not_a_directory) );
  VERIFY( r == fs::path() );

  std::error_code ec2;
  try {
    fs::temp_directory_path();
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
