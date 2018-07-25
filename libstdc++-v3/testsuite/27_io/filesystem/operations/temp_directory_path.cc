// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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
#include <stdlib.h>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
clean_env()
{
#if defined(__MINGW32__) || defined(__MINGW64__)
  ::_putenv("TMP=");
  ::_putenv("TEMP=");
#else
  ::unsetenv("TMPDIR");
  ::unsetenv("TMP");
  ::unsetenv("TEMPDIR");
  ::unsetenv("TEMP");
#endif
}

bool
set_env(const char* name, std::string value)
{
#if defined(__MINGW32__) || defined(__MINGW64__)
  std::string s = name;
  s += '=';
  s += value;
  return !::_putenv(s.c_str());
#else
  return !::setenv(name, value.c_str(), 1);
#endif
}

namespace fs = std::filesystem;

void
test01()
{
  clean_env();

  if (!fs::exists("/tmp"))
    return; // just give up

  std::error_code ec = make_error_code(std::errc::invalid_argument);
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

  if (!set_env("TMPDIR", __gnu_test::nonexistent_path().string()))
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
  set_env("TMPDIR", (p/"tmp").string());
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
  set_env("TMPDIR", f.path.string());
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
