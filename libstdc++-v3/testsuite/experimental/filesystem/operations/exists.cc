// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

using std::experimental::filesystem::path;

void
test01()
{
  const path root = __gnu_test::root_path();

  VERIFY( exists(root) );
  VERIFY( exists(root/".") );
  VERIFY( exists(path{"."}) );
  VERIFY( exists(path{".."}) );
  VERIFY( exists(std::experimental::filesystem::current_path()) );

  std::error_code ec = std::make_error_code(std::errc::invalid_argument);
  VERIFY( exists(root, ec) );
  VERIFY( !ec );
  VERIFY( exists(root/".", ec) );
  VERIFY( !ec );
  VERIFY( exists(path{"."}, ec) );
  VERIFY( !ec );
  VERIFY( exists(path{".."}, ec) );
  VERIFY( !ec );
  VERIFY( exists(std::experimental::filesystem::current_path(), ec) );
  VERIFY( !ec );
}

void
test02()
{
  path rel = __gnu_test::nonexistent_path();
  VERIFY( !exists(rel) );

  std::error_code ec = std::make_error_code(std::errc::invalid_argument);
  VERIFY( !exists(rel, ec) );
  VERIFY( !ec ); // DR 2725
}

void
test03()
{
  path abs = absolute(__gnu_test::nonexistent_path());
  VERIFY( !exists(abs) );

  std::error_code ec = std::make_error_code(std::errc::invalid_argument);
  VERIFY( !exists(abs, ec) );
  VERIFY( !ec ); // DR 2725
}

void
test04()
{
#if defined(__MINGW32__) || defined(__MINGW64__)
  // filesystem permissions not supported
  return;
#endif

  using perms = std::experimental::filesystem::perms;
  path p = __gnu_test::nonexistent_path();
  create_directory(p);
  permissions(p, perms::all | perms::remove_perms);

  auto unr = p / "unreachable";
  std::error_code ec;
  VERIFY( !exists(unr, ec) );
  VERIFY( ec == std::errc::permission_denied );
  ec.clear();
  try
  {
    exists(unr);
  }
  catch(const std::experimental::filesystem::filesystem_error& ex)
  {
    ec = ex.code();
    VERIFY( ex.path1() == unr );
  }
  VERIFY( ec == std::errc::permission_denied );

  permissions(p, perms::owner_all);
  remove(p);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
