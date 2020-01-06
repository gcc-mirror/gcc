// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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
#if defined(__MINGW32__) || defined(__MINGW64__)
  // filesystem permissions not supported
  return;
#endif

  auto p = __gnu_test::nonexistent_path();
  create_directory(p);
  permissions(p, fs::perms::none);
  std::error_code ec, ec2;

  bool result = fs::is_empty(p, ec);
  VERIFY( ec == std::make_error_code(std::errc::permission_denied) );
  VERIFY( !result );

  try {
    fs::is_empty(p);
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );

  result = fs::is_empty(p/"f", ec);
  VERIFY( ec == std::make_error_code(std::errc::permission_denied) );
  VERIFY( !result );

  try {
    fs::is_empty(p/"f");
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );

  permissions(p, fs::perms::owner_all, ec);
  remove_all(p, ec);
}

void
test02()
{
  auto p = __gnu_test::nonexistent_path();
  create_directory(p);
  std::error_code ec, bad_ec = make_error_code(std::errc::invalid_argument);
  bool empty;

  ec = bad_ec;
  empty = is_empty(p, ec);
  VERIFY( !ec );
  VERIFY( empty );
  empty = is_empty(p);
  VERIFY( empty );

  __gnu_test::scoped_file f(p/"f");
  ec = bad_ec;
  empty = is_empty(f.path, ec);
  VERIFY( !ec );
  VERIFY( empty );
  empty = is_empty(f.path);
  VERIFY( empty );

  std::ofstream{f.path.c_str()} << "data";
  ec = bad_ec;
  empty = is_empty(p, ec);
  VERIFY( !ec );
  VERIFY( !empty );
  empty = is_empty(p);
  VERIFY( !empty );

  ec = bad_ec;
  empty = is_empty(p, ec);
  VERIFY( !ec );
  VERIFY( !empty );
  empty = is_empty(p);
  VERIFY( !empty );

  f.path.clear();
  remove_all(p, ec);
}

int
main()
{
  test01();
  test02();
}
