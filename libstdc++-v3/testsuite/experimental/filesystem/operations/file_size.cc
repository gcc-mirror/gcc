// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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
  auto size = fs::file_size(".", ec);
  VERIFY( ec == std::errc::is_a_directory );
  VERIFY( size == (std::uintmax_t)-1 );

  try {
    size = fs::file_size(".");
    ec.clear();
  } catch (const fs::filesystem_error& e) {
    ec = e.code();
  }
  VERIFY( ec == std::errc::is_a_directory );
  VERIFY( size == (std::uintmax_t)-1 );
}

void
test02()
{
  fs::path p = __gnu_test::nonexistent_path();

  std::error_code ec;
  auto size = fs::file_size(p, ec);
  VERIFY( ec );
  VERIFY( size == (std::uintmax_t)-1 );

  try {
    size = fs::file_size(p);
    ec.clear();
  } catch (const fs::filesystem_error& e) {
    ec = e.code();
  }
  VERIFY( ec );
  VERIFY( size == (std::uintmax_t)-1 );
}

int
main()
{
  test01();
  test02();
}
