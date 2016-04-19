// { dg-options "-std=gnu++11 -lstdc++fs" }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

// 15.3 Copy [fs.op.copy]

#include <experimental/filesystem>
#include <fstream>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

// Test error conditions.
void
test01()
{
  bool test __attribute__((unused)) = false;

  auto p = __gnu_test::nonexistent_path();
  std::error_code ec;

  VERIFY( !fs::exists(p) );
  fs::copy(p, ".", fs::copy_options::none, ec);
  VERIFY( ec );

  ec.clear();
  fs::copy(".", ".", fs::copy_options::none, ec);
  VERIFY( ec );

  std::ofstream{p.native()};
  VERIFY( fs::is_directory(".") );
  VERIFY( fs::is_regular_file(p) );
  ec.clear();
  fs::copy(".", p, fs::copy_options::none, ec);
  VERIFY( ec );

  remove(p, ec);
}

// Test is_symlink(f) case.
void
test02()
{
  bool test __attribute__((unused)) = false;

  auto from = __gnu_test::nonexistent_path();
  auto to = __gnu_test::nonexistent_path();
  std::error_code ec;

  fs::create_symlink(".", from, ec);
  VERIFY( !ec );
  VERIFY( fs::exists(from) );

  fs::copy(from, to, fs::copy_options::skip_symlinks, ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  fs::copy(from, to, fs::copy_options::skip_symlinks, ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  fs::copy(from, to,
           fs::copy_options::skip_symlinks|fs::copy_options::copy_symlinks,
           ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  fs::copy(from, to, fs::copy_options::copy_symlinks, ec);
  VERIFY( !ec );
  VERIFY( fs::exists(to) );

  fs::copy(from, to, fs::copy_options::copy_symlinks, ec);
  VERIFY( ec );

  remove(from, ec);
  remove(to, ec);
}

// Test is_regular_file(f) case.
void
test03()
{
  bool test __attribute__((unused)) = false;

  auto from = __gnu_test::nonexistent_path();
  auto to = __gnu_test::nonexistent_path();

  // test empty file
  std::ofstream{from.native()};
  VERIFY( fs::exists(from) );
  VERIFY( fs::file_size(from) == 0 );
  fs::copy(from, to);
  VERIFY( fs::exists(to) );
  VERIFY( fs::file_size(to) == 0 );

  remove(to);
  VERIFY( !fs::exists(to) );
  std::ofstream{from.native()} << "Hello, filesystem!";
  VERIFY( fs::file_size(from) != 0 );
  fs::copy(from, to);
  VERIFY( fs::exists(to) );
  VERIFY( fs::file_size(to) == fs::file_size(from) );
}

// Test is_directory(f) case.
void
test04()
{
  bool test __attribute__((unused)) = false;

  auto from = __gnu_test::nonexistent_path();
  auto to = __gnu_test::nonexistent_path();
  std::error_code ec;

}

// Test no-op cases.
void
test05()
{
  bool test __attribute__((unused)) = false;

  auto to = __gnu_test::nonexistent_path();
  std::error_code ec;

  fs::copy("/", to, fs::copy_options::create_symlinks, ec);
  VERIFY( !ec );
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
