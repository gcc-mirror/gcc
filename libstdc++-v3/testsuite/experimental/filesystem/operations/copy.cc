// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

// Test error conditions.
void
test01()
{
  auto p = __gnu_test::nonexistent_path();
  std::error_code ec;

  VERIFY( !fs::exists(p) );
  fs::copy(p, ".", fs::copy_options::none, ec);
  VERIFY( ec );

  ec.clear();
  fs::copy(".", ".", fs::copy_options::none, ec);
  VERIFY( ec );

  __gnu_test::scoped_file f(p);
  VERIFY( fs::is_directory(".") );
  VERIFY( fs::is_regular_file(p) );
  ec.clear();
  fs::copy(".", p, fs::copy_options::none, ec);
  VERIFY( ec );

  auto to = __gnu_test::nonexistent_path();
  ec.clear();
  auto opts = fs::copy_options::create_symlinks;
  fs::copy("/", to, opts, ec);
  VERIFY( ec == std::make_error_code(std::errc::is_a_directory) );
  VERIFY( !exists(to) );

  ec.clear();
  opts |= fs::copy_options::recursive;
  fs::copy("/", to, opts, ec);
  VERIFY( ec == std::make_error_code(std::errc::is_a_directory) );
  VERIFY( !exists(to) );
}

// Test is_symlink(f) case.
void
test02()
{
#ifndef NO_SYMLINKS
  auto from = __gnu_test::nonexistent_path();
  std::error_code ec, bad = std::make_error_code(std::errc::invalid_argument);

  ec = bad;
  fs::create_symlink(".", from, ec);
  VERIFY( !ec );
  VERIFY( fs::exists(from) );

  auto to = __gnu_test::nonexistent_path();
  ec = bad;
  fs::copy(from, to, fs::copy_options::skip_symlinks, ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  ec = bad;
  fs::copy(from, to, fs::copy_options::skip_symlinks, ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  ec = bad;
  fs::copy(from, to,
           fs::copy_options::skip_symlinks|fs::copy_options::copy_symlinks,
           ec);
  VERIFY( !ec );
  VERIFY( !fs::exists(to) );

  ec = bad;
  fs::copy(from, to, fs::copy_options::copy_symlinks, ec);
  VERIFY( !ec );
  VERIFY( fs::exists(to) );
  VERIFY( is_symlink(to) );

  fs::copy(from, to, fs::copy_options::copy_symlinks, ec);
  VERIFY( ec );

  remove(from, ec);
  remove(to, ec);
#endif
}

// Test is_regular_file(f) case.
void
test03()
{
  auto from = __gnu_test::nonexistent_path();

  // test empty file
  std::ofstream{from.c_str()};
  VERIFY( fs::exists(from) );
  VERIFY( fs::file_size(from) == 0 );

  auto to = __gnu_test::nonexistent_path();
  fs::copy(from, to);
  VERIFY( fs::exists(to) );
  VERIFY( fs::file_size(to) == 0 );

  remove(to);
  VERIFY( !fs::exists(to) );
  std::ofstream{from.c_str()} << "Hello, filesystem!";
  VERIFY( fs::file_size(from) != 0 );
  fs::copy(from, to);
  VERIFY( fs::exists(to) );
  VERIFY( fs::file_size(to) == fs::file_size(from) );

  remove(from);
  remove(to);
}

// Test is_directory(f) case.
void
test04()
{
  auto from = __gnu_test::nonexistent_path();
  std::error_code ec;

  create_directories(from/"a/b/c");

  auto to = __gnu_test::nonexistent_path();
  {
    __gnu_test::scoped_file f(to);
    copy(from, to, ec);
    VERIFY( ec );
  }

  __gnu_test::scoped_file f1(from/"a/f1");
  std::ofstream{f1.path.c_str()} << "file one";
  __gnu_test::scoped_file f2(from/"a/b/f2");
  std::ofstream{f2.path.c_str()} << "file two";

  copy(from, to, ec);
  VERIFY( !ec );
  VERIFY( exists(to) && is_empty(to) );
  remove(to);

  copy(from, to, fs::copy_options::recursive, ec);
  VERIFY( !ec );
  VERIFY( exists(to) && !is_empty(to) );
  VERIFY( is_regular_file(to/"a/f1") && !is_empty(to/"a/f1") );
  VERIFY( file_size(from/"a/f1") == file_size(to/"a/f1") );
  VERIFY( is_regular_file(to/"a/b/f2") && !is_empty(to/"a/b/f2") );
  VERIFY( file_size(from/"a/b/f2") == file_size(to/"a/b/f2") );
  VERIFY( is_directory(to/"a/b/c") && is_empty(to/"a/b/c") );

  f1.path.clear();
  f2.path.clear();
  remove_all(from, ec);
  remove_all(to, ec);
}

// Test no-op cases.
void
test05()
{
  auto to = __gnu_test::nonexistent_path();
  std::error_code ec = std::make_error_code(std::errc::invalid_argument);

  fs::copy("/", to, fs::copy_options::copy_symlinks, ec);
  VERIFY( !ec );  // Previous value should be cleared (LWG 2683)
}

void
test_pr99290()
{
  auto dir = __gnu_test::nonexistent_path();
  auto source = dir/"source";
  auto dest = dir/"dest";
  create_directories(source/"emptydir");
  create_directories(dest/"emptydir");
  std::ofstream{source/"file"} << 'a';
  std::ofstream{dest/"file"} << 'b';
  // PR libstdc++/99290
  // std::filesystem::copy does not always report errors for recursion
  std::error_code ec;
  copy(source, dest, ec);
  VERIFY( ec == std::errc::file_exists );

#if __cpp_exceptions
  try {
    copy(source, dest);
    VERIFY( false );
  } catch (const fs::filesystem_error& e) {
    VERIFY( e.code() == std::errc::file_exists );
  }
#endif

  remove_all(dir);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test_pr99290();
}
