// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

  // Test non-existent path.
  const auto p = __gnu_test::nonexistent_path();
  fs::directory_iterator iter(p, ec);
  VERIFY( ec );
  VERIFY( iter == end(iter) );

  // Test empty directory.
  create_directory(p, fs::current_path(), ec);
  VERIFY( !ec );
  iter = fs::directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter == end(iter) );

  // Test non-empty directory.
  create_directory(p / "x", ec);
  VERIFY( !ec );
  iter = fs::directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter != fs::directory_iterator() );
  VERIFY( iter->path() == p/"x" );
  ++iter;
  VERIFY( iter == end(iter) );

  if (__gnu_test::permissions_are_testable())
  {
    // Test inaccessible directory.
    permissions(p, fs::perms::none, ec);
    VERIFY( !ec );
    iter = fs::directory_iterator(p, ec);
    VERIFY( ec );
    VERIFY( iter == end(iter) );

    // Test inaccessible directory, skipping permission denied.
    const auto opts = fs::directory_options::skip_permission_denied;
    iter = fs::directory_iterator(p, opts, ec);
    VERIFY( !ec );
    VERIFY( iter == end(iter) );

    permissions(p, fs::perms::owner_all, ec);
  }

  remove_all(p, ec);
}

void
test02()
{
  std::error_code ec;
  const auto p = __gnu_test::nonexistent_path();
  create_directory(p, fs::current_path(), ec);
  create_directory(p / "x", ec);
  VERIFY( !ec );

  // Test post-increment (libstdc++/71005)
  auto iter = fs::directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter != end(iter) );
  const auto entry1 = *iter;
  const auto entry2 = *iter++;
  VERIFY( entry1 == entry2 );
  VERIFY( entry1.path() == p/"x" );
  VERIFY( iter == end(iter) );

  remove_all(p, ec);
}

void
test03()
{
  std::error_code ec;
  const auto p = __gnu_test::nonexistent_path();
  create_directories(p / "longer_than_small_string_buffer", ec);
  VERIFY( !ec );

  // Test for no reallocation on each dereference (this is a GNU extension)
  auto iter = fs::directory_iterator(p, ec);
  const auto* s1 = iter->path().c_str();
  const auto* s2 = iter->path().c_str();
  VERIFY( s1 == s2 );

  remove_all(p, ec);
}

void
test04()
{
  const fs::directory_iterator it;
  VERIFY( it == fs::directory_iterator() );
}

void
test05()
{
  auto p = __gnu_test::nonexistent_path();
  create_directory(p);
  create_directory(p / "x");
  fs::directory_iterator it(p), endit;
  VERIFY( begin(it) == it );
  static_assert( noexcept(begin(it)), "begin is noexcept" );
  VERIFY( end(it) == endit );
  static_assert( noexcept(end(it)), "end is noexcept" );

  std::error_code ec;
  remove_all(p, ec);
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
