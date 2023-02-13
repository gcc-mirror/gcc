// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  // Test non-existent path.
  const auto p = __gnu_test::nonexistent_path();
  fs::recursive_directory_iterator iter(p, ec);
  VERIFY( ec );
  VERIFY( iter == end(iter) );

  // Test empty directory.
  ec = bad_ec;
  create_directory(p, fs::current_path(), ec);
  VERIFY( !ec );
  ec = bad_ec;
  iter = fs::recursive_directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter == end(iter) );

  // Test non-empty directory.
  ec = bad_ec;
  create_directories(p / "d1/d2", ec);
  VERIFY( !ec );
  ec = bad_ec;
  iter = fs::recursive_directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter != end(iter) );
  VERIFY( iter->path() == p/"d1" );
  ++iter;
  VERIFY( iter != end(iter) );
  VERIFY( iter->path() == p/"d1/d2" );
  ++iter;
  VERIFY( iter == end(iter) );

  if (__gnu_test::permissions_are_testable())
  {
    // Test inaccessible directory.
    ec = bad_ec;
    permissions(p, fs::perms::none, ec);
    VERIFY( !ec );
    iter = fs::recursive_directory_iterator(p, ec);
    VERIFY( ec );
    VERIFY( iter == end(iter) );

    // Test inaccessible directory, skipping permission denied.
    const auto opts = fs::directory_options::skip_permission_denied;
    ec = bad_ec;
    iter = fs::recursive_directory_iterator(p, opts, ec);
    VERIFY( !ec );
    VERIFY( iter == end(iter) );

    // Test inaccessible sub-directory.
    ec = bad_ec;
    permissions(p, fs::perms::owner_all, ec);
    VERIFY( !ec );
    ec = bad_ec;
    permissions(p/"d1/d2", fs::perms::none, ec);
    VERIFY( !ec );
    ec = bad_ec;
    iter = fs::recursive_directory_iterator(p, ec);
    VERIFY( !ec );
    VERIFY( iter != end(iter) );
    VERIFY( iter->path() == p/"d1" );
    ++iter;              // should recurse into d1
    VERIFY( iter != end(iter) );
    VERIFY( iter->path() == p/"d1/d2" );
    iter.increment(ec);  // should fail to recurse into p/d1/d2
    VERIFY( ec );
    VERIFY( iter == end(iter) );

    // Test inaccessible sub-directory, skipping permission denied.
    ec = bad_ec;
    iter = fs::recursive_directory_iterator(p, opts, ec);
    VERIFY( !ec );
    VERIFY( iter != end(iter) );
    VERIFY( iter->path() == p/"d1" );
    ++iter;              // should recurse into d1
    VERIFY( iter->path() == p/"d1/d2" );
    ec = bad_ec;
    iter.increment(ec);  // should fail to recurse into p/d1/d2, so skip it
    VERIFY( !ec );
    VERIFY( iter == end(iter) );

    permissions(p/"d1/d2", fs::perms::owner_all, ec);
  }

  remove_all(p, ec);
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;
  const auto p = __gnu_test::nonexistent_path();
  ec = bad_ec;
  create_directories(p / "d1/d2", ec);
  VERIFY( !ec );

  // Test post-increment (libstdc++/71005)
  ec = bad_ec;
  auto iter = fs::recursive_directory_iterator(p, ec);
  VERIFY( !ec );
  VERIFY( iter != end(iter) );
  const auto entry1 = *iter;
  const auto entry2 = *iter++;
  VERIFY( entry1 == entry2 );
  VERIFY( entry1.path() == p/"d1" );
  const auto entry3 = *iter;
  const auto entry4 = *iter++;
  VERIFY( entry3 == entry4 );
  VERIFY( entry3.path() == p/"d1/d2" );
  VERIFY( iter == end(iter) );

  remove_all(p, ec);
}

void
test03()
{
  std::error_code ec = make_error_code(std::errc::invalid_argument);
  const auto p = __gnu_test::nonexistent_path();
  create_directories(p / "longer_than_small_string_buffer", ec);
  VERIFY( !ec );

  // Test for no reallocation on each dereference (this is a GNU extension)
  auto iter = fs::recursive_directory_iterator(p, ec);
  const auto* s1 = iter->path().c_str();
  const auto* s2 = iter->path().c_str();
  VERIFY( s1 == s2 );

  remove_all(p, ec);
}

void
test04()
{
  // libstdc++/71004
  const fs::recursive_directory_iterator it;
  VERIFY( it == end(it) );
}

void
test05()
{
  auto p = __gnu_test::nonexistent_path();
  create_directory(p);
  create_directory(p / "x");
  fs::recursive_directory_iterator it(p), endit;
  VERIFY( begin(it) == it );
  static_assert( noexcept(begin(it)), "begin is noexcept" );
  VERIFY( end(it) == endit );
  static_assert( noexcept(end(it)), "end is noexcept" );

  std::error_code ec;
  remove_all(p, ec);
}

void
test06()
{
#ifndef NO_SYMLINKS
  auto p = __gnu_test::nonexistent_path();
  create_directories(p/"d1/d2");
  create_directory_symlink("d1", p/"link");
  fs::recursive_directory_iterator it(p), endit;
  VERIFY( std::distance(it, endit) == 3 ); // d1 and d2 and link

  it = fs::recursive_directory_iterator(p, fs::directory_options::follow_directory_symlink);
  VERIFY( std::distance(it, endit) == 4 ); // d1 and d1/d2 and link and link/d2

  std::error_code ec;
  remove_all(p, ec);
#endif
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
