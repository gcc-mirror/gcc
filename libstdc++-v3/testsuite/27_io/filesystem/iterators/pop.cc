// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  std::error_code ec;
  fs::recursive_directory_iterator dir;
  dir.pop(ec);  // This is undefined, but our implementation
  VERIFY( ec ); // checks and returns an error.
  VERIFY( dir == end(dir) );

  std::error_code ec2;
  try
  {
    dir.pop();
  }
  catch (const fs::filesystem_error& ex)
  {
    ec2 = ex.code();
  }
  VERIFY( ec2 == ec );
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;
  const auto p = __gnu_test::nonexistent_path();
  create_directories(p / "d1/d2/d3");
  for (int i = 0; i < 3; ++i)
  {
    fs::recursive_directory_iterator dir(p);
    VERIFY( dir != end(dir) );
    std::advance(dir, i);
    VERIFY( dir != end(dir) );
    VERIFY( dir.depth() == i );
    ec = bad_ec;
    dir.pop(ec);
    VERIFY( !ec );
    VERIFY( dir == end(dir) );

    dir = fs::recursive_directory_iterator(p);
    std::advance(dir, i);
    VERIFY( dir != end(dir) );
    VERIFY( dir.depth() == i );
    dir.pop();
    VERIFY( dir == end(dir) );
  }
  remove_all(p, ec);
}

void
test03()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;
  const auto p = __gnu_test::nonexistent_path();
  create_directories(p / "d1/d2/d3");
  create_directories(p / "d1/d2/e3");
  create_directories(p / "d1/e2/d3");
  for (int i = 0; i < 3; ++i)
  {
    fs::recursive_directory_iterator dir(p);
    std::advance(dir, i);
    VERIFY( dir != end(dir) );
    int expected_depth = i;
    VERIFY( dir.depth() == expected_depth );
    ec = bad_ec;
    dir.pop(ec);
    VERIFY( !ec );
    if (dir != end(dir))
      VERIFY( dir.depth() == (expected_depth - 1) );

    dir = fs::recursive_directory_iterator(p);
    std::advance(dir, i);
    VERIFY( dir != end(dir) );
    VERIFY( dir.depth() == i );
    dir.pop();
    if (dir != end(dir))
      VERIFY( dir.depth() == (i -1) );
  }
  remove_all(p, ec);
}

int
main()
{
  test01();
  test02();
  test03();
}
