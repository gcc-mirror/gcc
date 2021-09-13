// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  std::error_code ec, ec2;
  __gnu_test::scoped_file f;
  auto tgt = f.path;

  // Test empty path.
  fs::path p;
  create_symlink(tgt, p, ec );
  VERIFY( ec );
  try
  {
    create_symlink(tgt, p);
  }
  catch (const std::filesystem::filesystem_error& ex)
  {
    ec2 = ex.code();
    VERIFY( ex.path1() == tgt );
    VERIFY( ex.path2() == p );
  }
  VERIFY( ec2 == ec );
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec, ec2;
  __gnu_test::scoped_file f;
  auto tgt = f.path;

  // Test non-existent path
  auto p = __gnu_test::nonexistent_path();
  VERIFY( !exists(p) );

  ec = bad_ec;
  create_symlink(tgt, p, ec); // create the symlink once
  VERIFY( !ec );
  VERIFY( exists(p) );
  VERIFY( is_symlink(p) );
  remove(p);
  create_symlink(tgt, p); // create the symlink again
  VERIFY( exists(p) );
  VERIFY( is_symlink(p) );

  ec.clear();
  create_symlink(tgt, p, ec); // Try to create existing symlink
  VERIFY( ec );
  try
  {
    create_symlink(tgt, p);
  }
  catch (const std::filesystem::filesystem_error& ex)
  {
    ec2 = ex.code();
    VERIFY( ex.path1() == tgt );
    VERIFY( ex.path2() == p );
  }
  VERIFY( ec2 == ec );

  remove(p);
}

int
main()
{
  test01();
}
