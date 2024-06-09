// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

  // Test empty path.
  fs::path p;
  bool b = create_directory( p, ec );
  VERIFY( ec );
  VERIFY( !b );

  // Test non-existent path
  p = __gnu_test::nonexistent_path();
  VERIFY( !exists(p) );

  b = create_directory(p, ec); // create the directory once
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( exists(p) );

  // Test existing path (libstdc++/71036).
  ec = make_error_code(std::errc::invalid_argument);
  b = create_directory(p, ec);
  VERIFY( !ec );
  VERIFY( !b );
  b = create_directory(p);
  VERIFY( !b );

  auto f = p/"file";
  std::ofstream{f} << "create file";
  b = create_directory(f, ec);
  VERIFY( ec == std::errc::file_exists );
  VERIFY( !b );
  try
  {
    create_directory(f);
    VERIFY( false );
  }
  catch (const fs::filesystem_error& e)
  {
    VERIFY( e.code() == std::errc::file_exists );
    VERIFY( e.path1() == f );
  }

#ifndef NO_SYMLINKS
  // PR libstdc++/101510 create_directory on an existing symlink to a directory
  fs::create_directory(p/"dir");
  auto link = p/"link";
  fs::create_directory_symlink("dir", link);
  ec = make_error_code(std::errc::invalid_argument);
  b = fs::create_directory(link, ec);
  VERIFY( !b );
  VERIFY( !ec );
  b = fs::create_directory(link);
  VERIFY( !b );
#endif

  remove_all(p, ec);
}

int
main()
{
  test01();
}
