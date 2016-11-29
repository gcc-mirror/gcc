// Copyright (C) 2016 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
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
  b = create_directory(p, ec);
  VERIFY( !ec );
  VERIFY( !b );
  b = create_directory(p);
  VERIFY( !ec );
  VERIFY( !b );

  remove_all(p, ec);
}

int
main()
{
  test01();
}
