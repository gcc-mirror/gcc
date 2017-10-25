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

// 15.4 Copy [fs.op.copy_file]

#include <filesystem>
#include <fstream>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  using std::filesystem::copy_options;
  std::error_code ec;

  auto from = __gnu_test::nonexistent_path();
  auto to = __gnu_test::nonexistent_path();

  // test non-existent file
  bool b = copy_file(from, to, ec);
  VERIFY( !b );
  VERIFY( ec );
  VERIFY( !exists(to) );

  // test empty file
  std::ofstream{from.native()};
  VERIFY( exists(from) );
  VERIFY( file_size(from) == 0 );

  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == 0 );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to, copy_options::none, ec);
  VERIFY( b );
  VERIFY( !ec );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == 0 );

  std::ofstream{from.native()} << "Hello, filesystem!";
  VERIFY( file_size(from) != 0 );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == file_size(from) );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == file_size(from) );

  remove(from);
  remove(to);
}

int
main()
{
  test01();
}
