// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  // Test empty path.
  bool b = fs::create_directories( "", ec );
  VERIFY( ec );
  VERIFY( !b );

  // Test existing path.
  ec = bad_ec;
  b = fs::create_directories( fs::current_path(), ec );
  VERIFY( !ec );
  VERIFY( !b );

  // Test non-existent path.
  const auto p = __gnu_test::nonexistent_path();
  ec = bad_ec;
  b = fs::create_directories( p, ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p) );

  ec = bad_ec;
  b = fs::create_directories( p/".", ec );
  VERIFY( !ec );
  VERIFY( !b );

  ec = bad_ec;
  b = fs::create_directories( p/"..", ec );
  VERIFY( !ec );
  VERIFY( !b );

  ec = bad_ec;
  b = fs::create_directories( p/"d1/d2/d3", ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p/"d1/d2/d3") );

  ec = bad_ec;
  b = fs::create_directories( p/"./d4/../d5", ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p/"./d4/../d5") );

  std::uintmax_t count = remove_all(p, ec);
  VERIFY( count == 6 );
}

int
main()
{
  test01();
}
