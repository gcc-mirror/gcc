// Copyright (C) 2017 Free Software Foundation, Inc.
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
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::filesystem;

void
test01()
{
  auto dir = __gnu_test::nonexistent_path();
  fs::create_directory(dir);
  const auto dirc = canonical(dir);
  fs::path foo = dir/"foo", bar = dir/"bar";
  fs::create_directory(foo);
  fs::create_directory(bar);
  fs::create_directory(bar/"baz");
  fs::create_symlink("../bar", foo/"bar");

  auto p = fs::weakly_canonical(dir/"foo//./bar///../biz/.");
  VERIFY( p == dirc/"biz/" );
  p = fs::weakly_canonical(dir/"foo/.//bar/././baz/.");
  VERIFY( p == dirc/"bar/baz" );
  p = fs::weakly_canonical(fs::current_path()/dir/"bar//../foo/bar/baz");
  VERIFY( p == dirc/"bar/baz" );

  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  ec = bad_ec;
  p = fs::weakly_canonical(dir/"foo//./bar///../biz/.", ec);
  VERIFY( !ec );
  VERIFY( p == dirc/"biz/" );
  ec = bad_ec;
  p = fs::weakly_canonical(dir/"foo/.//bar/././baz/.", ec);
  VERIFY( !ec );
  VERIFY( p == dirc/"bar/baz" );
  ec = bad_ec;
  p = fs::weakly_canonical(fs::current_path()/dir/"bar//../foo/bar/baz", ec);
  VERIFY( !ec );
  VERIFY( p == dirc/"bar/baz" );

  fs::remove_all(dir, ec);
}

int
main()
{
  test01();
}
