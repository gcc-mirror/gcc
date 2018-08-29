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
using __gnu_test::compare_paths;

void
test01()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;
  auto p = __gnu_test::nonexistent_path();
  canonical( p, ec );
  VERIFY( ec );

  create_directory(p);
  auto p2 = canonical( p, ec );
  compare_paths( p2, fs::current_path()/p );
  VERIFY( !ec );

  ec = bad_ec;
  p2 = canonical( fs::current_path() / "." / (p.string() + "////././."), ec );
  compare_paths( p2, fs::current_path()/p );
  VERIFY( !ec );

  ec = bad_ec;
  p = fs::current_path();
  p2 = canonical( p, ec );
  compare_paths( p2, p );
  VERIFY( !ec );

  ec = bad_ec;
  p = "/";
  p = canonical( p, ec );
  compare_paths( p, "/" );
  VERIFY( !ec );

  ec = bad_ec;
  p = "/.";
  p = canonical( p, ec );
  compare_paths( p, "/" );
  VERIFY( !ec );

  ec = bad_ec;
  p = "/..";
  p = canonical( p, ec );
  compare_paths( p, "/" );
  VERIFY( !ec );

  ec = bad_ec;
  p = "/../.././.";
  p = canonical( p, ec );
  compare_paths( p, "/" );
  VERIFY( !ec );
}

void
test02()
{
  const fs::path p = __gnu_test::nonexistent_path();
  std::error_code ec, ec2;
  const fs::path res = canonical(p, ec);
  VERIFY( ec );
  VERIFY( res.empty() );

#if __cpp_exceptions
  fs::path e1, e2;
  try {
    canonical(p);
  } catch (const fs::filesystem_error& e) {
    e1 = e.path1();
    e2 = e.path2();
    ec2 = e.code();
  }
  VERIFY( e1 == p );
  VERIFY( e2.empty() );
  VERIFY( ec == ec2 );
#endif
}


void
test03()
{
  std::error_code ec;
  auto dir = __gnu_test::nonexistent_path();
  fs::create_directory(dir);
  fs::path foo = dir/"foo", bar = dir/"bar";
  fs::create_directory(foo);
  fs::create_directory(bar);
  fs::create_symlink("../bar", foo/"baz");

  auto dirc = canonical(dir);
  auto barc = canonical(bar);

  auto p1 = fs::canonical(dir/"foo//.///..//./");
  compare_paths( p1, dirc );
  auto p2 = fs::canonical(dir/"foo//./baz///..//./");
  compare_paths( p2, dirc );
  auto p3 = fs::canonical(dir/"foo//./baz////./");
  compare_paths( p3, barc );
  auto p4 = fs::canonical(dir/"foo//./baz///..//./bar");
  compare_paths( p4, barc );
  auto p5 = fs::canonical(dir/"foo//./baz///..//./bar/");
  compare_paths( p5, p4 );
  auto p6 = fs::canonical(dir/"foo//./baz///..//./bar/.");
  compare_paths( p6, p4 );

  remove_all(dir);
}

int
main()
{
  test01();
  test02();
  test03();
}
