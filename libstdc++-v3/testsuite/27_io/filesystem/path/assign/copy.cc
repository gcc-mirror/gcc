// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path copy;
    copy = p;
    __gnu_test::compare_paths(p, copy);
  }
}

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    path copy = p;
    path move;
    move = std::move(copy);
    __gnu_test::compare_paths(p, move);
  }
}

void
test03()
{
  // self assignment should have no effect
  const path orig = "foo/bar/baz";
  path p = orig;
  const auto ptr1 = p.c_str();
  const auto ptr2 = p.begin()->c_str();
  p = std::move(p);
  __gnu_test::compare_paths(p, orig);
  p = p;
  __gnu_test::compare_paths(p, orig);
  VERIFY( ptr1 == p.c_str() );
  VERIFY( ptr2 == p.begin()->c_str() );
}

void
test04()
{
  // PR libstdc++/90557
  path p1 = "a/b/c";
  const path p2 = "d/e";
  const path p3 = p2;
  p1.clear();
  p1 = p2;
  __gnu_test::compare_paths(p1, p2);
  __gnu_test::compare_paths(p1, p3);
  __gnu_test::compare_paths(p2, p3);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
