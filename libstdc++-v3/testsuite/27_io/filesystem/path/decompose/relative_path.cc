// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

// 8.4.9 path decomposition [path.decompose]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  path p1 = "foo";
  VERIFY( p1.relative_path() == p1 );
  path p2 = "foo/bar";
  VERIFY( p2.relative_path() == p2 );
  path p3 = "/foo/bar";
  VERIFY( p3.relative_path() == p2 );
}

#include <iostream> // XXX

void
test02()
{
  for (const path& p : __gnu_test::test_paths)
  {
    bool after_root = false;
    const path prel = p.relative_path();
    VERIFY( !prel.has_root_name() );
    path rel;
    for (const auto& cmpt : p)
    {
      if (!cmpt.has_root_path())
        after_root = true;
      if (after_root)
        rel /= cmpt;
    }
    if (prel != rel)
      std::cout << prel << ' ' << rel << '\n';
    VERIFY( prel == rel );
  }
}

int
main()
{
  test01();
  test02();
}
