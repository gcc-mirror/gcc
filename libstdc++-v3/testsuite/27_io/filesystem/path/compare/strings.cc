// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// 8.4.8 path compare [path.compare]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  const std::string s0 = "/a/a/b/b";
  const path p0 = s0;
  for (const std::string& s : __gnu_test::test_paths)
  {
    path p(s);
    VERIFY( p.compare(s) == 0 );
    VERIFY( p.compare(s.c_str()) == 0 );
    VERIFY( p.compare(p0) == p.compare(s0) );
    VERIFY( p.compare(p0) == p.compare(s0.c_str()) );
  }
}

int
main()
{
  test01();
}
