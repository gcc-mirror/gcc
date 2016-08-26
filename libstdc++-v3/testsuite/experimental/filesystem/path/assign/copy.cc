// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;
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

int
main()
{
  test01();
  test02();
}
