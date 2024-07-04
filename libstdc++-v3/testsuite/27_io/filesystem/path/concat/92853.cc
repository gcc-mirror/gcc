// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <filesystem>
#include <testsuite_fs.h>

void
test01()
{
  // PR libstdc++/92853
  using std::filesystem::path;
  path p1{ "." }, p2{ "/" };
  p1 += p2;	// corrupts heap
  path p3{ p1 };	// CRASH!
  __gnu_test::compare_paths( p3, "./" );
}

void
test02()
{
  using std::filesystem::path;
  path p1{ "." }, p2{ "////" };
  p1 += p2;
  path p3{ p1 };
  __gnu_test::compare_paths( p3, ".////" );
}

void
test03()
{
  using std::filesystem::path;
  path p1{ "./" }, p2{ "/" };
  p1 += p2;
  path p3{ p1 };
  __gnu_test::compare_paths( p3, ".//" );
}

int
main()
{
  test01();
  test02();
  test03();
}
