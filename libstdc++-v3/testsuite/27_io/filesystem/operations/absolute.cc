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

// C++17 30.10.14.1 Absolute [fs.op.absolute]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  for (const path& p : __gnu_test::test_paths)
    VERIFY( absolute(p).is_absolute() );
}

void
test02()
{
  path p1("/");
  VERIFY( absolute(p1) == p1 );
  path p2("/foo");
  VERIFY( absolute(p2) == p2 );
  path p3("foo");
  VERIFY( absolute(p3) != p3 );
  VERIFY( absolute(p3) == (std::filesystem::current_path()/p3) );
}

int
main()
{
  test01();
  test02();
}
