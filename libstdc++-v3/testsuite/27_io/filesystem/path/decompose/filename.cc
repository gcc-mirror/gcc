// { dg-do run { target c++17 } }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// C++17 30.10.7.4.9 path decomposition [fs.path.decompose]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

void
test01()
{
  // [fs.path.decompose] p7
  VERIFY( path("/foo/bar.txt").filename() == "bar.txt" );
  VERIFY( path("/foo/bar").filename()     == "bar"     );
  VERIFY( path("/foo/bar/").filename()    == ""        );
  VERIFY( path("/").filename()            == ""        );
#ifdef __CYGWIN__
  VERIFY( path("//host").filename()       == ""        );
#else
  VERIFY( path("//host").filename()       == "host"    );
#endif
  VERIFY( path(".").filename()            == "."       );
  VERIFY( path("..").filename()           == ".."      );
}

void
test02()
{
  for (const path p : __gnu_test::test_paths)
  {
    path f = p.filename();
    if (p.empty())
      VERIFY( f.empty() );
    else
    {
      const path back = *--p.end();
      if (back.has_root_path())
	VERIFY( f.empty() );
      else
	VERIFY( f == back );
    }
  }
}

int
main()
{
  test01();
  test02();
}
