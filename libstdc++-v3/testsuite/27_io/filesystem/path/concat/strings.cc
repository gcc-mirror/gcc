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

// 30.10.7.4.4 path concatenation [fs.path.concat]

#include <filesystem>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  path p("/");
  p += path::string_type("foo");
  VERIFY( p.filename() == "foo" );
  p += "bar";
  VERIFY( p.filename() == "foobar" );
  p += '/';
  VERIFY( p.parent_path() == "/foobar" && p.filename() == "" );
#if _GLIBCXX_USE_WCHAR_T
  p += L"baz.txt";
#else
  p += "baz.txt";
#endif
  VERIFY( p.filename() == "baz.txt" );
  p.concat("/dir/");
  VERIFY( p.parent_path() == "/foobar/baz.txt/dir" && p.filename() == "" );
  std::string file = "file";
  p.concat(file.begin(), file.end());
  VERIFY( p.filename() == "file" );
}

int
main()
{
  test01();
}
