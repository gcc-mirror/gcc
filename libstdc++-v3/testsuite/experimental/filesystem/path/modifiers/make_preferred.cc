// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

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

// 8.4.5 path modifiers [path.modifiers]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::experimental::filesystem::path;

template<typename T, T sep>
struct checker
{
  static void check(const char* s) { }
};

template<>
struct checker<char, '/'>
{
  static void check()
  {
    VERIFY( path("foo/bar").make_preferred() == "foo/bar" );
  }
};

template<>
struct checker<wchar_t, L'\\'>
{
  static void check()
  {
    VERIFY( path("foo/bar").make_preferred() == L"foo\\bar" );
  }
};

void
test01()
{
  checker<path::value_type, path::preferred_separator>::check();
}

int
main()
{
  test01();
}
