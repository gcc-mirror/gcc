// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <testsuite_hooks.h>

using std::experimental::filesystem::path;

void
test01()
{
#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  const bool is_posix = false;
#else
  const bool is_posix = true;
#endif

  VERIFY( path("/").is_absolute() == is_posix );
  VERIFY( path("/foo").is_absolute() == is_posix );
  VERIFY( path("/foo/").is_absolute() == is_posix );
  VERIFY( path("/foo/bar").is_absolute() == is_posix );
  VERIFY( path("/foo/bar/").is_absolute() == is_posix );
  VERIFY( ! path("foo").is_absolute() );
  VERIFY( ! path("foo/").is_absolute() );
  VERIFY( ! path("foo/bar").is_absolute() );
  VERIFY( ! path("foo/bar/").is_absolute() );
  VERIFY( ! path("c:").is_absolute() );
  VERIFY( ! path("c:foo").is_absolute() );
  VERIFY( ! path("c:foo/").is_absolute() );
  VERIFY( ! path("c:foo/bar").is_absolute() );
  VERIFY( ! path("c:foo/bar/").is_absolute() );
  VERIFY( path("c:/").is_absolute() == !is_posix );
  VERIFY( path("c:/foo").is_absolute() == !is_posix );
  VERIFY( path("c:/foo/").is_absolute() == !is_posix );
  VERIFY( path("c:/foo/bar").is_absolute() == !is_posix );
  VERIFY( path("c:/foo/bar/").is_absolute() == !is_posix );
}

int
main()
{
  test01();
}
