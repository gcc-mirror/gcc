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

// C++17 30.10.8.4.3 path appends [fs.path.append]

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

// path::operator/=(const path&)

path append(path l, const path& r)
{
  l /= r;
  return l;
}

void
test01()
{
  compare_paths( append("/foo/bar", "/foo/"), "/foo/" );

#ifndef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  compare_paths( append("baz", "baz"), "baz/baz" );
#else
  compare_paths( append("baz", "baz"), "baz\\baz" );
#endif
  compare_paths( append("baz/", "baz"), "baz/baz" );
  compare_paths( append("baz",  "/foo/bar"), "/foo/bar" );
  compare_paths( append("baz/", "/foo/bar"), "/foo/bar" );

  VERIFY( append("", "").empty() );
  VERIFY( !append("", "rel").is_absolute() );

  compare_paths( append("dir/", "/file"), "/file" );
  compare_paths( append("dir/", "file"),  "dir/file" );

#ifdef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  compare_paths( append("c:/foo", "/bar"),  "c:/bar" );
#endif
}

void
test02()
{
  // C++17 [fs.path.append] p4
#ifndef _GLIBCXX_FILESYSTEM_IS_WINDOWS
  compare_paths( append("//host", "foo"),  "//host/foo" );

  compare_paths( append("//host/", "foo"),  "//host/foo" );

  // path("foo") / ""; // yields "foo/"
  compare_paths( append("foo", ""), "foo/" );

  // path("foo") / "/bar"; // yields "/bar"
  compare_paths( append("foo", "/bar"), "/bar" );
#else
  compare_paths( append("//host", "foo"),  "//host\\foo" );

  compare_paths( append("//host/", "foo"), "//host/foo" );

  // path("foo") / ""; // yields "foo/"
  compare_paths( append("foo", ""), "foo\\" );

  // path("foo") / "/bar"; // yields "/bar"
  compare_paths( append("foo", "/bar"),  "/bar" );

  // path("foo") / "c:/bar"; // yields "c:/bar"
  compare_paths( append("foo", "c:/bar"),  "c:/bar" );

  // path("foo") / "c:"; // yields "c:"
  compare_paths( append("foo", "c:"),  "c:" );

  // path("c:") / ""; // yields "c:"
  compare_paths( append("c:", ""),  "c:" );

  // path("c:foo") / "/bar"; // yields "c:/bar"
  compare_paths( append("c:foo", "/bar"),  "c:/bar" );

  // path("c:foo") / "c:bar"; // yields "c:foo/bar"
  compare_paths( append("foo", "c:\\bar"),  "c:\\bar" );
#endif
}

int
main()
{
  test01();
  test02();
}
