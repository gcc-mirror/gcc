// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

// 8.4.3 path appends [path.append]

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;

void
test01()
{
  const path p("/foo/bar");

  path pp = p;
  pp /= p;
  VERIFY( pp.string() == "/foo/bar/foo/bar" );

  path q("baz");

  path qq = q;
  qq /= q;
#if defined(__MINGW32__) || defined(__MINGW64__)
  VERIFY( qq.string() == "baz\\baz" );
#else
  VERIFY( qq.string() == "baz/baz" );
#endif

  q /= p;
  VERIFY( q.string() == "baz/foo/bar" );

  path r = "";
  r /= path();
  VERIFY( r.empty() );

  r /= path("rel");
  VERIFY( !r.is_absolute() );

  path s = "dir/";
  s /= path("/file");
  VERIFY( s.string() == "dir//file" );
}

int
main()
{
  test01();
}
