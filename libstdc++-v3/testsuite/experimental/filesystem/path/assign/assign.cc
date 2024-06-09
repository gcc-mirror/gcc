// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <testsuite_fs.h>

using std::experimental::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  for (std::string s : __gnu_test::test_paths)
  {
    path p0 = s, p1, p2, p3, p4;

    p1 = s;
    compare_paths(p0, p1);

    p2 = s.c_str();
    compare_paths(p0, p2);

#if _GLIBCXX_USE_WCHAR_T
    std::wstring ws(s.begin(), s.end());

    p3 = ws;
    compare_paths(p0, p3);

    p4 = ws.c_str();
    compare_paths(p0, p4);
#endif
  }
}

void
test02()
{
  for (std::string s : __gnu_test::test_paths)
  {
    path p0 = s, p1, p2, p3, p4, p5, p6, p7, p8;

    p1.assign(s);
    compare_paths(p0, p1);

    p2.assign( s.begin(), s.end() );
    compare_paths(p0, p2);

    p3.assign( s.c_str() );
    compare_paths(p0, p3);

    p4.assign( s.c_str(), s.c_str() + s.size() );
    compare_paths(p0, p4);

#if _GLIBCXX_USE_WCHAR_T
    std::wstring ws(s.begin(), s.end());

    p5.assign(ws);
    compare_paths(p0, p5);

    p6.assign( ws.begin(), ws.end() );
    compare_paths(p0, p6);

    p7.assign( ws.c_str() );
    compare_paths(p0, p7);

    p8.assign( ws.c_str(), ws.c_str() + ws.size() );
    compare_paths(p0, p8);
#endif
  }
}

int
main()
{
  test01();
  test02();
}
