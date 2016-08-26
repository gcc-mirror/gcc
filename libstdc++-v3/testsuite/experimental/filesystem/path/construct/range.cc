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

// 8.4.1 path constructors [path.construct]

#include <experimental/filesystem>
#include <string>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  for (std::string s : __gnu_test::test_paths)
  {
    path p1 = s;
    path p2( s.begin(), s.end() );
    path p3( s.c_str() );
    path p4( s.c_str(), s.c_str() + s.size() );

    compare_paths(p1, p2);
    compare_paths(p1, p3);
    compare_paths(p1, p4);

#if _GLIBCXX_USE_WCHAR_T
    std::wstring ws(s.begin(), s.end());
    path p5 = ws;
    path p6( ws.begin(), ws.end() );
    path p7( ws.c_str() );
    path p8( ws.c_str(), ws.c_str() + ws.size() );

    compare_paths(p1, p5);
    compare_paths(p1, p6);
    compare_paths(p1, p7);
    compare_paths(p1, p8);
#endif
  }
}

int
main()
{
  test01();
}
