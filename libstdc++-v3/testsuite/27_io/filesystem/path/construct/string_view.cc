// { dg-do run { target c++17 } }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include <filesystem>
#include <string_view>
#include <string>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;
using __gnu_test::compare_paths;

void
test01()
{
  for (std::string s : __gnu_test::test_paths)
  {
    path p1 = s;
    std::string_view sv(s);
    path p2 = sv;
    compare_paths(p1, p2);

#if _GLIBCXX_USE_WCHAR_T
    std::wstring ws(s.begin(), s.end());
    path p3 = ws;
    std::wstring_view wsv(ws);
    path p4 = wsv;
    compare_paths(p1, p4);
#endif
  }
}

void
test02()
{
  std::string s;
  for (int i = 0; i < 10; ++i)
    s += "0/1/2/3/4/5/6/7/8/9/";
  // Construct a path with a large number of components:
  path p = s;
  auto iter = p.begin();
  for (int i = 0; i < 100; ++i)
  {
    char c = '0' + i % 10;
    std::string_view sv(&c, 1);
    VERIFY( iter != p.end() );
    compare_paths( *iter, sv );
    ++iter;
  }
  VERIFY( iter != p.end() );
  VERIFY( iter->native().empty() );
  VERIFY( ++iter == p.end() );
}

int
main()
{
  test01();
  test02();
}
