// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// 8.5 path iterators [path.itr]

#include <experimental/filesystem>
#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::experimental::filesystem::path;

void
test01()
{
  path p;
  VERIFY( p.begin() == p.end() );

  std::vector<path> v, v2;

  p = "/";
  v.assign(p.begin(), p.end());
  v2 = { "/" };
  VERIFY( v == v2 );

  p = "filename";
  v.assign(p.begin(), p.end());
  v2 = { "filename" };
  VERIFY( v == v2 );

  p = "dir/";
  v.assign(p.begin(), p.end());
  v2 = { "dir", "." };
  VERIFY( v == v2 );

  p = "//rootname/dir/";
  v.assign(p.begin(), p.end());
  v2 = { "//rootname", "/", "dir", "." };
  VERIFY( v == v2 );

  p = "//rootname/dir/filename";
  v.assign(p.begin(), p.end());
  v2 = { "//rootname", "/", "dir", "filename" };
  VERIFY( v == v2 );
}

void
test02()
{
  using reverse_iterator = std::reverse_iterator<path::iterator>;
  std::vector<path> fwd, rev;

  for (const path p : __gnu_test::test_paths)
  {
    const auto begin = p.begin(), end = p.end();
    fwd.assign(begin, end);
    rev.assign(reverse_iterator(end), reverse_iterator(begin));
    VERIFY( fwd.size() == rev.size() );
    VERIFY( std::equal(fwd.begin(), fwd.end(), rev.rbegin()) );
  }
}

void
test03()
{
  path paths[] = { "single", "multiple/elements" };
  for (const path& p : paths)
    for (auto iter = p.begin(); iter != p.end(); ++iter)
    {
      auto iter2 = iter;
      ++iter;
      iter2++;
      VERIFY( iter2 == iter );
      --iter;
      iter2--;
      VERIFY( iter2 == iter );
    }
}

int
main()
{
  test01();
  test02();
  test03();
}
