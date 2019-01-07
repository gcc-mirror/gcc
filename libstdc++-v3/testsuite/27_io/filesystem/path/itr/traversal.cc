// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// C++17 30.10.7.5 path iterators [fs.path.itr]

#include <filesystem>
#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::path;

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

  p = "dir/.";
  v.assign(p.begin(), p.end());
  v2 = { "dir", "." };
  VERIFY( v == v2 );

  p = "dir/";
  v.assign(p.begin(), p.end());
  v2 = { "dir", "" };
  VERIFY( v == v2 );

  p = "//rootname/dir/.";
  v.assign(p.begin(), p.end());
#ifdef __CYGWIN__
  v2 = { "//rootname", "/", "dir", "." };
#else
  v2 = { "/", "rootname", "dir", "." };
#endif
  VERIFY( v == v2 );

  p = "//rootname/dir/";
  v.assign(p.begin(), p.end());
#ifdef __CYGWIN__
  v2 = { "//rootname", "/", "dir", "" };
#else
  v2 = { "/", "rootname", "dir", "" };
#endif
  VERIFY( v == v2 );

  p = "//rootname/dir/filename";
  v.assign(p.begin(), p.end());
#ifdef __CYGWIN__
  v2 = { "//rootname", "/", "dir", "filename" };
#else
  v2 = { "/", "rootname", "dir", "filename" };
#endif
  VERIFY( v == v2 );

  p = "c:relative/path";
  v.assign(p.begin(), p.end());
#if defined(__MINGW32__) || defined(__MINGW64__)
  v2 = { "c:", "relative", "path" };
#else
  v2 = { "c:relative", "path" };
#endif
  VERIFY( v == v2 );

  p = "c:/absolute/path";
  v.assign(p.begin(), p.end());
#if defined(__MINGW32__) || defined(__MINGW64__)
  v2 = { "c:", "/", "absolute", "path" };
#else
  v2 = { "c:", "absolute", "path" };
#endif
  VERIFY( v == v2 );
}

void
test02()
{
  using reverse_iterator = std::reverse_iterator<path::iterator>;
  std::vector<path> fwd, rev;

  for (const path& p : __gnu_test::test_paths)
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
  path paths[] = { "single", "multiple/elements", "trailing/slash/", "/." };
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

void
test04()
{
  std::filesystem::path p = "/a/b/c/d/e/f/g";
  VERIFY( std::distance(p.begin(), p.end()) == 8);
  auto it = p.begin();
  std::advance(it, 1);
  VERIFY( std::distance(p.begin(), it) == 1 );
  VERIFY( it->string() == "a" );
  std::advance(it, 3);
  VERIFY( std::distance(p.begin(), it) == 4 );
  VERIFY( it->string() == "d" );
  std::advance(it, -2);
  VERIFY( std::distance(p.begin(), it) == 2 );
  VERIFY( it->string() == "b" );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
