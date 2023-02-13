// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#undef _GLIBCXX_USE_CXX11_ABI
#define _GLIBCXX_USE_CXX11_ABI 0
#include <filesystem>
#include <testsuite_fs.h>

using std::filesystem::path;

void
compare_paths(path p, std::string expected)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
  for (auto& c : expected)
    if (c == '/')
      c = '\\';
#endif
  __gnu_test::compare_paths(p, expected);
}

void
test02()
{
  path p = "./a/b/c/../.././b/c";
  // For the COW string this used to produce incorrect results:
  auto norm = p.lexically_normal();
  compare_paths( norm, "a/b/c" );
}

int
main()
{
  test02();
}
