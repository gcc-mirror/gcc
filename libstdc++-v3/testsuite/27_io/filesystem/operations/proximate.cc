// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

using std::filesystem::proximate;
using __gnu_test::compare_paths;

// Normalize directory-separators
std::string operator""_norm(const char* s, std::size_t n)
{
  std::string str(s, n);
#if defined(__MING32__) || defined(__MINGW64__)
  for (auto& c : str)
    if (c == '/')
      c = '\\';
#endif
  return str;
}

void
test01()
{
  compare_paths( proximate("/a/d", "/a/b/c"), "../../d"_norm );
  compare_paths( proximate("/a/b/c", "/a/d"), "../b/c"_norm );
  compare_paths( proximate("a/b/c", "a"), "b/c"_norm );
  compare_paths( proximate("a/b/c", "a/b/c/x/y"), "../.."_norm );
  compare_paths( proximate("a/b/c", "a/b/c"), "." );
  compare_paths( proximate("a/b", "c/d"), "../../a/b"_norm );
}

void
test02()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec = bad_ec;
  compare_paths( proximate("/a/d", "/a/b/c", ec), "../../d"_norm );
  VERIFY( !ec );
  ec = bad_ec;
  compare_paths( proximate("/a/b/c", "/a/d", ec), "../b/c"_norm );
  VERIFY( !ec );
  ec = bad_ec;
  compare_paths( proximate("a/b/c", "a", ec), "b/c"_norm );
  VERIFY( !ec );
  ec = bad_ec;
  compare_paths( proximate("a/b/c", "a/b/c/x/y", ec), "../.."_norm );
  VERIFY( !ec );
  ec = bad_ec;
  compare_paths( proximate("a/b/c", "a/b/c", ec), "." );
  VERIFY( !ec );
  ec = bad_ec;
  compare_paths( proximate("a/b", "c/d", ec), "../../a/b"_norm );
  VERIFY( !ec );
}

int
main()
{
  test01();
  test02();
}
