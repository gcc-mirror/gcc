// Copyright (C) 2017-2022 Free Software Foundation, Inc.
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

// { dg-options "-DUSE_FILESYSTEM_TS -lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }
// { dg-require-target-fs-space "" }

// 30.10.14.3 Permissions [fs.op.space]

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

bool check(fs::space_info const& s)
{
  const std::uintmax_t err = -1;
  return s.capacity != err || s.free != err || s.available != err;
}

void
test01()
{
  const fs::path root = __gnu_test::root_path();
  fs::space_info s = fs::space(root);
  std::error_code ec = make_error_code(std::errc::invalid_argument);
  s = fs::space(root, ec);
  VERIFY( !ec );

  s = fs::space(__gnu_test::nonexistent_path()/".", ec);
  if (ec)
    VERIFY( ! check(s) );
  else
    VERIFY( check(s) );
}

void
test02()
{
  fs::space_info s = fs::space(".");
  VERIFY( check(s) );
  VERIFY( s.capacity >= s.free );
}

int
main()
{
  test01();
  test02();
}
