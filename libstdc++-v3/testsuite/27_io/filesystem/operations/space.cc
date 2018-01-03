// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// 30.10.14.3 Permissions [fs.op.space]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  std::filesystem::space_info s = std::filesystem::space("/");
  std::error_code ec = make_error_code(std::errc::invalid_argument);
  s = std::filesystem::space("/", ec);
  VERIFY( !ec );
  s = std::filesystem::space(__gnu_test::nonexistent_path(), ec);
  VERIFY( ec );
  VERIFY( s.capacity ==  static_cast<uintmax_t>(-1) );
  VERIFY( s.free ==  static_cast<uintmax_t>(-1) );
  VERIFY( s.available ==  static_cast<uintmax_t>(-1) );
}

int
main()
{
  test01();
}
