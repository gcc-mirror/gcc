// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::experimental::filesystem;

void
test01()
{
  std::error_code ec;
  fs::file_status st1 = fs::status(".", ec);
  VERIFY( !ec );
  VERIFY( st1.type() == fs::file_type::directory );

  fs::file_status st2 = fs::status(".");
  VERIFY( st2.type() == fs::file_type::directory );
}

void
test02()
{
  fs::path p = __gnu_test::nonexistent_path();

  std::error_code ec;
  fs::file_status st1 = fs::status(p, ec);
  VERIFY( ec );
  VERIFY( st1.type() == fs::file_type::not_found );

  fs::file_status st2 = fs::status(p);
  VERIFY( st2.type() == fs::file_type::not_found );
}

int
main()
{
  test01();
  test02();
}
