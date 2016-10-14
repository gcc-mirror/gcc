// Copyright (C) 2016 Free Software Foundation, Inc.
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

// 15.26 Permissions [fs.op.permissions]

#include <experimental/filesystem>
#include <fstream>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  using perms = std::experimental::filesystem::perms;

  auto p = __gnu_test::nonexistent_path();
  std::ofstream{p.native()};
  VERIFY( exists(p) );
  permissions(p, perms::owner_all);
  VERIFY( status(p).permissions() == perms::owner_all );
  permissions(p, perms::group_read | perms::add_perms);
  VERIFY( status(p).permissions() == (perms::owner_all | perms::group_read) );
  permissions(p, perms::group_read | perms::remove_perms);
  VERIFY( status(p).permissions() == perms::owner_all );

  remove(p);
}

int
main()
{
  test01();
}
