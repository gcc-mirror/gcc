// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

#include <filesystem>
#include <system_error>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

// PR libstdc++/86597

void
test01()
{
  const auto bad_ec = make_error_code(std::errc::address_in_use);
  std::error_code ec;
  std::filesystem::directory_entry ent(".");

  ec = bad_ec;
  VERIFY( ent.exists(ec) );
  VERIFY( !ec );

  ec = bad_ec;
  VERIFY( ent.is_directory(ec) );
  VERIFY( !ec );

  ec = bad_ec;
  VERIFY( !ent.is_regular_file(ec) );
  VERIFY( !ec );
}

void
test02()
{
  const auto bad_ec = make_error_code(std::errc::address_in_use);
  std::error_code ec;
  std::filesystem::directory_entry ent(__gnu_test::nonexistent_path());

  ec = bad_ec;
  VERIFY( !ent.exists(ec) );
  VERIFY( !ec );

  ec = bad_ec;
  VERIFY( !ent.is_directory(ec) );
  VERIFY( !ec );

  ec = bad_ec;
  VERIFY( !ent.is_regular_file(ec) );
  VERIFY( !ec );
}

int
main()
{
  test01();
  test02();
}
