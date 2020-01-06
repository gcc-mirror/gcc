// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#include <experimental/filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;


void
test01()
{
  auto p1 = __gnu_test::nonexistent_path();
  auto p2 = __gnu_test::nonexistent_path();
  std::error_code ec;
  bool result;

  result = equivalent(p1, p2, ec);
  VERIFY( ec );
  VERIFY( !result );
  const auto bad_ec = ec;

  __gnu_test::scoped_file f1(p1);
  result = equivalent(p1, p2, ec);
  VERIFY( !ec );
  VERIFY( !result );

  __gnu_test::scoped_file f2(p2);
  ec = bad_ec;
  result = equivalent(p1, p2, ec);
  VERIFY( !ec );
  VERIFY( !result );

  auto p3 = __gnu_test::nonexistent_path();
  create_hard_link(p1, p3, ec);
  if (ec)
    return;  // hard links not supported
  __gnu_test::scoped_file f3(p3, __gnu_test::scoped_file::adopt_file);

  ec = bad_ec;
  result = equivalent(p1, p3, ec);
  VERIFY( !ec );
  VERIFY( result );

  ec = bad_ec;
  result = equivalent(p2, p3, ec);
  VERIFY( !ec );
  VERIFY( !result );
}

int
main()
{
  test01();
}
