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
// { dg-do run { target c++11 } }E
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <stdlib.h>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
clean_env()
{
  ::unsetenv("TMPDIR");
  ::unsetenv("TMP");
  ::unsetenv("TEMPDIR");
  ::unsetenv("TEMP");
}

namespace fs = std::experimental::filesystem;

void
test01()
{
  clean_env();

  if (!fs::exists("/tmp"))
    return; // just give up

  std::error_code ec;
  fs::path p1 = fs::temp_directory_path(ec);
  VERIFY( exists(p1) );

  fs::path p2 = fs::temp_directory_path();
  VERIFY( p1 == p2 );
}

void
test02()
{
  clean_env();

  if (::setenv("TMPDIR", __gnu_test::nonexistent_path().string().c_str(), 1))
    return; // just give up

  std::error_code ec;
  fs::path p = fs::temp_directory_path(ec);
  VERIFY( ec );

  std::error_code ec2;
  try {
    p = fs::temp_directory_path();
  } catch (const fs::filesystem_error& e) {
    ec2 = e.code();
  }
  VERIFY( ec2 == ec );
}


int
main()
{
  test01();
  test02();
}
