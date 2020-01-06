// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// 15.11 Current path [fs.op.current_path]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

namespace fs = std::filesystem;

void
test01()
{
  fs::path dot(".");
  fs::path cwd = fs::current_path();
  std::error_code ec;
  fs::path cwd2 = fs::current_path(ec);
  VERIFY( cwd == cwd2 );
}

void
test02()
{
  auto oldwd = fs::current_path();
  auto tmpdir = fs::temp_directory_path();
  current_path(tmpdir);
  VERIFY( canonical(fs::current_path()) == canonical(tmpdir) );
  std::error_code ec;
  current_path(oldwd, ec);
  VERIFY( canonical(fs::current_path()) == canonical(oldwd) );
  VERIFY( canonical(fs::current_path(ec)) == canonical(oldwd) );
}

int
main()
{
  test01();
  test02();
}
