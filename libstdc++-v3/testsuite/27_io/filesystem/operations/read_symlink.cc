// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
// { dg-require-target-fs-symlinks "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  auto p = __gnu_test::nonexistent_path();
  std::error_code ec;

  (void) read_symlink(p, ec);
  VERIFY( ec );

  fs::path tgt = ".";
  create_symlink(tgt, p);

  auto result = read_symlink(p, ec);
  VERIFY( !ec );
  VERIFY( result == tgt );

  remove(p);
}

int
main()
{
  test01();
}
