// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// C++17 30.10.14.1 Absolute [fs.op.absolute]

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

using std::filesystem::path;

void
test01()
{
  for (const path p : __gnu_test::test_paths)
  {
    std::error_code ec;
    path abs = absolute(p, ec);
    VERIFY( ec || abs.is_absolute() );
  }
}

void
test02()
{
  std::error_code ec = make_error_code(std::errc::invalid_argument);
  path root = __gnu_test::root_path();
  VERIFY( absolute(root) == root );
  VERIFY( absolute(root, ec) == root && !ec );
  VERIFY( absolute(path{}, ec).empty() && ec );

#if defined(__MINGW32__) || defined(__MINGW64__)
  path p1("/");
  VERIFY( absolute(p1) != p1 );
  path p2("/foo");
  VERIFY( absolute(p2) != p2 );
  path p3("foo");
  VERIFY( absolute(p3) != p3 );
  path p4("C:\\");
  VERIFY( absolute(p4) == p4 );
#else
  path p1("/");
  VERIFY( absolute(p1) == p1 );
  path p2("/foo");
  VERIFY( absolute(p2) == p2 );
  path p3("foo");
  VERIFY( absolute(p3) != p3 );
  VERIFY( absolute(p3) == (std::filesystem::current_path()/p3) );
#endif
}

void
test03()
{
  // PR libstdc++/90299
  const path p = __gnu_test::nonexistent_path();
  std::error_code ec;
  const path pabs = absolute(p, ec);
  VERIFY( !ec );
  VERIFY( pabs.is_absolute() );

  const path pabs2 = absolute(p);
  VERIFY( pabs2 == pabs );

  const path eabs = absolute(path{}, ec);
  VERIFY( ec == std::errc::invalid_argument );
  VERIFY( eabs.empty() );

  try {
    (void) absolute(path{});
    VERIFY( false );
  } catch (const std::filesystem::filesystem_error& e) {
    VERIFY( e.code() == std::errc::invalid_argument );
    VERIFY( e.path1().empty() );
    VERIFY( e.path2().empty() );
  }
}

int
main()
{
  test01();
  test02();
  test03();
}
