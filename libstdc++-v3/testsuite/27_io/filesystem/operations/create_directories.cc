// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void
test01()
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  // Test empty path.
  bool b = fs::create_directories( "", ec );
  VERIFY( ec );
  VERIFY( !b );

  // Test existing path.
  ec = bad_ec;
  b = fs::create_directories( fs::current_path(), ec );
  VERIFY( !ec );
  VERIFY( !b );

  // Test non-existent path.
  const auto p = __gnu_test::nonexistent_path();
  ec = bad_ec;
  b = fs::create_directories( p, ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p) );

  ec = bad_ec;
  b = fs::create_directories( p/".", ec );
  VERIFY( !ec );
  VERIFY( !b );

  ec = bad_ec;
  b = fs::create_directories( p/"..", ec );
  VERIFY( !ec );
  VERIFY( !b );

  ec = bad_ec;
  b = fs::create_directories( p/"d1/d2/d3", ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p/"d1/d2/d3") );

  ec = bad_ec;
  b = fs::create_directories( p/"./d4/../d5", ec );
  VERIFY( !ec );
  VERIFY( b );
#if defined(__MINGW32__) || defined(__MINGW64__)
  // create_directories("./d4/..") is a no-op, does not create "d4"
#else
  VERIFY( is_directory(p/"d4") );
#endif
  VERIFY( is_directory(p/"d5") );
  VERIFY( is_directory(p/"./d4/../d5") );

  std::uintmax_t count = remove_all(p, ec);
#if defined(__MINGW32__) || defined(__MINGW64__)
  VERIFY( count == 5 );
#else
  VERIFY( count == 6 );
#endif
}

void
test02()
{
  // PR libstdc++/86910
  const auto p = __gnu_test::nonexistent_path();
  std::error_code ec;
  bool result;

  {
    __gnu_test::scoped_file file;

    result = create_directories(file.path, ec);
    VERIFY( !result );
    VERIFY( ec == std::errc::not_a_directory );
    ec.clear();
    result = create_directories(file.path / "foo", ec);
    VERIFY( !result );
    VERIFY( ec == std::errc::not_a_directory );
    ec.clear();
  }

  create_directories(p);
  {
    __gnu_test::scoped_file dir(p, __gnu_test::scoped_file::adopt_file);
    __gnu_test::scoped_file file(dir.path/"file");

    result = create_directories(file.path, ec);
    VERIFY( !result );
    VERIFY( ec == std::errc::not_a_directory );
    ec.clear();
    result = create_directories(file.path/"../bar", ec);
#if defined(__MINGW32__) || defined(__MINGW64__)
    VERIFY( result );
    VERIFY( !ec );
    VERIFY( is_directory(dir.path/"bar") );
    remove(dir.path/"bar");
#else
    VERIFY( !result );
    VERIFY( ec == std::errc::not_a_directory );
    VERIFY( !is_directory(dir.path/"bar") );
#endif
  }
}

void
test03()
{
  // PR libstdc++/87846
  const auto p = __gnu_test::nonexistent_path() / "";
  bool result = create_directories(p);
  VERIFY( result );
  VERIFY( exists(p) );
  remove(p);
  result = create_directories(p/"foo/");
  VERIFY( result );
  VERIFY( exists(p) );
  VERIFY( exists(p/"foo") );
  remove_all(p);
}

int
main()
{
  test01();
  test02();
  test03();
}
