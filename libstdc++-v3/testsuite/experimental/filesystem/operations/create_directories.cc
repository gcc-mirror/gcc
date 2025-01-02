// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::experimental::filesystem;

void
test01()
{
  std::error_code ec;

  // Test empty path.
  bool b = fs::create_directories( "", ec );
  VERIFY( ec );
  VERIFY( !b );

  // Test existing path.
  b = fs::create_directories( fs::current_path(), ec );
  VERIFY( !ec );
  VERIFY( !b );

  // Test non-existent path.
  const auto p = __gnu_test::nonexistent_path();
  b = fs::create_directories( p, ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p) );

  b = fs::create_directories( p/".", ec );
  VERIFY( !ec );
  VERIFY( !b );

  b = fs::create_directories( p/"..", ec );
  VERIFY( !ec );
  VERIFY( !b );

  b = fs::create_directories( p/"d1/d2/d3", ec );
  VERIFY( !ec );
  VERIFY( b );
  VERIFY( is_directory(p/"d1/d2/d3") );

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
    result = create_directories(file.path / "foo", ec);
    VERIFY( !result );
    VERIFY( ec );
    ec.clear();
  }

  create_directories(p);
  {
    __gnu_test::scoped_file dir(p, __gnu_test::scoped_file::adopt_file);
    __gnu_test::scoped_file file(dir.path/"file");

    result = create_directories(file.path, ec);
    VERIFY( !result );
    VERIFY( ec == std::errc::not_a_directory );
    result = create_directories(file.path/"../bar", ec);
#if defined(__MINGW32__) || defined(__MINGW64__)
    VERIFY( result );
    VERIFY( !ec );
    VERIFY( is_directory(dir.path/"bar") );
    remove(dir.path/"bar");
#else
    VERIFY( !result );
    VERIFY( ec );
#endif
  }
}

void
test03()
{
  // PR libstdc++/87846
  const auto p = __gnu_test::nonexistent_path() / "/";
  bool result = create_directories(p);
  VERIFY( result );
#if defined(__MINGW32__) || defined(__MINGW64__)
  VERIFY( exists(p/".") ); // needed due to PR libstdc++/88881
#else
  VERIFY( exists(p) );
#endif
  remove(p);
  result = create_directories(p/"foo/");
  VERIFY( result );
#if defined(__MINGW32__) || defined(__MINGW64__)
  VERIFY( exists(p/".") ); // needed due to PR libstdc++/88881
#else
  VERIFY( exists(p) );
#endif
  VERIFY( exists(p/"foo") );
  remove_all(p);
}

void
test04()
{
#ifndef NO_SYMLINKS
  // PR libstdc++/101510
  // create_directories reports an error if the path is a symlink to a dir
  std::error_code ec = make_error_code(std::errc::invalid_argument);
  const auto p = __gnu_test::nonexistent_path() / "";
  fs::create_directories(p/"dir");
  auto link = p/"link";
  fs::create_directory_symlink("dir", link);
  bool created = fs::create_directories(link, ec);
  VERIFY( !created );
  VERIFY( !ec );
  created = fs::create_directories(link);
  VERIFY( !created );
  remove_all(p);
#endif
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
