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

// 15.4 Copy [fs.op.copy_file]

#include <filesystem>
#include <fstream>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

void
test01()
{
  using std::filesystem::copy_options;
  std::error_code ec;

  auto from = __gnu_test::nonexistent_path();
  auto to = __gnu_test::nonexistent_path();

  // test non-existent file
  bool b = copy_file(from, to, ec);
  VERIFY( !b );
  VERIFY( ec );
  VERIFY( !exists(to) );

  // test empty file
  std::ofstream{from};
  VERIFY( exists(from) );
  VERIFY( file_size(from) == 0 );

  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == 0 );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to, copy_options::none, ec);
  VERIFY( b );
  VERIFY( !ec );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == 0 );

  std::ofstream{from} << "Hello, filesystem!";
  VERIFY( file_size(from) != 0 );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == file_size(from) );
  remove(to);
  VERIFY( !exists(to) );
  b = copy_file(from, to);
  VERIFY( b );
  VERIFY( exists(to) );
  VERIFY( file_size(to) == file_size(from) );

  remove(from);
  remove(to);
}

void
test_directory()
{
  std::error_code ec;

  auto dir = __gnu_test::nonexistent_path();
  auto file = __gnu_test::nonexistent_path();

  // error condition: is_regular_file(from) is false
  create_directory(dir);
  bool b = copy_file(dir, file, ec);
  VERIFY( !b );
  VERIFY( ec == std::errc::invalid_argument );
  VERIFY( !exists(file) );
  VERIFY( is_directory(dir) );

  ec = std::make_error_code(std::errc::address_in_use);
  // error condition: exists(to) is true and is_regular_file(to) is false
  std::ofstream{file} << "regular file";
  b = copy_file(file, dir, ec);
  VERIFY( !b );
  VERIFY( ec == std::errc::invalid_argument );
  VERIFY( exists(file) );
  VERIFY( is_directory(dir) );

  remove(dir);
  remove(file);
}

void
test_existing()
{
  using std::filesystem::copy_options;
  std::error_code ec;

  auto from = __gnu_test::nonexistent_path();
  auto to = from;

  // error condition: exists(to) is true and equivalent(from, to) is true
  std::ofstream{from} << "source";
  bool b = copy_file(from, to, ec);
  VERIFY( !b );
  VERIFY( ec == std::errc::file_exists );

  to = __gnu_test::nonexistent_path();
  std::ofstream{to} << "overwrite me";

  // error condition: exists(to) is true and options == copy_options::none
  b = copy_file(from, to, ec);
  VERIFY( !b );
  VERIFY( ec == std::errc::file_exists );
  VERIFY( file_size(to) != file_size(from) );

#if __cpp_exceptions
  try
  {
    (void) copy_file(from, to);
    VERIFY(false);
  }
  catch (const std::filesystem::filesystem_error& e)
  {
    std::string_view msg = e.what();
    VERIFY( msg.find(from.string()) != msg.npos );
    VERIFY( msg.find(to.string()) != msg.npos );
    VERIFY( e.code() == std::errc::file_exists );
  }
  VERIFY( file_size(to) == 12 );
#endif

  b = copy_file(from, to, copy_options::skip_existing, ec);
  VERIFY( !b );
  VERIFY( !ec );
  VERIFY( file_size(to) != file_size(from) );

  b = copy_file(from, to, copy_options::skip_existing); // doesn't throw
  VERIFY( !b );
  VERIFY( file_size(to) != file_size(from) );

  b = copy_file(from, to, copy_options::overwrite_existing, ec);
  VERIFY( b );
  VERIFY( !ec );
  VERIFY( file_size(to) == file_size(from) );

  std::ofstream{to} << "overwrite me again";
  b = copy_file(from, to, copy_options::overwrite_existing); // doesn't throw
  VERIFY( b );
  VERIFY( file_size(to) == file_size(from) );

  ec = std::make_error_code(std::errc::address_in_use);
  auto time = last_write_time(from);
  std::ofstream{to} << "touched";
  last_write_time(to, time + std::chrono::seconds(60));
  b = copy_file(from, to, copy_options::update_existing, ec);
  VERIFY( !b );
  VERIFY( !ec );
  VERIFY( file_size(to) != file_size(from) );

  b = copy_file(from, to, copy_options::update_existing); // doesn't throw
  VERIFY( !b );
  VERIFY( file_size(to) != file_size(from) );

  ec = std::make_error_code(std::errc::address_in_use);
  time = last_write_time(to);
  std::ofstream{from} << "touched more recently";
  last_write_time(from, time + std::chrono::seconds(60));
  b = copy_file(from, to, copy_options::update_existing, ec);
  VERIFY( b );
  VERIFY( !ec );
  VERIFY( file_size(to) == file_size(from) );

  time = last_write_time(to);
  std::ofstream{from} << "touched even more recently";
  last_write_time(from, time + std::chrono::seconds(60));
  b = copy_file(from, to, copy_options::update_existing); // doesn't throw
  VERIFY( b );
  VERIFY( file_size(to) == file_size(from) );

  remove(from);
  remove(to);
}

int
main()
{
  test01();
  test_existing();
}
