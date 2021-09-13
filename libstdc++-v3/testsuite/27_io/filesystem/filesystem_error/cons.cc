// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

using std::filesystem::filesystem_error;

bool contains(std::string_view what_str, std::string_view expected)
{
  return what_str.find(expected) != std::string_view::npos;
}

void
test01()
{
  const char* const str = "error test";
  const std::error_code ec = make_error_code(std::errc::is_a_directory);
  const std::filesystem::path p1 = "test/path/one";
  const std::filesystem::path p2 = "/test/path/two";
  std::system_error syserr(ec, str);

  const filesystem_error e1(str, ec);
  VERIFY( contains(e1.what(), syserr.what()) );
  VERIFY( !contains(e1.what(), "[]") ); // no "empty path" in the string
  VERIFY( e1.path1().empty() );
  VERIFY( e1.path2().empty() );
  VERIFY( e1.code() == ec );

  const filesystem_error e2(str, p1, ec);
  VERIFY( e2.path1() == p1 );
  VERIFY( e2.path2().empty() );
  VERIFY( contains(e2.what(), syserr.what()) );
  VERIFY( contains(e2.what(), p1.string()) );
  VERIFY( !contains(e2.what(), "[]") );
  VERIFY( e2.code() == ec );

  const filesystem_error e3(str, std::filesystem::path{}, ec);
  VERIFY( e3.path1().empty() );
  VERIFY( e3.path2().empty() );
  VERIFY( contains(e3.what(), syserr.what()) );
  VERIFY( contains(e3.what(), "[]") );
  VERIFY( !contains(e3.what(), "[] []") );
  VERIFY( e3.code() == ec );

  const filesystem_error e4(str, p1, p2, ec);
  VERIFY( e4.path1() == p1 );
  VERIFY( e4.path2() == p2 );
  VERIFY( contains(e4.what(), syserr.what()) );
  VERIFY( contains(e4.what(), p1.string()) );
  VERIFY( contains(e4.what(), p2.string()) );
  VERIFY( !contains(e4.what(), "[]") );
  VERIFY( e4.code() == ec );

  const filesystem_error e5(str, p1, std::filesystem::path{}, ec);
  VERIFY( e5.path1() == p1 );
  VERIFY( e5.path2().empty() );
  VERIFY( contains(e5.what(), syserr.what()) );
  VERIFY( contains(e5.what(), p1.string()) );
  VERIFY( contains(e5.what(), "[]") );
  VERIFY( e5.code() == ec );

  const filesystem_error e6(str, std::filesystem::path{}, p2, ec);
  VERIFY( e6.path1().empty() );
  VERIFY( e6.path2() == p2 );
  VERIFY( contains(e6.what(), syserr.what()) );
  VERIFY( contains(e6.what(), "[]") );
  VERIFY( contains(e6.what(), p2.string()) );
  VERIFY( e6.code() == ec );
}

int
main()
{
  test01();
}
