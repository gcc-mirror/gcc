// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// The COW std::string does not have noexcept copies, because copying a
// "leaked" string can throw (and for fully-dynamic strings, copying the
// empty rep can also throw).
// That's OK, because we know that the std::string in the std::runtime_error
// or std::logic_error base class won't be leaked (and usually won't be empty).
// The is_nothrow_xxx type traits don't know that though, so we can only
// check them for the cxx11 ABI, which uses __cow_string, which has noexcept
// copies.
#if _GLIBCXX_USE_CXX11_ABI
// PR libstdc++/83306
static_assert(std::is_nothrow_copy_constructible_v<filesystem_error>);
static_assert(std::is_nothrow_copy_assignable_v<filesystem_error>);
#endif

void
test01()
{
  const char* const str = "error test";
  const std::error_code ec = make_error_code(std::errc::is_a_directory);
  const filesystem_error e1(str, ec);
  auto e2 = e1;
  VERIFY( e2.path1().empty() );
  VERIFY( e2.path2().empty() );
  VERIFY( std::string_view(e2.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );

  const filesystem_error e3(str, "test/path/one", ec);
  auto e4 = e3;
  VERIFY( e4.path1() == "test/path/one" );
  VERIFY( e4.path2().empty() );
  VERIFY( std::string_view(e4.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );

  const filesystem_error e5(str, "test/path/one", "/test/path/two", ec);
  auto e6 = e5;
  VERIFY( e6.path1() == "test/path/one" );
  VERIFY( e6.path2() == "/test/path/two" );
  VERIFY( std::string_view(e6.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );
}

void
test02()
{
  const char* const str = "error test";
  const std::error_code ec = make_error_code(std::errc::is_a_directory);
  const filesystem_error e1(str, ec);
  filesystem_error e2("", {});
  e2 = e1;
  VERIFY( e2.path1().empty() );
  VERIFY( e2.path2().empty() );
  VERIFY( std::string_view(e2.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );

  const filesystem_error e3(str, "test/path/one", ec);
  filesystem_error e4("", {});
  e4 = e3;
  VERIFY( e4.path1() == "test/path/one" );
  VERIFY( e4.path2().empty() );
  VERIFY( std::string_view(e4.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );

  const filesystem_error e5(str, "test/path/one", "/test/path/two", ec);
  filesystem_error e6("", {});
  e6 = e5;
  VERIFY( e6.path1() == "test/path/one" );
  VERIFY( e6.path2() == "/test/path/two" );
  VERIFY( std::string_view(e6.what()).find(str) != std::string_view::npos );
  VERIFY( e2.code() == ec );
}

void
test03()
{
  filesystem_error e("test", std::error_code());
  VERIFY( e.path1().empty() );
  VERIFY( e.path2().empty() );
  auto e2 = std::move(e);
  // Observers must still be usable on moved-from object:
  VERIFY( e.path1().empty() );
  VERIFY( e.path2().empty() );
  VERIFY( e.what() != nullptr );
  e2 = std::move(e);
  VERIFY( e.path1().empty() );
  VERIFY( e.path2().empty() );
  VERIFY( e.what() != nullptr );
}

int
main()
{
  test01();
  test02();
  test03();
}
