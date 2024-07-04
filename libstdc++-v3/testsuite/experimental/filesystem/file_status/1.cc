// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

namespace fs = std::experimental::filesystem;

template<typename... Args>
constexpr bool nothrow_constructible() {
  return std::is_nothrow_constructible<fs::file_status, Args...>::value;
}

void
test01()
{
  fs::file_status st0;
  VERIFY( st0.type() == fs::file_type::none );
  VERIFY( st0.permissions() == fs::perms::unknown );
  static_assert( nothrow_constructible<>(), "" );

  fs::file_status st1(fs::file_type::regular);
  VERIFY( st1.type() == fs::file_type::regular );
  VERIFY( st1.permissions() == fs::perms::unknown );
  static_assert( nothrow_constructible<fs::file_type>(), "" );

  fs::file_status st2(fs::file_type::directory, fs::perms::owner_all);
  VERIFY( st2.type() == fs::file_type::directory );
  VERIFY( st2.permissions() == fs::perms::owner_all );
  static_assert( nothrow_constructible<fs::file_type, fs::perms>(), "" );

  static_assert( nothrow_constructible<const fs::file_status&>(), "" );
  static_assert( nothrow_constructible<fs::file_status>(), "" );
}

void
test02()
{
  fs::file_status st;
  VERIFY( st.type() == fs::file_type::none );
  VERIFY( st.permissions() == fs::perms::unknown );

  st.type(fs::file_type::symlink);
  VERIFY( st.type() == fs::file_type::symlink );
  VERIFY( st.permissions() == fs::perms::unknown );

  st.permissions(fs::perms::owner_all);
  VERIFY( st.type() == fs::file_type::symlink );
  VERIFY( st.permissions() == fs::perms::owner_all );
}

int
main()
{
  test01();
  test02();
}
