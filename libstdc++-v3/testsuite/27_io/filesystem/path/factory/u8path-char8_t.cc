// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-options "-fchar8_t -Wno-stringop-overread" }
// { dg-do run { target c++17 } }
// { dg-additional-options "-Wno-deprecated-declarations" { target c++20 } }

#include <filesystem>
#include <string_view>
#include <testsuite_hooks.h>

namespace fs = std::filesystem;

void
test01()
{
  fs::path p = fs::u8path(u8"");
  VERIFY( p.empty() );

  p = fs::u8path(u8"filename1");
  VERIFY( p.u8string() == u8"filename1" );

  p = fs::u8path(u8"\xf0\x9d\x84\x9e");
  VERIFY( p.u8string() == u8"\U0001D11E" );

  // The following triggers -Wstringop-overread.  See PR 102958.
  std::u8string s1 = u8"filename2";
  p = fs::u8path(s1);
  VERIFY( p.u8string() == u8"filename2" );

  std::u8string s2 = u8"filename3";
  p = fs::u8path(s2.begin(), s2.end());
  VERIFY( p.u8string() == u8"filename3" );

  std::u8string_view sv1{ s1 };
  p = fs::u8path(sv1);
  VERIFY( p.u8string() == u8"filename2" );

  std::u8string_view sv2{ s2 };
  p = fs::u8path(sv2.begin(), sv2.end());
  VERIFY( p.u8string() == u8"filename3" );
}

int
main()
{
  test01();
}
