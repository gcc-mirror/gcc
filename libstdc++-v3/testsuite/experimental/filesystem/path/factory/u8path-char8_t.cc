// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-options "-lstdc++fs -fchar8_t -Wno-stringop-overread" }
// { dg-do run { target c++11 } }
// { dg-require-filesystem-ts "" }

#include <experimental/filesystem>
#include <testsuite_hooks.h>

namespace fs = std::experimental::filesystem;

void
test01()
{
  fs::path p = fs::u8path(u8"");
  VERIFY( p.empty() );

  p = fs::u8path(u8"filename1");
  VERIFY( p.u8string() == u8"filename1" );

  p = fs::u8path(u8"\xf0\x9d\x84\x9e");
  VERIFY( p.u8string() == u8"\U0001D11E" );

  // The following triggers -Wstringop-overread.  See PR 103332.
  std::u8string s1 = u8"filename2";
  p = fs::u8path(s1);
  VERIFY( p.u8string() == u8"filename2" );

  // The following triggers -Wstringop-overread.  See PR 103332.
  std::u8string s2 = u8"filename3";
  p = fs::u8path(s2.begin(), s2.end());
  VERIFY( p.u8string() == u8"filename3" );
}

int
main()
{
  test01();
}
