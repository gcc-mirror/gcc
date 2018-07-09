// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-do run { target *-*-mingw* } }
// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const wchar_t name_01[] = L"filebuf_members-1.tst";
const wchar_t name_02[] = L"filebuf_members-1.txt";

// Test member functions.
void test01()
{
  const wchar_t* name_03 = L"filebuf_members-w3"; // empty file, need to create

  std::filebuf fb_01; // in
  std::filebuf fb_02; // out
  std::filebuf fb_03; // in | out

  // bool is_open()
  VERIFY( !fb_01.is_open() );
  VERIFY( !fb_02.is_open() );
  VERIFY( !fb_03.is_open() );

  // filebuf_type* open(const wchar_t* __s, ios_base::openmode __mode)
  fb_01.open(name_01, std::ios_base::in | std::ios_base::ate);
  VERIFY( fb_01.is_open() );

  // Try to open two different files without closing the first:
  // Should keep the old file attached, and disregard attempt to overthrow.
  std::filebuf* f = fb_02.open(name_02, std::ios_base::in | std::ios_base::out
			       | std::ios_base::trunc);
  VERIFY( f );
  VERIFY( fb_02.is_open() );

  f = fb_02.open(name_03, std::ios_base::in | std::ios_base::out);
  VERIFY( !f );
  VERIFY( fb_02.is_open() );

  fb_03.open(name_03, std::ios_base::out | std::ios_base::trunc);
  VERIFY( fb_03.is_open() );
}

void test02()
{
  std::wfilebuf fb;
  fb.open(name_01, std::wios::in);
  VERIFY( fb.is_open() );
}

int
main()
{
  test01();
  test02();
}
