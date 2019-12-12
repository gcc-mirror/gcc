// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -lstdc++fs" }
// { dg-do run { target c++17 } }
// { dg-require-fileio "" }
// { dg-require-filesystem-ts "" }

#include <fstream>
#include <filesystem>
#include <testsuite_hooks.h>

char cstr[] = "filebuf_members-1.tst";
const std::filesystem::path filename = cstr;

void
test01()
{
  std::ifstream f;
  f.open(filename);
  VERIFY( f.is_open() );
}

void
test02()
{
  std::ifstream f;
  f.open(filename, std::ios::in);
  VERIFY( f.is_open() );
}

void
test03() // compile-only
{
  std::ifstream f;
  f.open(cstr);		      // PR libstdc++/83025
  f.open(cstr, std::ios::in); // PR libstdc++/83025
}

int
main()
{
  test01();
  test02();
}
