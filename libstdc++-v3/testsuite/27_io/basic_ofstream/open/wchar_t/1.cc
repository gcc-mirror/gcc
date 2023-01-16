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

// 30.9.4.1 basic_ofstream constructors [ofstream.cons]

// { dg-do run { target *-*-mingw* } }
// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const wchar_t name_02[] = L"ofstream_members-1.txt";

void test01()
{
  std::ofstream ofs1;
  ofs1.close();

  VERIFY( !ofs1.is_open() );
  VERIFY( !(ofs1) );

  ofs1.open(name_02);
  VERIFY( ofs1.is_open() );

  VERIFY( (ofs1) );
  VERIFY( ofs1.rdstate() == std::ios_base::goodbit );

  ofs1.close();
}

void test02()
{
  std::wofstream wofs1;
  wofs1.open(name_02, std::wios::out);
  VERIFY( wofs1.is_open() );

  VERIFY( (wofs1) );
  VERIFY( wofs1.rdstate() == std::ios_base::goodbit );
}

int main()
{
  test01();
  test02();
}
