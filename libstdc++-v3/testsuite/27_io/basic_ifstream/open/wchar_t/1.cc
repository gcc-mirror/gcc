// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// 30.9.3.1 basic_ifstream constructors [ifstream.cons]

// { dg-do run { target *-*-mingw* } }
// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const wchar_t name_01[] = L"ifstream_members-1.tst";

void test01()
{
  std::ifstream ifs1;
  ifs1.close();

  VERIFY( !ifs1.is_open() );
  VERIFY( !(ifs1) );

  ifs1.open(name_01);
  VERIFY( ifs1.is_open() );

  VERIFY( (ifs1) );
  VERIFY( ifs1.rdstate() == std::ios_base::goodbit );

  ifs1.close();
}

void test02()
{
  std::wifstream wifs1;
  wifs1.open(name_01, std::wios::in);
  VERIFY( wifs1.is_open() );

  VERIFY( (wifs1) );
  VERIFY( wifs1.rdstate() == std::ios_base::goodbit );
}

int main()
{
  test01();
  test02();
}
