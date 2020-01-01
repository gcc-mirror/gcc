// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

#include <fstream>
#include <testsuite_hooks.h>

void test01()
{
  std::fstream fs;

  const wchar_t* name = L"fstream_name.txt";
  fs.open(name, std::wios::out);
  VERIFY( fs.is_open() );
}

void test02()
{
  const wchar_t name[] = L"fstream_name.txt";
  std::wfstream wfs;
  wfs.open(name, std::wios::in|std::wios::out);
  VERIFY( wfs.is_open() );
}

int
main()
{
  test01();
  test02();
}
