// { dg-do run }
// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::literals::string_literals;

  std::string planet = "Mercury"s;
  std::wstring wplanet = L"Venus"s;
  std::string u8planet = u8"Mars"s;
  std::u16string u16planet = u"Juiter"s;
  std::u32string u32planet = U"Saturn"s;

  VERIFY( planet == std::string("Mercury") );
  VERIFY( wplanet == std::wstring(L"Venus") );
  VERIFY( u8planet == std::string(u8"Mars") );
  VERIFY( u16planet == std::u16string(u"Juiter") );
  VERIFY( u32planet == std::u32string(U"Saturn") );
}

int
main()
{
  test01();
}
