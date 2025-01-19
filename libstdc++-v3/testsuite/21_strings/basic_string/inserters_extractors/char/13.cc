// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run }

#include <string>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  const std::string str = "1234";
  std::istringstream in(str);
  std::string buf;
  in.width(4);
  in >> buf;
  VERIFY( !in.eof() ); // should stop after reading 4 chars
  VERIFY( buf == str );
}

struct CT : std::char_traits<char> { };

void
test02()
{
  const std::basic_string<char, CT> str = "1234";
  std::basic_istringstream<char, CT> in(str);
  std::basic_string<char, CT> buf;
  in.width(4);
  in >> buf;
  VERIFY( !in.eof() ); // should stop after reading 4 chars
  VERIFY( buf == str );
}

int
main()
{
  test01();
  test02();
}
