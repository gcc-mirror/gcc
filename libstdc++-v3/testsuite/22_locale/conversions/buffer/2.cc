// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-do run }

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  struct Cvt : std::codecvt<char, char, std::mbstate_t> { };
  std::stringstream ss;
  std::wbuffer_convert<Cvt, char> cvt(ss.rdbuf());
  auto p = ss.std::ios::rdbuf(&cvt);
  ss << "hello";
  VERIFY( ss.flush().good() );
  ss.std::ios::rdbuf(p);
}

int main()
{
  test01();
}
