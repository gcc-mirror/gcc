// Copyright (C) 2013-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-D_GLIBCXX_DEBUG_PEDANTIC" }
// { dg-do run { target c++11 } }

#include <string>
#include <testsuite_hooks.h>

// PR c++/58163
void test01()
{
  const std::string cs;
        std::string  s;

  VERIFY( cs[0] == '\0' );
  VERIFY(  s[0] == '\0' );
}

int main()
{
  test01();
  return 0;
}
