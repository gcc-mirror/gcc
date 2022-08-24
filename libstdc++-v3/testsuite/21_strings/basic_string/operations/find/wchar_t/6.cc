// 2017-01-06  Jonathan Wakely  <jwakely@redhat.com>

// Copyright (C) 2017-2022 Free Software Foundation, Inc.
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

// C++11 21.4.7.2 [string::find] basic_string find

#include <testsuite_string.h>
#include <testsuite_hooks.h>

// https://gcc.gnu.org/ml/libstdc++/2017-01/msg00021.html
void test01()
{
  typedef __gnu_test::wstring string_type;
  string_type::size_type npos = string_type::npos;

  string_type use = L"aaa";
  string_type::size_type pos1 = use.find(L"ab");

  VERIFY( pos1 == npos );
}

int main()
{
  test01();
  return 0;
}
