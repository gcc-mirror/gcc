// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

// 21.4.6.5 basic_string::pop_back
// { dg-do run { target c++11 } }

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  const std::wstring cstr(L"Badger");
  std::wstring str = cstr;
  str.pop_back();
  VERIFY( str.size() == cstr.size() - 1 );

  str += cstr.back();
  VERIFY( str == cstr );
}

int main()
{ 
  test01();
  return 0;
}
