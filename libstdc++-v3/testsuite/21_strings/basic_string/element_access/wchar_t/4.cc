// 2004-01-18  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004, 2009 Free Software Foundation, Inc.
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

// 21.3.4 basic_string element access

#include <string>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2004-01/msg00184.html
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  for (int i = 0; i < 2000; ++i)
    {
      wstring str_01;

      for (int j = 0; j < i; ++j)
	str_01 += L'a';

      str_01.reserve(i + 10);

      const wstring str_02(str_01);
      VERIFY( str_02[i] == L'\0' );
    }
}

int main()
{
  test01();
  return 0;
}
