// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 27.6.2.7 standard basic_ostream manipulators

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// based vaguely on this:
// http://gcc.gnu.org/ml/libstdc++/2000-q2/msg00109.html
void test02()
{
  using namespace std;
  typedef wostringstream::int_type int_type;

  wostringstream osst_01;
  const wstring str_00(L"herbie_hancock");
  int_type len1 = str_00.size();
  osst_01 << str_00;
  VERIFY( static_cast<int_type>(osst_01.str().size()) == len1 );

  osst_01 << ends;

  const wstring str_01(L"speak like a child");
  int_type len2 = str_01.size();
  osst_01 << str_01;
  int_type len3 = osst_01.str().size();
  VERIFY( len1 < len3 );
  VERIFY( len3 == len1 + len2 + 1 );

  osst_01 << ends;

  const wstring str_02(L"+ inventions and dimensions");
  int_type len4 = str_02.size();
  osst_01 << str_02;
  int_type len5 = osst_01.str().size();
  VERIFY( len3 < len5 );
  VERIFY( len5 == len3 + len4 + 1 );
}

int main()
{ 
  test02();
  return 0;
}
