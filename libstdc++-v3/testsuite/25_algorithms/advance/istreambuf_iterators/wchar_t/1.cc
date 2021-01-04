// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <iterator>
#include <sstream>
#include <algorithm>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> in_iterator_type;

  const wchar_t data1[] = L"Drei Phantasien nach Friedrich Holderlin";
  const wstring str1(data1);
  wistringstream iss1(str1);
  in_iterator_type beg1(iss1);
  in_iterator_type end1;

  VERIFY( *beg1 == L'D' );

  advance(beg1, 1);

  VERIFY( beg1 != end1 );
  VERIFY( *beg1 == L'r' );

  advance(beg1, 0);
  VERIFY( *beg1 == L'r' );

  advance(beg1, 38);
  VERIFY( *beg1 == L'n' );

  advance(beg1, 1);
  VERIFY( beg1 == end1 );
}

int main()
{
  test01();
  return 0;
}
