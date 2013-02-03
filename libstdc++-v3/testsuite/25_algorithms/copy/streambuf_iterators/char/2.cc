// 2006-03-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
#include <cstring>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25482
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;

  const char data1[] = "Drei Phantasien nach Friedrich Holderlin";
  const string str1(data1);
  istringstream iss1(str1);
  in_iterator_type beg1(iss1);
  in_iterator_type end1;

  char buffer1[sizeof(data1) * 5];
  memset(buffer1, '*', sizeof(buffer1));
  char* out1 = buffer1;

  out1 = copy(beg1, beg1, out1);
  VERIFY( out1 == buffer1 );

  out1 = copy(end1, end1, out1);
  VERIFY( out1 == buffer1 );

  out1 = copy(beg1, end1, out1);
  VERIFY( string(buffer1, out1) == str1 );
  *out1++ = 'x';
  VERIFY( string(buffer1, out1) == str1 + 'x' );
  memset(buffer1, '*', sizeof(buffer1));

  iss1.seekg(0);
  out1 = buffer1;
  memset(buffer1, '*', sizeof(buffer1));
  out1 = copy(beg1, end1, out1);
  VERIFY( string(buffer1, out1) == str1 );
  *out1++ = 'y';
  VERIFY( string(buffer1, out1) == str1 + 'y' );
  out1 = buffer1;
  memset(buffer1, '*', sizeof(buffer1));
  out1 = copy(beg1, end1, out1);
  VERIFY( string(buffer1, out1) == "" );

  iss1.seekg(0);
  out1 = copy(beg1, end1, out1);
  VERIFY( string(buffer1, out1) == str1 );
}

int main()
{
  test01();
  return 0;
}
