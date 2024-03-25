// 2006-03-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2024 Free Software Foundation, Inc.
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

// In the occasion of libstdc++/25482
void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> in_iterator_type;

  const wchar_t data1[] = L"Drei Phantasien nach Friedrich Holderlin";
  const wstring str1(data1);
  wistringstream iss1(str1);
  in_iterator_type beg1(iss1);
  in_iterator_type end1;

  wchar_t buffer1[sizeof(data1) * 5 / sizeof(wchar_t)];
  wmemset(buffer1, L'*', sizeof(buffer1) / sizeof(wchar_t));
  wchar_t* out1 = buffer1;

  out1 = copy(beg1, beg1, out1);
  VERIFY( out1 == buffer1 );

  out1 = copy(end1, end1, out1);
  VERIFY( out1 == buffer1 );

  out1 = copy(beg1, end1, out1);
  VERIFY( wstring(buffer1, out1) == str1 );
  *out1++ = L'x';
  VERIFY( wstring(buffer1, out1) == str1 + L'x' );
  wmemset(buffer1, L'*', sizeof(buffer1) / sizeof(wchar_t));

  iss1.seekg(0);
  out1 = buffer1;
  wmemset(buffer1, L'*', sizeof(buffer1) / sizeof(wchar_t));
  out1 = copy(beg1, end1, out1);
  VERIFY( wstring(buffer1, out1) == str1 );
  *out1++ = L'y';
  VERIFY( wstring(buffer1, out1) == str1 + L'y' );
  out1 = buffer1;
  wmemset(buffer1, L'*', sizeof(buffer1) / sizeof(wchar_t));
  out1 = copy(beg1, end1, out1);
  VERIFY( wstring(buffer1, out1) == L"" );

  iss1.seekg(0);
  out1 = copy(beg1, end1, out1);
  VERIFY( wstring(buffer1, out1) == str1 );
}

int main()
{
  test01();
  return 0;
}
