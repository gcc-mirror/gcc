// 2006-03-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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
  in_iterator_type end1, it1;

  it1 = find(beg1, beg1, L'l');
  VERIFY( it1 == beg1 );
  VERIFY( *it1 == L'D' );

  it1 = find(end1, end1, L'D');
  VERIFY( it1 == end1 );

  it1 = find(end1, end1, L'Z');
  VERIFY( it1 == end1 );

  it1 = find(beg1, end1, L'P');
  VERIFY( *it1 == L'P' );
  it1 = find(beg1, end1, L't');
  VERIFY( *it1 == L't' );
  ++it1;
  VERIFY( *it1 == L'a' );

  it1 = find(beg1, end1, L'H');
  VERIFY( *it1 == L'H' );
  it1 = find(beg1, end1, L'l');
  VERIFY( *it1 == L'l' );
  ++it1;
  it1 = find(beg1, end1, L'l');
  VERIFY( *it1 == L'l' );
  ++it1;
  VERIFY( *it1 == L'i' );
  it1 = find(beg1, end1, L'Z');
  VERIFY( it1 == end1 );

  it1 = find(beg1, end1, L'D');
  VERIFY( it1 == end1 );

  iss1.seekg(0);
  it1 = find(beg1, end1, L'D');
  VERIFY( it1 != end1 );
  VERIFY( *it1 == L'D' );
  ++it1;
  VERIFY( *it1 == L'r' );
}

int main()
{
  test01();
  return 0;
}
