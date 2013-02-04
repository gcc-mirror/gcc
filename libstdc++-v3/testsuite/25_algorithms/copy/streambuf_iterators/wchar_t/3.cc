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
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25482
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef ostreambuf_iterator<wchar_t> out_iterator_type;

  const wchar_t data1[] = L"Drei Phantasien nach Friedrich Holderlin";
  const wstring str1(data1);
  const wchar_t* beg1 = data1;
  const wchar_t* end1 = beg1 + str1.size();

  wostringstream oss1;
  out_iterator_type out1(oss1);

  out1 = copy(beg1, beg1, out1);
  VERIFY( oss1.str().empty() );

  out1 = copy(end1, end1, out1);
  VERIFY( oss1.str().empty() );
  
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );
  *out1 = L'x';
  VERIFY( oss1.str() == str1 + L'x' );
  oss1.str(L"");

  oss1.seekp(0);
  oss1.str(L"");
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );
  *out1 = L'y';
  VERIFY( oss1.str() == str1 + L'y' );
  oss1.str(L"");
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );

  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 + str1 );
}

int main()
{
  test01();
  return 0;
}
