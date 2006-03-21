// 2006-03-20  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <iterator>
#include <sstream>
#include <algorithm>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25482
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;
  typedef ostreambuf_iterator<char> out_iterator_type;

  const char data1[] = "Drei Phantasien nach Friedrich Holderlin";
  const string str1(data1);
  istringstream iss1(str1);
  in_iterator_type beg1(iss1);
  in_iterator_type end1;

  ostringstream oss1;
  out_iterator_type out1(oss1);

  out1 = copy(beg1, beg1, out1);
  VERIFY( oss1.str().empty() );

  out1 = copy(end1, end1, out1);
  VERIFY( oss1.str().empty() );

  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );
  *out1 = 'x';
  VERIFY( oss1.str() == str1 + 'x' );
  oss1.str("");

  iss1.seekg(0);
  oss1.seekp(0);
  oss1.str("");
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );
  *out1 = 'y';
  VERIFY( oss1.str() == str1 + 'y' );
  oss1.str("");
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == "" );

  iss1.seekg(0);
  out1 = copy(beg1, end1, out1);
  VERIFY( oss1.str() == str1 );
}

int main()
{
  test01();
  return 0;
}
