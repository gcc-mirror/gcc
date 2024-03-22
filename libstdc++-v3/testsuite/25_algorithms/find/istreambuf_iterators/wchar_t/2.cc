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

// { dg-require-fileio "" }

#include <iterator>
#include <fstream>
#include <algorithm>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25482
void test01()
{
  using namespace std;

  typedef istreambuf_iterator<wchar_t> in_iterator_type;

  wifstream fbuf("istream_unformatted-1.txt");

  in_iterator_type beg(fbuf);
  in_iterator_type end;

  unsigned found = 0;
  for (;;)
    {
      beg = find(beg, end, L'1');
      if (beg == end)
	break;
      
      ++found;
      VERIFY( *beg == L'1' );

      advance(beg, 9);
      VERIFY( *beg == L'0' );
    }
  VERIFY( found == 1500 );
}

int main()
{
  test01();
  return 0;
}
