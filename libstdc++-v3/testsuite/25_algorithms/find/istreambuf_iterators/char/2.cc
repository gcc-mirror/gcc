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
#include <fstream>
#include <algorithm>
#include <testsuite_hooks.h>

// { dg-require-fileio "" }

// In the occasion of libstdc++/25482
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;

  ifstream fbuf("istream_unformatted-1.txt");

  in_iterator_type beg(fbuf);
  in_iterator_type end;

  unsigned found = 0;
  for (;;)
    {
      beg = find(beg, end, '1');
      if (beg == end)
	break;
      
      ++found;
      VERIFY( *beg == '1' );

      for (unsigned sk = 0; sk < 9; sk++)
	++beg;
      VERIFY( *beg == '0' );
    }
  VERIFY( found == 1500 );
}

int main()
{
  test01();
  return 0;
}
