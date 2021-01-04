// { dg-do run { target c++11 } }
// { dg-require-fileio "" }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
#include <fstream>
#include <algorithm>
#include <deque>

#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;

  ifstream fbuf_ref("istream_unformatted-1.txt"),
	   fbuf("istream_unformatted-1.txt");

  char buffer_ref[16500];
  deque<char> dq(16500, 'a');

  fbuf_ref.read(buffer_ref, 16500);

  in_iterator_type beg(fbuf);
  copy_n(beg, 16500, dq.begin());

  VERIFY( fbuf_ref.good() );
  VERIFY( fbuf.good() );

  VERIFY( equal(dq.begin(), dq.end(), buffer_ref) );
}

int
main()
{
  test01();
  return 0;
}
