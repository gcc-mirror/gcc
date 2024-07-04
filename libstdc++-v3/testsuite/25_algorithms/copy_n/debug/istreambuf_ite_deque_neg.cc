// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-fileio "" }
// { dg-require-debug-mode "" }

#include <iterator>
#include <fstream>
#include <algorithm>
#include <deque>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;

  ifstream fbuf_ref("istream_unformatted-1.txt"),
	   fbuf("istream_unformatted-1.txt");

  char buffer_ref[16500];
  deque<char> dq(17000, 'a');

  fbuf_ref.read(buffer_ref, 16500);

  in_iterator_type beg(fbuf);
  copy_n(beg, 17000, dq.begin());
}

int
main()
{
  test01();
  return 0;
}
