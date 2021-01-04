// 2000-08-02 bkoz

// Copyright (C) 2000-2021 Free Software Foundation, Inc.
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
 
#include <iostream>

// Interactive test, to be exercised as follows:
// assign stderr to stdout in shell command line,
// pipe stdout to cat process and/or redirect stdout to file.
// a.out >& output
// "hello fine world\n" should be written to stdout, and output, in
// proper order.  This is a version of the scott snyder test taken
// from: http://gcc.gnu.org/ml/libstdc++/1999-q4/msg00108.html
void test04()
{
  using namespace std;

  cout << "hello ";
  cout.flush();
  cerr << "fine ";
  cerr.flush();
  cout << "world" << endl;
  cout.flush();
}

int 
main()
{
  test04();
  return 0;
}
