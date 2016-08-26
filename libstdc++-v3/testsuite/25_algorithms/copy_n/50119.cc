// { dg-do run { target c++11 } }

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <algorithm>
#include <vector>
#include <sstream>
#include <iterator>
#include <testsuite_hooks.h>

// libstdc++/50119
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  vector<int> v;
  istringstream s("1 2 3 4 5");

  copy_n(istream_iterator<int>(s), 2, back_inserter(v));
  VERIFY( v.size() == 2 );
  VERIFY( v[0] == 1 );
  VERIFY( v[1] == 2 );

  copy_n(istream_iterator<int>(s), 2, back_inserter(v));
  VERIFY( v.size() == 4 );
  VERIFY( v[0] == 1 );
  VERIFY( v[1] == 2 );
  VERIFY( v[2] == 3 );
  VERIFY( v[3] == 4 );
}

int main()
{
  test01();
  return 0;
}
