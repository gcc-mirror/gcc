// 2005-12-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.2.1.1 deque constructors, copy, and assignment

#include <deque>
#include <cstdlib>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  int data3[1000];
  fill(data3, data3 + 1000, 3);

  int data5[1000];
  fill(data5, data5 + 1000, 5);

  for (deque<int>::size_type i = 0; i < 1000; ++i)
    {
      deque<int> d(rand() % 500, 1);
      d.assign(i, i % 2 ? 3 : 5);

      VERIFY( d.size() == i );
      VERIFY( equal(d.begin(), d.end(), i % 2 ? data3 : data5) );
    }
}

int main()
{
  test01();
  return 0;
}
