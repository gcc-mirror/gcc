// 2005-12-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.1.1 deque constructors, copy, and assignment

#include <deque>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  int data3[10000];
  fill(data3, data3 + 10000, 3);

  int data5[10000];
  fill(data5, data5 + 10000, 5);

  for (deque<int>::size_type i = 0; i < 10000; ++i)
    {
      deque<int> d(rand() % 5000, 1);
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
