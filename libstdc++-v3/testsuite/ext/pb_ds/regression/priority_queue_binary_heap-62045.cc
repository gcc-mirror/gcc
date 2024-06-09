// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// { dg-require-normal-mode "" }

#include <ext/pb_ds/priority_queue.hpp>
#include <testsuite_hooks.h>

int count = 0;

struct less
{
  bool operator()(int i, int j) const
  {
    ++count;
    return i < j;
  }
};

void
test01()
{
  __gnu_pbds::priority_queue<int, less, __gnu_pbds::binary_heap_tag> c;
  c.push(1);
  c.push(2);
  c.push(3);
  c.push(4);
  count = 0;
  c.push(5);
  VERIFY( count < c.size() );
}

int
main()
{
  test01();
}
