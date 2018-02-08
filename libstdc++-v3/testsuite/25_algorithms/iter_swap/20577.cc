// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void
test1()
{
  std::vector<bool> v;
  v.push_back(true);
  v.push_back(false);
  std::iter_swap(v.begin(), v.begin() + 1);
  VERIFY( v[0] == false && v[1] == true );
}

void
test2()
{
  std::vector<int> v;
  v.push_back(1);
  v.push_back(2);
  std::iter_swap(v.begin(), v.begin() + 1);
  VERIFY( v[0] == 2 && v[1] == 1 );
}

int int_swap_count;

struct X {};
void swap(X&, X&)
{ ++int_swap_count; }

void
test3()
{
  int_swap_count = 0;
  X i, j;
  std::iter_swap(&i, &j);
  VERIFY( int_swap_count == 1 );
}

// libstdc++/20577
int main()
{
  test1();
  test2();
  test3();
  return 0;
}
