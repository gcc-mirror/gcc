// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct Inc
{
  int operator()() { return ++i; }

  int i;
};

void
test01()
{
  const int N = 3;
  int array[N];

  using __gnu_test::test_container;
  using __gnu_test::output_iterator_wrapper;
  test_container<int, output_iterator_wrapper> c(array, array + N);
  std::generate_n(c.begin(), N, Inc());
  VERIFY(array[0] == 1);
  VERIFY(array[1] == 2);
  VERIFY(array[2] == 3);
}

int main()
{
  test01();
}
