// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

// 25.3.9 [lib.alg.permutation.generators]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using std::prev_permutation;

typedef test_container<int, bidirectional_iterator_wrapper> Container;

void
test1()
{
  // Note: The standard is unclear on what should happen in this case.
  // This seems the only really sensible behaviour, and what is done.
  int array[] = {0};
  Container con(array, array);
  VERIFY(!prev_permutation(con.begin(), con.end()));
}

void
test2()
{
  int array[] = {0};
  Container con(array, array + 1);
  VERIFY(!prev_permutation(con.begin(), con.end()));
}

void
test3()
{
  int array[] = {3, 0};
  Container con(array, array + 2);
  VERIFY(prev_permutation(con.begin(), con.end()));
  VERIFY(array[0] == 0 && array[1] == 3);
  VERIFY(!prev_permutation(con.begin(), con.end()));
  VERIFY(array[0] == 3 && array[1] == 0);
}

void
test4()
{
  int array[6] = {5, 4, 3, 2, 1, 0};
  Container con(array, array + 6);
  for(int i = 0 ; i < 719; ++i)
    {
      int temp_array[6];
      std::copy(array, array + 6, temp_array);
      VERIFY(prev_permutation(array, array + 6));
      VERIFY(std::lexicographical_compare(array, array + 6,
					  temp_array, temp_array + 6));
    }
  VERIFY(!prev_permutation(array,array + 6));
  for(int i = 0; i < 6; ++i)
    VERIFY(array[i] == 5 - i);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
}
