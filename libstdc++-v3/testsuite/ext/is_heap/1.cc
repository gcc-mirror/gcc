// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, random_access_iterator_wrapper> container;

void 
test1()
{
  int array[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  for(int i = 0; i < 10; ++i)
    {
      container con(array, array + i);
      std::make_heap(con.begin(), con.end());
      VERIFY(std::__is_heap(con.begin(), con.end()));
      VERIFY(std::__is_heap(con.begin(), i));
    }
}

int 
main()
{
  test1();
  return 0;
}
