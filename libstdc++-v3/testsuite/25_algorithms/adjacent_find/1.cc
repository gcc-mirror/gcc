// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 25.1.5 [lib.alg.adjacent_find]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::adjacent_find;

typedef test_container<int, forward_iterator_wrapper> Container;
int array[] = {0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1};

void 
test01()
{
  Container con(array, array);
  VERIFY(adjacent_find(con.begin(), con.end()).ptr == array);
}  

void 
test02()
{
  Container con(array, array + 1);
  VERIFY(adjacent_find(con.begin(), con.end()).ptr == array + 1);
}

void 
test03()
{
  Container con(array, array + 2);
  VERIFY(adjacent_find(con.begin(), con.end()).ptr == array);
}

void 
test04()
{
  Container con(array + 1, array + 10);
  VERIFY(adjacent_find(con.begin(), con.end()).ptr == array + 10);
}

int 
main()
{
  test01();
  test02();
  test03();
  test04();
}
