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

// 25.1.2 find_if

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Container;
int array[] = {0, 0, 0, 1, 0, 1};

bool
predicate(const int& i) 
{ return i == 1; }

void
test1()
{
  Container con(array, array);
  VERIFY(std::find_if(con.begin(), con.end(), 
		      predicate).ptr == array);
}

void
test2()
{
  Container con(array, array + 1);
  VERIFY(std::find_if(con.begin(), con.end(), 
		      predicate).ptr == array + 1);
}

void
test3()
{
  Container con(array, array + 6);
  VERIFY(std::find_if(con.begin(), con.end(),
		      predicate).ptr == array + 3);
}

int 
main()
{
  test1();
  test2();
  test3();
}
