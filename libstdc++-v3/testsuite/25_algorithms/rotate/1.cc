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

// 25.2.10 rotate

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, forward_iterator_wrapper> Fcontainer; 
typedef test_container<int, bidirectional_iterator_wrapper> Bcontainer; 
typedef test_container<int, random_access_iterator_wrapper> Rcontainer; 



void
test1()
{
  int array[]={1};
  Fcontainer fcon(array, array);
  Bcontainer bcon(array, array);
  Rcontainer rcon(array, array);
  std::rotate(fcon.begin(), fcon.begin(), fcon.end());
  std::rotate(bcon.begin(), bcon.begin(), bcon.end());
  std::rotate(rcon.begin(), rcon.begin(), rcon.end());
}

void
test2()
{
  int array[] = {1};
  Fcontainer fcon(array, array + 1);
  Bcontainer bcon(array, array + 1);
  Rcontainer rcon(array, array + 1);
  std::rotate(fcon.begin(), fcon.begin(), fcon.end());
  std::rotate(bcon.begin(), bcon.begin(), bcon.end());
  std::rotate(rcon.begin(), rcon.begin(), rcon.end());
  std::rotate(fcon.begin(), fcon.end(), fcon.end());
  std::rotate(bcon.begin(), bcon.end(), bcon.end());
  std::rotate(rcon.begin(), rcon.end(), rcon.end());
}

void
test3()
{
  int array[] = {1, 2, 3, 4, 5};
  Fcontainer fcon(array, array + 5);
  Bcontainer bcon(array, array + 5);
  Rcontainer rcon(array, array + 5);
  std::rotate(fcon.begin(), fcon.it(2), fcon.end());
  VERIFY(array[0] == 3 && array[1] == 4 && array[2] == 5 && 
	 array[3] == 1 && array[4] == 2);
  std::rotate(bcon.begin(), bcon.it(2), bcon.end());
  VERIFY(array[0] == 5 && array[1] == 1 && array[2] == 2 && 
	 array[3] == 3 && array[4] == 4);
  std::rotate(rcon.begin(), rcon.it(2), rcon.end());
  VERIFY(array[0] == 2 && array[1] == 3 && array[2] == 4 && 
	 array[3] == 5 && array[4] == 1);
}

void
test4()
{
  int array[] = {1, 2, 3, 4};
  Fcontainer fcon(array, array + 4);
  Bcontainer bcon(array, array + 4);  
  Rcontainer rcon(array, array + 4);

  std::rotate(fcon.begin(), fcon.it(3), fcon.end());
  VERIFY(array[0] == 4 && array[1] == 1 && array[2] == 2 && 
	 array[3] == 3);

  std::rotate(bcon.begin(), bcon.it(3), bcon.end());
  VERIFY(array[0] == 3 && array[1] == 4 && array[2] == 1 && 
	 array[3] == 2);

  std::rotate(rcon.begin(), rcon.it(3), rcon.end());
  VERIFY(array[0] == 2 && array[1] == 3 && array[2] == 4 && 
	 array[3] == 1);
  
}

void
test5()
{
  int array[] = {1, 2, 3, 4};
  Rcontainer con(array, array + 4);
  std::rotate(con.begin(), con.it(2), con.end());
  VERIFY(array[0] == 3 && array[1] == 4 && array[2] == 1 && 
	 array[3] == 2);
}

int 
main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
}
