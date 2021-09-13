// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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


// 2004-03-11  Dhruv Matani  <dhruvbird@HotPOP.com>

#include <testsuite_performance.h>

template<typename Container, int Iter>
  void
  do_loop()
  {
    Container obj;
    int x = 2;
    while (x--)
      {
	for (int i = 0; i < 300000; ++i)
	  obj.insert(std::make_pair(rand()%1000000, i));
	for (int i = 0; i < 100000; ++i)
	  obj.insert(std::make_pair(rand()%2000000, i));
	obj.clear();
      }
  } 

int
main()
{ 
#ifdef TEST_S1
#define thread_type false
#endif    

#ifdef TEST_T1
#define thread_type true
#endif    

  typedef __gnu_test::maps<int, thread_type>::type container_types;
  typedef test_sequence<thread_type> test_type;
  test_type test("find");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

