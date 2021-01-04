// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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


#include <testsuite_performance.h>

template<typename Container, int Iter>
  void
  do_loop()
  {
    // XXX
    const int iter = 150000;
    typedef Container container_type;
    container_type obj;
    int ctr = 3;
    while (ctr--)
      {
	for (int i = 0; i < iter; ++i)
	  obj.push_back(rand()%500001);

	//Search for random values that may or may not belong to the list.
	for (int i = 0; i < 50; ++i)
	  std::find(obj.begin(), obj.end(), rand() % 100001);
      
	obj.sort();
      
	//Search for random values that may or may not belong to the list.
	for (int i = 0; i < 50; ++i)
	  {
	    typedef typename container_type::iterator iterator_type;
	    iterator_type _liter = std::find(obj.begin(), obj.end(),
					     rand() % 100001);
	    if (_liter != obj.end())
	      obj.erase(_liter);
	  }
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

  typedef __gnu_test::lists<int, thread_type>::type container_types;
  typedef test_sequence<thread_type> test_type;
  test_type test("sort_search");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

