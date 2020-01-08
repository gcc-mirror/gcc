// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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
    Container obj; // XXX
    int insert_values = 128; // XXX

    int test_iterations = 0;
    
    // XXX
    //    typedef typename Container::value_type test_type;
    typedef int test_type;

    value_type<test_type> test_value;
    while (test_iterations < Iter)
      {
	for (int j = 0; j < insert_values; ++j)
	  obj.insert(++test_value);
	++test_iterations;
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

  using __gnu_test::associative_containers;
  typedef associative_containers<int, thread_type>::type container_types;

  typedef test_sequence<thread_type> test_type;
  test_type test("insert_associative");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

