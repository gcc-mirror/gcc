// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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
    typedef int test_type;
    typedef Container container_type;
    static const unsigned max_size = 250000; // avoid excessive swap file use!
    static const unsigned iterations = 10;    // make results less random while
    static const unsigned step = 50000;   // keeping the total time reasonable

    std::vector<test_type> v(max_size, 0);
    for (test_type i = 0; i != max_size; ++i)
      v[i] = i; // initialize sorted array
    
    for (test_type count = step; count <= max_size; count += step)
      {
	// Measure set construction time (linear in count (Table 69))
	for (test_type i = 0; i != iterations; ++i)
	  container_type(v.begin(), v.begin() + count);
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

  typedef __gnu_test::sets<int, thread_type>::type container_types;
  typedef test_sequence<thread_type> test_type;
  test_type test("create_from_sorted");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

