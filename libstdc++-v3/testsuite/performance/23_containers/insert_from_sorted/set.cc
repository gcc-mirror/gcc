// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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
    // avoid excessive swap file use!
    static const unsigned max_size = 250000; 

    // make results less random while
    static const unsigned iterations = 10;    

    // keeping the total time reasonable
    static const unsigned step = 50000;       

    using namespace std;
    typedef int test_type;
    typedef Container container_type;
    typedef vector<test_type> vector_type;
    
    // Initialize sorted array.
    vector_type v(max_size, 0);
    for (unsigned int i = 0; i != max_size; ++i)
      v[i] = i; 
    
    for (unsigned int count = step; count <= max_size; count += step)
      {
	for (unsigned i = 0; i != iterations; ++i)
	  {
	    container_type test_set;
	    typename container_type::iterator iter = test_set.end();
	    
	    // Each insert in amortized constant time (Table 69)
	    for (unsigned j = 0; j != count; ++j)
	      iter = test_set.insert(iter, v[j]);
	  }
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
  test_type test("insert_from_sorted");
  __gnu_cxx::typelist::apply(test, container_types());

  return 0;
}

