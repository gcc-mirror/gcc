// Copyright (C) 2004 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 * The goal with this application is to compare the performance
 * between different std::allocator implementations. The results are
 * influenced by the underlying allocator in the "C" library, malloc.
 */

// libstdc++/13823 recast for this testing framework

#include <map>
#include <iostream>
#include <typeinfo>
#include <sstream>
#include <ext/mt_allocator.h>
#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>
#include <ext/bitmap_allocator.h>
#include <ext/pool_allocator.h>
#include <cxxabi.h>
#include <testsuite_performance.h>

using namespace std;
using __gnu_cxx::__mt_alloc;
using __gnu_cxx::new_allocator;
using __gnu_cxx::malloc_allocator;
using __gnu_cxx::bitmap_allocator;
using __gnu_cxx::__pool_alloc;

// The number of iterations to be performed.
int iterations = 10000;

template<typename Container>
  void*
  do_loop(void* p = NULL)
  {
    try
      {
	for (int c = 0; c < 10; c++)
	  {
	    Container m;

	    for (unsigned i = 0; i < iterations; ++i) 
	      m[i] = i;
	  }
      }
    catch(...)
      {
	// No point allocating all available memory, repeatedly.	
      }
  }

template<typename Container>
  void
  test_container(Container obj)
  {
    using namespace __gnu_test;
    int status;

    time_counter time;
    resource_counter resource;

    clear_counters(time, resource);
    start_counters(time, resource);
    
    pthread_t  t1, t2, t3, t4;
    pthread_create(&t1, NULL, &do_loop<Container>, NULL);
    pthread_create(&t2, NULL, &do_loop<Container>, NULL);
    pthread_create(&t3, NULL, &do_loop<Container>, NULL);
    pthread_create(&t4, NULL, &do_loop<Container>, NULL);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    pthread_join(t3, NULL);
    pthread_join(t4, NULL);

    stop_counters(time, resource);
 
    std::ostringstream comment;
    comment << "iterations: " << iterations << '\t';
    comment << "type: " << abi::__cxa_demangle(typeid(obj).name(),
					       0, 0, &status);
    report_header(__FILE__, comment.str());
    report_performance(__FILE__, string(), time, resource);
  }

int main(void)
{
  typedef pair<const int, int> pair_type;
  
#ifdef TEST_T1
  test_container(map<int, int>());
#endif
#ifdef TEST_T2
  test_container(map<int, int, less<const int>,
		 new_allocator<pair_type> >());
#endif
#ifdef TEST_T3
  test_container(map<int, int, less<const int>,
		 malloc_allocator<pair_type> >());
#endif
#ifdef TEST_T4
  test_container(map<int, int, less<const int>,
		 __mt_alloc<pair_type> >());
#endif
#ifdef TEST_T5
  test_container(map<int, int, less<const int>,
		 bitmap_allocator<pair_type> >());
#endif
#ifdef TEST_T6
  test_container(map<int, int, less<const int>,
		 __pool_alloc<pair_type> >());
#endif
  return 0;
}
