// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

// 2003-02-05 Stefan Olsson <stefan@snon.net>

#include <vector>
#include <list>
#include <typeinfo>
#include <sstream>
#include <pthread.h>
#include <ext/mt_allocator.h>
#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>
#include <cxxabi.h>
#include <testsuite_performance.h>

using namespace std;
using __gnu_cxx::__mt_alloc;
using __gnu_cxx::new_allocator;
using __gnu_cxx::malloc_allocator;

typedef int test_type;

// The number of iterations to be performed.
int iterations = 25000;

// The number of values to insert in the container, 32 will cause 5
// (re)allocations to be performed (sizes 4, 8, 16, 32 and 64)
// This means that all allocations are within _MAX_BYTES = 128 as
// defined in stl_alloc.h for __pool_alloc.  Whether or not this
// value is relevant in "the real world" or not I don't know and
// should probably be investigated in more detail.
int insert_values = 128;

template<typename Container>
  void*
  do_loop(void* p = NULL)
  {
    Container obj;    
    try
      {
	int test_iterations = 0;
	while (test_iterations < iterations)
	  {
	    for (int j = 0; j < insert_values; ++j)
	      obj.push_back(test_iterations);
	    ++test_iterations;
	  }
	obj.clear();
	test_iterations = 0;
	while (test_iterations < iterations)
	  {
	    for (int j = 0; j < insert_values; ++j)
	      obj.push_back(test_iterations);
	    ++test_iterations;
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
    pthread_create(&t1, 0, &do_loop<Container>, 0);
    pthread_create(&t2, 0, &do_loop<Container>, 0);
    pthread_create(&t3, 0, &do_loop<Container>, 0);
    pthread_create(&t4, 0, &do_loop<Container>, 0);

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

// http://gcc.gnu.org/ml/libstdc++/2001-05/msg00105.html
// http://gcc.gnu.org/ml/libstdc++/2003-05/msg00231.html
int main(void)
{
#ifdef TEST_T1
  test_container(vector<test_type>());
#endif
#ifdef TEST_T2
  test_container(vector<test_type, malloc_allocator<test_type> >());
#endif
#ifdef TEST_T3
  test_container(vector<test_type, new_allocator<test_type> >());
#endif
#ifdef TEST_T4
  test_container(vector<test_type, __mt_alloc<test_type> >());
#endif

#ifdef TEST_T5
  test_container(list<test_type>());
#endif
#ifdef TEST_T6
  test_container(list<test_type, malloc_allocator<test_type> >());
#endif
#ifdef TEST_T7
  test_container(list<test_type, new_allocator<test_type> >());
#endif
#ifdef TEST_T8
  test_container(list<test_type, __mt_alloc<test_type> >());
#endif

  return 0;
}
