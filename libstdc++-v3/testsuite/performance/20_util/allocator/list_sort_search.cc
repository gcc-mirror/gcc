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

// 2004-03-11  Dhruv Matani  <dhruvbird@HotPOP.com>

#include <list>
#include <algorithm>
#include <cstdlib>
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
using __gnu_cxx::malloc_allocator;
using __gnu_cxx::new_allocator;
using __gnu_cxx::__mt_alloc;
using __gnu_cxx::bitmap_allocator;
using __gnu_cxx::__pool_alloc;

typedef int test_type;

template <typename Alloc>
  int
  Test_Allocator()
  {
    typedef list<int, Alloc> My_List;
    My_List il1;
    int const Iter = 150000;

    int ctr = 3;
    while (ctr--)
      {
	for (int i = 0; i < Iter; ++i)
	  il1.push_back(rand()%500001);

	//Search for random values that may or may not belong to the list.
	for (int i = 0; i < 50; ++i)
	  std::find(il1.begin(), il1.end(), rand() % 100001);
      
	il1.sort();
      
	//Search for random values that may or may not belong to the list.
	for (int i = 0; i < 50; ++i)
	  {
	    typename My_List::iterator _liter = std::find(il1.begin(),
							  il1.end(),
							  rand() % 100001);
	    if (_liter != il1.end())
	      il1.erase(_liter);
	  }
      
	il1.clear();
      }
    return Iter;
  }

template <typename Alloc>
  void
  do_test()
  {
    using namespace __gnu_test;
    int status;
    Alloc obj;

    time_counter time;
    resource_counter resource;
    clear_counters(time, resource);
    start_counters(time, resource);
    int test_iterations = Test_Allocator<Alloc>();
    stop_counters(time, resource);
 
    std::ostringstream comment;
    comment << "iterations: " << test_iterations << '\t';
    comment << "type: " << abi::__cxa_demangle(typeid(obj).name(),
					       0, 0, &status);
    report_header(__FILE__, comment.str());
    report_performance(__FILE__, string(), time, resource);
  }

int main()
{
#ifdef TEST_S0
  do_test<new_allocator<int> >();
#endif
#ifdef TEST_S1
  do_test<malloc_allocator<int> >();
#endif
#ifdef TEST_S2
  do_test<__mt_alloc<int> >();
#endif
#ifdef TEST_S3
  do_test<bitmap_allocator<int> >();
#endif
#ifdef TEST_S4
  do_test<__pool_alloc<int> >();
#endif
}
