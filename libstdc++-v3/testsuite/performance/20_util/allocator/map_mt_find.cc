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

#include <new>
#include <map>
#include <cstdlib>
#include <string>
#include <pthread.h>
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

bool less_int(int x1, int x2) { return x1 < x2; }

#if defined USE_FUNCTION_COMPARE
#define COMPARE_T typeof(&less_int)
#define COMPARE_F &less_int
#else
#define COMPARE_T std::less<int>
#define COMPARE_F std::less<int>()
#endif

template <typename Alloc>
  void*
  find_proc(void *_vptr)
  {
    map<int, string, COMPARE_T, Alloc> *_mptr = 
      reinterpret_cast<map<int, string, COMPARE_T, Alloc>*>(_vptr);

    for (int i = 0; i < 700000; ++i)
      {
	int Finder = rand() % 2000000;
	_mptr->find(Finder);
      }
    return _vptr;
  }


template <typename Alloc>
  int
  do_test()
  {
    COMPARE_T _comp = COMPARE_F;
    map<int, string, COMPARE_T, Alloc> imap(_comp);
    int x = 2;
    pthread_t thr[3];
    const int Iter = 1000000;

    while (x--)
      {
	for (int i = 0; i < 300000; ++i)
	  imap.insert(make_pair(rand()%1000000, "_test_data"));

	for (int i = 0; i < 100000; ++i)
	  imap.insert(make_pair(rand()%2000000, "_test_data"));

	pthread_create(&thr[0], NULL, find_proc<Alloc>, (void*)&imap);
	pthread_create(&thr[1], NULL, find_proc<Alloc>, (void*)&imap);
	pthread_create(&thr[2], NULL, find_proc<Alloc>, (void*)&imap);

	pthread_join(thr[0], 0);
	pthread_join(thr[1], 0);
	pthread_join(thr[2], 0);

	imap.clear();
      }
    return Iter;
  }

template <typename Alloc>
  void
  exec_tests()
  {
    using namespace __gnu_test;
    int status;
    COMPARE_T _comp = COMPARE_F;
    map<int, string, COMPARE_T, Alloc> obj(_comp);

    time_counter time;
    resource_counter resource;
    clear_counters(time, resource);
    start_counters(time, resource);
    int test_iterations = do_test<Alloc>();
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
  typedef pair<const int, string> pair_type;

#ifdef TEST_T0
  exec_tests<new_allocator<pair_type> >();
#endif
#ifdef TEST_T1
  exec_tests<malloc_allocator<pair_type> >();
#endif
#ifdef TEST_T2
  exec_tests<__mt_alloc<pair_type> >();
#endif
#ifdef TEST_T3
  exec_tests<bitmap_allocator<pair_type> >();
#endif
#ifdef TEST_T4
  exec_tests<__pool_alloc<pair_type> >();
#endif
}
