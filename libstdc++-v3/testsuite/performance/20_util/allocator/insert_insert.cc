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
#include <map>
#include <deque>
#include <set>
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

typedef int test_type;

// The number of iterations to be performed.
int iterations = 10000;

// The number of values to insert in the container, 32 will cause 5
// (re)allocations to be performed (sizes 4, 8, 16, 32 and 64)
// This means that all allocations are within _MAX_BYTES = 128 as
// defined in stl_alloc.h for __pool_alloc.  Whether or not this
// value is relevant in "the real world" or not I don't know and
// should probably be investigated in more detail.
int insert_values = 128;

template<typename TestType>
  struct value_type : public pair<TestType, TestType>
  {
    value_type() : pair<TestType, TestType>(0, 0) { }

    inline value_type operator++() { return ++this->first, *this; }
    inline operator TestType() const { return this->first; }
  };

template<typename Container>
  void
  do_loop()
  {
    Container obj;
    int test_iterations = 0;
    value_type<test_type> test_value;
    while (test_iterations < iterations)
      {
	for (int j = 0; j < insert_values; ++j)
	  obj.insert(obj.end(), ++test_value);
	++test_iterations;
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
    {
      start_counters(time, resource);
      do_loop<Container>();
      do_loop<Container>();
      stop_counters(time, resource);
      
      std::ostringstream comment;
      comment << "repeated iterations: " << iterations*2 << '\t';
      comment << "type: " << abi::__cxa_demangle(typeid(obj).name(),
						 0, 0, &status);
      report_header(__FILE__, comment.str());
      report_performance(__FILE__, string(), time, resource);
    }
  }

// http://gcc.gnu.org/ml/libstdc++/2001-05/msg00105.html
// http://gcc.gnu.org/ml/libstdc++/2003-05/msg00231.html
int main(void)
{
  typedef __gnu_cxx::malloc_allocator<test_type> m_alloc_type;
  typedef __gnu_cxx::new_allocator<test_type> n_alloc_type;
  typedef __gnu_cxx::__mt_alloc<test_type> so_alloc_type;
  typedef __gnu_cxx::bitmap_allocator<test_type> bit_alloc_type;
  typedef __gnu_cxx::__pool_alloc<test_type> po_alloc_type;

#ifdef TEST_S0
  test_container(vector<test_type, m_alloc_type>());
#endif
#ifdef TEST_S1
  test_container(vector<test_type, n_alloc_type>());
#endif
#ifdef TEST_S2
  test_container(vector<test_type, so_alloc_type>());
#endif
#ifdef TEST_S3
  test_container(vector<test_type, bit_alloc_type>());
#endif
#ifdef TEST_S4
  test_container(vector<test_type, po_alloc_type>());
#endif

#ifdef TEST_S5
  test_container(list<test_type, m_alloc_type>());
#endif
#ifdef TEST_S6
  test_container(list<test_type, n_alloc_type>());
#endif
#ifdef TEST_S7
  test_container(list<test_type, so_alloc_type>());
#endif
#ifdef TEST_S8
  test_container(list<test_type, bit_alloc_type>());
#endif
#ifdef TEST_S9
  test_container(list<test_type, po_alloc_type>());
#endif

#ifdef TEST_S10
  test_container(deque<test_type, m_alloc_type>());
#endif
#ifdef TEST_S11
  test_container(deque<test_type, n_alloc_type>());
#endif
#ifdef TEST_S12
  test_container(deque<test_type, so_alloc_type>());
#endif
#ifdef TEST_S13
  test_container(deque<test_type, bit_alloc_type>());
#endif
#ifdef TEST_S14
  test_container(deque<test_type, po_alloc_type>());
#endif

  typedef less<test_type> compare_type;
  typedef pair<const test_type, test_type> pair_type;
  typedef __gnu_cxx::malloc_allocator<pair_type> m_pair_alloc_type;
  typedef __gnu_cxx::new_allocator<pair_type> n_pair_alloc_type;
  typedef __gnu_cxx::__mt_alloc<pair_type> so_pair_alloc_type;
  typedef __gnu_cxx::bitmap_allocator<pair_type> bit_pair_alloc_type;
  typedef __gnu_cxx::__pool_alloc<pair_type> po_pair_alloc_type;

#ifdef TEST_S15
  test_container(map<test_type, test_type, compare_type,
		 m_pair_alloc_type>());
#endif
#ifdef TEST_S16
  test_container(map<test_type, test_type, compare_type,
		 n_pair_alloc_type>());
#endif
#ifdef TEST_S17
  test_container(map<test_type, test_type, compare_type,
		 so_pair_alloc_type>());
#endif
#ifdef TEST_S18
  test_container(map<test_type, test_type, compare_type,
		 bit_pair_alloc_type>());
#endif
#ifdef TEST_S19
  test_container(map<test_type, test_type, compare_type,
		 po_pair_alloc_type>());
#endif

#ifdef TEST_S20
  test_container(set<test_type, compare_type, m_alloc_type>());
#endif
#ifdef TEST_S21
  test_container(set<test_type, compare_type, n_alloc_type>());
#endif
#ifdef TEST_S22
  test_container(set<test_type, compare_type, so_alloc_type>());
#endif
#ifdef TEST_S23
  test_container(set<test_type, compare_type, bit_alloc_type>());
#endif
#ifdef TEST_S24
  test_container(set<test_type, compare_type, po_alloc_type>());
#endif
  return 0;
}
