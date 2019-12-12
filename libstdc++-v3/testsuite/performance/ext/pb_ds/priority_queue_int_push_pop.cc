// -*- C++ -*-

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file priority_queue_random_int_push_pop_timing_test.cpp
 * Contains test for finding random_int.
 */

#include <ext/typelist.h>
#include <performance/io/xml_formatter.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <common_type/priority_queue/common_type.hpp>
#include <performance/priority_queue/timing/push_pop_test.hpp>
#include <native_type/native_priority_queue.hpp>
#include <testsuite_rng.h>
#include <iostream>
#include <vector>

void
usage();

int
main(int argc, char* a_p_argv[])
{
  using namespace __gnu_pbds::test;

  size_t vn = 200;
  size_t vs = 200;
  size_t vm = 2100;

  try
    {
      xml_test_performance_formatter fmt("Size", "Average time (sec.)");

      typedef std::vector<std::pair<int, char> > vector_type;
      vector_type a_v(vm);
      twister_rand_gen g;

      for (size_t i = 0; i < vm; ++i)
	a_v[i] = std::make_pair(static_cast<int>(g.get_unsigned_long()), 0);
      vector_type::const_iterator b = a_v.begin();

      typedef push_pop_test<vector_type::const_iterator> test_type;
      test_type tst(b, vn, vs, vm);
      {
	typedef pq_common_types<int>::performance_tl pq_tl_t;
	pq_tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef native_priority_queue<int, true> native_pq_t;
	tst(native_pq_t());
      }

      {
	typedef native_priority_queue<int, false> native_pq_t;
	tst(native_pq_t());
      }
    }
  catch(...)
    {
      std::cerr << "Test failed" << std::endl;
      return -1;
    }
  return 0;
}

void
usage()
{
  std::cerr << "usage: priority_queue_random_int_push_pop_timing_test <vn> <vs> <vm>" <<
    std::endl << std::endl;

  std::cerr <<
    "This test checks the performance of various priority_queue containers "
    "using their push and pop method. " << std::endl;
  std::cerr << "Specifically, it does the following:" << std::endl;
  std::cerr << "*  Creates a vector of random integers " << std::endl;
  std::cerr << "*  Pushes the elements into the container" << std::endl;
  std::cerr << "*  Pops the elements from the container" << std::endl;
  std::cerr << "*  Repeats the above test a number of times "
	    << std::endl;

  std::cerr << std::endl << std::endl;

  std::cerr << "vn = minimum size of the vector" << std::endl;
  std::cerr << "vs = step size of the vector" << std::endl;
  std::cerr << "vm = maximum size of the vector" << std::endl;
}
