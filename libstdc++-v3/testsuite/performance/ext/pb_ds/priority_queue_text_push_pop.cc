// -*- C++ -*-

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
 * @file priority_queue_text_push_pop_timing_test.cpp
 * Contains test for finding text.
 */

#include <ext/typelist.h>
#include <performance/io/xml_formatter.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <common_type/priority_queue/common_type.hpp>
#include <performance/priority_queue/timing/push_pop_test.hpp>
#include <io/text_populate.hpp>
#include <native_type/native_priority_queue.hpp>
#include <iostream>
#include <vector>

void
usage();

int
main(int argc, char* a_p_argv[])
{
  using namespace __gnu_pbds::test;
  std::string f_name = "thirty_years_among_the_dead_preproc.txt";
  size_t vn = 200;
  size_t vs = 200;
  size_t vm = 2100;

  try
    {
      xml_test_performance_formatter fmt("Size", "Average time (sec.)");

      typedef std::vector<std::pair<std::string, char> > vec_t;
      vec_t a_v(vm);
      text_populate(f_name, a_v);

      typedef push_pop_test<vec_t::const_iterator> test_t;
      vec_t::const_iterator b = a_v.begin();
      test_t tst(b, vn, vs, vm);
      {
	typedef pq_common_types<std::string>::performance_tl pq_tl_t;
	pq_tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef native_priority_queue<std::string, true> native_pq_t;
	tst(native_pq_t());
      }

      {
	typedef native_priority_queue<std::string, false> native_pq_t;
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
  using namespace std;
  cerr << "usage: priority_queue_text_push_pop_timing_test "
          "<f_name> <vn> <vs> <vm>" << endl << endl;

  cerr << "This test checks the performance of various"
    "priority_queue containers using their push method. " << endl;
  cerr << "Specifically, it does the following:" << endl;
  cerr << "*  Creates a vector of text words " << endl;
  cerr << "*  Pushes the elements into the container, then pops until it's empty." << endl;
  cerr << "*  Repeats the above test a number of times) "
	    << endl;

  cerr << endl << endl;

  cerr << "f_name = file name containing the text words. "
    "Each line should contain one word." << endl;
  cerr << "vn = minimum size of the vector" << endl;
  cerr << "vs = step size of the vector" << endl;
  cerr << "vm = maximum size of the vector" << endl;
}
