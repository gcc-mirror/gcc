// -*- C++ -*-

// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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
 * @file tree_split_join_timing_test.cpp
 * Contains test for joining trees.
 */

#include <ext/typelist.h>
#include <performance/io/xml_formatter.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <testsuite_rng.h>
#include <common_type/assoc/common_type.hpp>
#include <performance/assoc/timing/tree_split_join_test.hpp>
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

      {
	typedef tree_split_join_test<true> test_t;
	test_t tst(vn, vs, vm);

	typedef tree_common_types<int, __gnu_pbds::null_type>::performance_tl tl_t;
	tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef tree_split_join_test<false> test_t;
	test_t tst(vn, vs, vm);
	typedef native_set<int> native_set_t;
	tst(native_set_t());
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
  cerr << "usage: tree_split_join_test.cpp <vn> <vs> <vm>" <<
    endl << endl;

  cerr << "This test checks the performance of splitting joining" 
          "tree based containers. " << endl;
  cerr << "Specifically, it does the following:" << endl;
  cerr << "*  Creates a tree " << endl;
  cerr << "*  Inserts integers into the tree" << endl;
  cerr << "*  Splits half the tree into a different tree" << endl;
  cerr << "*  Joins the trees" << endl;
  cerr << "*  Repeats the above test a given number of times) "
	    << endl;

  cerr << endl << endl;

  cerr << "vn = minimum size of the tree" << endl;
  cerr << "vs = step size of the tree" << endl;
  cerr << "vm = maximum size of the tree" << endl;
}
