// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

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
 * @file random_int_find_timing_test.cpp
 * Contains test for finding random integers.
 */

#include <ext/typelist.h>
#include <performance/io/xml_formatter.hpp>
#include <native_type/assoc/native_hash_map.hpp>
#include <native_type/assoc/native_map.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <rng/twister_rand_gen.hpp>
#include <common_type/assoc/common_type.hpp>
#include <performance/assoc/timing/find_test.hpp>
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

      typedef std::vector< std::pair< int, char> > vec_t;
      vec_t a_v(vm);
      twister_rand_gen g;
      for (size_t i = 0; i < vm; ++i)
	a_v[i] = std::make_pair(static_cast<int>(g.get_unsigned_long()), 0);
      vec_t::const_iterator b = a_v.begin();

      typedef find_test< vec_t::const_iterator> test_t;
      test_t tst(b, b, vn, vs, vm, vn, vs, vm);
      {
	typedef native_hash_map< int, char> native_t;
	tst(native_t());
      }

      {
	typedef native_map< int, char> native_t;
	tst(native_t());
      }

      {
	typedef hash_common_types<int, char>::performance_tl tl_t;
	tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef tree_common_types<int, char>::performance_tl tl_t;
	tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }
    }
  catch (...)
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
  cerr << "usage: hash_random_int_find_timing_test <vn> <vs> <vm>" <<
    endl << endl;

  cerr <<
    "This test checks the performance of various associative containers "
    "using their find method. " << endl;
  cerr << "Specifically, it does the following:"    << endl;
  cerr << "*  Creates a vector of random integers "    << endl;
  cerr << "*  Inserts the elements into the container"    << endl;
  cerr << "*  Performs a sequence of find operations. At each iteration, "
    "it finds, for each integer in the vector, its entry in the "
    "container, using the find method";
  cerr << "*  Repeats the above test a number of times" << endl;

  cerr << endl << endl;

  cerr << "vn = minimum size of the vector" << endl;
  cerr << "vs = step size of the vector" << endl;
  cerr << "vm = maximum size of the vector" << endl;
}
