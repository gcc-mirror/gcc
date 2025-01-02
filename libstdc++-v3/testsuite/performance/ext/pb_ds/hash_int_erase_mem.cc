// -*- C++ -*-

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
 * @file hash_random_int_erase_mem_usage_test.cpp
 * Contains test for erasing random integers.
 */

#include <ext/typelist.h>
#include <testsuite_allocator.h>
#include <performance/io/xml_formatter.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <testsuite_rng.h>
#include <common_type/assoc/common_type.hpp>
#include <performance/assoc/mem_usage/erase_test.hpp>
#include <iostream>
#include <vector>
#include <functional>

struct int_hash
{
  typedef int result_type;
  typedef int argument_type;

  inline int
  operator()(int i) const
  { return i; }
};

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
      xml_test_performance_formatter fmt("Size", "Memory (bytes)");

      typedef std::vector<int> vec_t;
      vec_t a_v(vm);
      twister_rand_gen g;
      for (size_t i = 0; i < vm; ++i)
	a_v[i] = static_cast<int>(g.get_unsigned_long());

      vec_t::const_iterator b = a_v.begin();
      erase_test<vec_t::const_iterator> tst(b,  vn, vs, vm);
      typedef __gnu_test::tracker_allocator<char> alloc_t;
      {
	typedef hash_common_types<int, __gnu_pbds::null_type, int_hash, std::equal_to<int>, alloc_t>::performance_tl tl_t;

	tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef native_hash_set<int, 8, int_hash, std::equal_to<int>, std::less<int>, alloc_t> native_t;

	tst(native_t());
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
  cerr << "usage: hash_random_int_erase_if_test <vn> <vs> <vm>" 
       << endl << endl;

  cerr << "This test checks the performance of various associative containers "
          "using their erase method. " << endl;
  cerr << "Specifically, it does the following:" << endl;
  cerr << "*  Creates a vector of random integers " << endl;
  cerr << "*  Inserts the elements into the container" << endl;
  cerr << "*  Erases all the elements, except one, from the constainer" 
       << endl << endl;

  cerr << "vn = minimum size of the vector" << endl;
  cerr << "vs = step size of the vector" << endl;
  cerr << "vm = maximum size of the vector" << endl;
}
