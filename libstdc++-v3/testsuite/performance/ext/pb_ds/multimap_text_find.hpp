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
 * @file multimap_text_find_timing_test.cpp
 * Contains test for inserting text words.
 */

#include <ext/typelist.h>
#include <io/text_populate.hpp>
#include <performance/io/xml_formatter.hpp>
#include <native_type/native_hash_multimap.hpp>
#include <native_type/native_multimap.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <testsuite_rng.h>
#include <common_type/assoc/common_type.hpp>
#include <performance/assoc/timing/multimap_find_test.hpp>
#include <performance/assoc/multimap_common_type.hpp>
#include <hash_fn/string_hash_fn.hpp>
#include <iostream>
#include <vector>

void
usage();

void
set_test_parameters(size_t& n, size_t&s, size_t& m, size_t& prm);

int
main(int argc, char* a_p_argv[])
{
  using namespace __gnu_pbds::test;

  std::string f_name = "thirty_years_among_the_dead_preproc.txt";
  size_t prm;
  size_t ratio_n;
  size_t ratio_s;
  size_t ratio_m;


  set_test_parameters(prm, ratio_n, ratio_s, ratio_m);

  try
    {
      xml_test_performance_formatter fmt("Size", "Average time (sec.)");

      typedef std::vector<std::pair<std::string, int> > vec_t;
      vec_t a_v_init(prm);
      distinct_text_populate(f_name, a_v_init);

      vec_t a_v;
      twister_rand_gen g;
      for (size_t i = 0; i < ratio_m; ++i)
	for (size_t j = 0; j < a_v_init.size(); ++j)
	  a_v.push_back(std::make_pair(a_v_init[j].first,
				       static_cast<int>(g.get_unsigned_long())));

      vec_t::const_iterator b = a_v.begin();
      {
	typedef mmap_tl_t<std::string, int, std::allocator<char> >::type mmap_tl_tl;
	mmap_tl_tl tl;

	typedef multimap_find_test<vec_t::const_iterator, false> test_type;
	test_type tst(b, prm* ratio_n, prm* ratio_s, prm* ratio_m);
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef native_hash_multimap<std::string, int, 8, string_hash_fn> native_t;
	typedef multimap_find_test<vec_t::const_iterator, true> test_type;
	test_type tst(b, prm* ratio_n, prm* ratio_s, prm* ratio_m);
	tst(native_t());
      }

      {
	typedef native_multimap<std::string, int> native_t;
	typedef multimap_find_test<vec_t::const_iterator, true> test_type;
	test_type tst(b, prm* ratio_n, prm* ratio_s, prm* ratio_m);
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
  cerr << "usage: multimap_text_insert_test.out <prm> <ratio_n> <ratio_s> <ratio_m>" 
       << endl << endl;

  cerr << "This test checks the performance of various associative containers "
          "using their insert method. " << endl;
  cerr << "Specifically, it does the following:" << endl;
  cerr << "*  Creates a vector of pairs of text words" << endl;
  cerr << "*  Inserts the elements into the container" << endl;
  cerr << "*  Repeats the above test a number of times" << endl;
  cerr << endl << endl;

  cerr << "prm = maximum size of distinct pair-first entries" << endl;
  cerr << "ratio_n = minimum ratio of secondary keys to primary keys" << endl;
  cerr << "ratio_s = step ratio of secondary keys to primary keys" << endl;
  cerr << "ratio_m = maximum ratio of secondary keys to primary keys" << endl;
}
