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
 * @file tree_text_insert_timing_test.cpp
 * Contains test for finding text.
 */

#include <ext/typelist.h>
#include <performance/io/xml_formatter.hpp>
#include <io/verified_cmd_line_input.hpp>
#include <common_type/assoc/common_type.hpp>
#include <performance/assoc/timing/insert_test.hpp>
#include <io/text_populate.hpp>
#include <hash_fn/string_hash_fn.hpp>
#include <native_type/assoc/native_hash_map.hpp>
#include <native_type/assoc/native_map.hpp>
#include <iostream>
#include <vector>

void
usage();

int
main(int argc, char* a_p_argv[])
{
  using namespace pb_ds::test;
  
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

      typedef insert_test< vec_t::const_iterator> test_t;
      vec_t::const_iterator b = a_v.begin();
      test_t tst(b, vn, vs, vm);
      {
	typedef trie_common_types<std::string, char>::performance_tl pat_trie_tl_t;
	typedef tree_common_types<std::string, char>::performance_tl tree_tl_t;
	typedef __gnu_cxx::typelist::append<pat_trie_tl_t, tree_tl_t>::type tl_t;
	tl_t tl;
	__gnu_cxx::typelist::apply(tst, tl);
      }

      {
	typedef native_map<std::string, char> native_map_t;
	tst(native_map_t());
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
  cerr << "usage: tree_text_insert_timing_test <f_name> <vn> <vs> <vm>" <<
    endl << endl;

  cerr <<
    "This test checks the performance of various associative containers "
    "using their insert method. " << endl;
  cerr << "Specifically, it does the following:" << endl;
  cerr << "*  Creates a vector of text words " << endl;
  cerr << "*  Inserts the elements into the container" << endl;
  cerr << "*  Repeats the above test a number of times) "
	    << endl;

  cerr << endl << endl;

  cerr << "f_name = file name containing the text words. "
    "Each line should contain one word." << endl;
  cerr << "vn = minimum size of the vector" << endl;
  cerr << "vs = step size of the vector" << endl;
  cerr << "vm = maximum size of the vector" << endl;
}
