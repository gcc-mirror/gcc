// { dg-require-debug-mode "" }
// { dg-require-time "" }
// This can take long on simulators, timing out the test.
// { dg-options "-DITERATIONS=5" { target simulator } }
// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

/**
 * @file tree_data_map_rand_debug.cc
 * Contains a random-operation test for maps and sets, separated out.
 */

#define PB_DS_REGRESSION
//#define PB_DS_REGRESSION_TRACE

#include <regression/rand/assoc/rand_regression_test.hpp>
#include <regression/common_type.hpp>
#include <ext/throw_allocator.h>
#include <ext/pb_ds/tag_and_trait.hpp>

#ifndef ITERATIONS
# define ITERATIONS 100
#endif

#ifndef KEYS
# define KEYS 200
#endif

// Debug version of the rand regression tests, based on tree_data_map.

// 1
// Simplify things by unrolling the typelist of the different
// container types into individual statements.
//
// Unroll the typelist represented by tree_types, from
// regression/common_type.hpp. This is just a compile-time list of 6
// tree types, with different policies for the type of tree
// (ov_tree_tag, rb_tree_tag, splay_tree_tag) and for the node
// update (null_node_update, tree_order_statistics_node_update)

using namespace __gnu_pbds::test::detail;
using namespace __gnu_pbds;
typedef __gnu_pbds::test::basic_type 			basic_type;
typedef __gnu_cxx::throw_allocator_random<basic_type>	allocator_type;

// ov_tree_tag
typedef tree<basic_type, basic_type, std::less<basic_type>,
	     ov_tree_tag, null_node_update,
	     allocator_type>				ov_tree_type1;

typedef tree<basic_type, basic_type, std::less<basic_type>,
	     ov_tree_tag, tree_order_statistics_node_update,
	     allocator_type> 				ov_tree_type2;

// rb_tree_tag
typedef tree<basic_type, basic_type, std::less<basic_type>,
	     rb_tree_tag, null_node_update,
	     allocator_type>				rb_tree_type1;

typedef tree<basic_type, basic_type, std::less<basic_type>,
	     rb_tree_tag, tree_order_statistics_node_update,
	     allocator_type> 				rb_tree_type2;

// splay_tree_tag
typedef tree<basic_type, basic_type, std::less<basic_type>,
	     splay_tree_tag, null_node_update,
	     allocator_type>				splay_tree_type1;

typedef tree<basic_type, basic_type, std::less<basic_type>,
	     splay_tree_tag, tree_order_statistics_node_update,
	     allocator_type> 				splay_tree_type2;


// 2
// Specialize container_rand_regression_test for specific container
// type and test function.

#ifdef SPECIALIZE
// For testing one specific container type.
typedef ov_tree_type2 					test_type;

void debug_break_here() { }

namespace __gnu_pbds {
  namespace test {
    namespace detail {

      template<>
      void
      container_rand_regression_test<test_type>::operator()()
      {
      }

    }
  }
}
#endif

int
main()
{
  // Set up the test object.
  size_t sd = 1303948889;
  rand_reg_test test(sd, ITERATIONS, KEYS, 0.2, .6, .2, .001, .25, true);

  // 1
  // Determine the problem container, function that fails.
  test(ov_tree_type1());
  test(ov_tree_type2());
  test(rb_tree_type1());
  test(rb_tree_type2());
  test(splay_tree_type1());
  test(splay_tree_type2());

#ifdef SPECIALIZE
  // 2
  // With specified problem container set test_type typedef
  // appropriately above. Then, specialize operator()(), also
  // above. Finally, run this below.
  using namespace std;
  test_type obj;
  test(obj);
#endif

  return 0;
}
