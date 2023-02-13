// { dg-require-debug-mode "" }
// { dg-require-time "" }
// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
 * @file list_update_data_map_rand_debug.cc
 * Contains a random-operation test for maps and sets, separated out.
 */

#define PB_DS_REGRESSION
//#define PB_DS_REGRESSION_TRACE

#include <regression/rand/assoc/rand_regression_test.hpp>
#include <regression/common_type.hpp>
#include <ext/throw_allocator.h>
#include <ext/pb_ds/tag_and_trait.hpp>

// Debug version of the rand regression tests, based on list_update_data_map.

// 1
// Simplify things by unrolling the typelist of the different
// container types into individual statements.
//
// Unroll the typelist represented by list_update_types, from
// regression/common_type.hpp. This is just a compile-time list of
// list_update types, with different policies for the type of
// update (lu_move_to_front_policy, lu_counter_policy).

using namespace __gnu_pbds::test::detail;
using namespace __gnu_pbds;
typedef __gnu_pbds::test::basic_type 			basic_type;
typedef __gnu_cxx::throw_allocator_random<basic_type>	allocator_type;
typedef std::equal_to<basic_type>			equal_type;

typedef __gnu_pbds::test::lu_move_to_front_policy_t_	policy_type1;

typedef __gnu_pbds::test::lu_counter_policy_t_<allocator_type, 5u>
							policy_type2;


typedef list_update<basic_type, null_type, equal_type, policy_type1,
		    allocator_type>
							list_type1;

typedef list_update<basic_type, null_type, equal_type, policy_type2,
		    allocator_type>
							list_type2;

// 2
// Specialize container_rand_regression_test for specific container
// type and test function.

#ifdef SPECIALIZE
// For testing one specific container type.
typedef list_type1 					test_type;

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
  rand_reg_test test(sd, 50, 10, .2, .6, .2, .001, .25, true);

  // 1
  // Determine the problem container, function that fails.
  test(list_type1());
  test(list_type2());

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
