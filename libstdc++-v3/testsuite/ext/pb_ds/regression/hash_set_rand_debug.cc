// { dg-require-debug-mode "" }
// { dg-require-time "" }
// This can take long on simulators, timing out the test.
// { dg-options "-DITERATIONS=5" { target simulator } }
// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
 * @file hash_data_map_rand.cc
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

// Debug version of the rand regression tests, based on hash_data_map_rand

// 1
// Simplify things by unrolling the typelist of the different
// container types into individual statements.
//
// Unroll the typelist represented by priority_queue_types, from
// regression/common_type.hpp. This is just a compile-time list of 5
// hash tables, with different policies for the type of table
// (gp_hash, cc_hash), for the resize policies and probe functions.
using namespace __gnu_pbds::test::detail;
using namespace __gnu_pbds;
typedef __gnu_pbds::test::basic_type 			basic_type;
typedef __gnu_cxx::throw_allocator_random<basic_type>	allocator_type;
typedef std::equal_to<basic_type>			equal_type;


typedef __gnu_pbds::test::direct_mod_range_hashing_t_<allocator_type>
			       				cprobe_type1;

typedef __gnu_pbds::test::direct_mask_range_hashing_t_<allocator_type>
			       				cprobe_type2;


typedef __gnu_pbds::test::quadratic_probe_fn_t_<basic_type, allocator_type>
							probe_type1;

typedef __gnu_pbds::test::linear_probe_fn_t_<basic_type, allocator_type>
							probe_type2;


typedef __gnu_pbds::test::hash_load_check_resize_trigger_t_<allocator_type,
							    1, 8, 1, 2, false>
							trigger_type1;

typedef __gnu_pbds::test::hash_load_check_resize_trigger_t_<allocator_type,
							    1, 8, 1, 2, true>
							trigger_type2;

typedef __gnu_pbds::test::hash_load_check_resize_trigger_t_<allocator_type,
							    1, 8, 1, 1, false>
							trigger_type3;

typedef __gnu_pbds::test::cc_hash_max_collision_check_resize_trigger_t_<allocator_type, 1, 2, false>
							trigger_type4;

typedef __gnu_pbds::test::cc_hash_max_collision_check_resize_trigger_t_<allocator_type, 1, 2, true>
							trigger_type5;


typedef hash_standard_resize_policy<__gnu_pbds::test::hash_prime_size_policy_t_, trigger_type1, false, unsigned long>		      	resize_type1;

typedef hash_standard_resize_policy<__gnu_pbds::test::hash_exponential_size_policy_t_<allocator_type>, trigger_type2, true, unsigned long>
							resize_type2;

typedef hash_standard_resize_policy<__gnu_pbds::test::hash_exponential_size_policy_t_<allocator_type>, trigger_type5, false, unsigned long>
							resize_type3;



// gp_hash 1
typedef gp_hash_table<basic_type, null_type, __gnu_pbds::test::hash,
		      equal_type, cprobe_type1, probe_type1, resize_type1,
		      false, allocator_type>
							gp_hash_type1;

// gp_hash 2
typedef gp_hash_table<basic_type, null_type, __gnu_pbds::test::hash,
		      equal_type, cprobe_type2, probe_type2, resize_type2,
		      true, allocator_type>
							gp_hash_type2;


// cc_hash 1
typedef cc_hash_table<basic_type, null_type, __gnu_pbds::test::hash,
		      equal_type, cprobe_type2, resize_type3,
		      false, allocator_type>
							cc_hash_type1;

// cc_hash 2
typedef cc_hash_table<basic_type, null_type, __gnu_pbds::test::hash,
		      equal_type, cprobe_type2, resize_type2,
		      false, allocator_type>
							cc_hash_type2;

// cc_hash 3
typedef cc_hash_table<basic_type, null_type, __gnu_pbds::test::hash,
		      equal_type, cprobe_type1, resize_type1,
		      true, allocator_type>
							cc_hash_type3;

// 2
// Specialize container_rand_regression_test for specific container
// type and test function.

#ifdef SPECIALIZE
// For testing one specific container type.
typedef cc_hash_type1 					test_type;

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
  test(gp_hash_type1());
  test(gp_hash_type2());

  test(cc_hash_type1());
  test(cc_hash_type2());
  test(cc_hash_type3());

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
