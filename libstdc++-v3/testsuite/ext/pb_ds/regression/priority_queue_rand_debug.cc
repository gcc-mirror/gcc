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
 * @file priority_queue_rand_debug.cc
 * Contains a random-operation test for priority queues.
 */

#define PB_DS_REGRESSION
//#define PB_DS_REGRESSION_TRACE

#include <regression/rand/priority_queue/rand_regression_test.hpp>
#include <regression/common_type.hpp>
#include <ext/throw_allocator.h>
#include <ext/pb_ds/tag_and_trait.hpp>

#ifndef ITERATIONS
# define ITERATIONS 500
#endif

#ifndef KEYS
# define KEYS 1000
#endif

// Debug version of the rand regression tests, based on priority_queue_rand

// 1
// Simplify things by unrolling the typelist of the different
// container types into individual statements.
//
// Unroll the typelist represented by priority_queue_types, from
// regression/common_type.hpp. This is just a compile-time list of 5
// priority_queue types, with different policies for the type of priority_queue
// (pairing_heap_tag, binomial_heap_tag, rc_binomial_heap_tag,
// binary_heap_tag, thin_heap_tag).

using namespace __gnu_pbds::test::detail;
using namespace __gnu_pbds;
typedef __gnu_pbds::test::basic_type 			basic_type;
typedef __gnu_cxx::throw_allocator_random<basic_type>	allocator_type;

// pairing_heap_tag
typedef priority_queue<basic_type, std::less<basic_type>,
		       pairing_heap_tag, allocator_type>
							priority_queue_type1;

// binomial_heap_tag
typedef priority_queue<basic_type, std::less<basic_type>,
		       binomial_heap_tag, allocator_type>
							priority_queue_type2;

// rc_binomial_heap_tag
typedef priority_queue<basic_type, std::less<basic_type>,
		       rc_binomial_heap_tag, allocator_type>
							priority_queue_type3;

// binary_heap_tag
typedef priority_queue<basic_type, std::less<basic_type>,
		       binary_heap_tag, allocator_type>
							priority_queue_type4;

// thin_heap_tag
typedef priority_queue<basic_type, std::less<basic_type>,
		       thin_heap_tag, allocator_type>
							priority_queue_type5;


// 2
// Specialize container_rand_regression_test for specific container
// type and test function.

#ifdef SPECIALIZE
// For testing one specific container type.
typedef priority_queue_type4 			      	test_type;

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
  size_t sd = 1304368293;
  rand_reg_test test(sd, ITERATIONS, KEYS, 0.2, .6, .1, .2, .001, 1, true);

  // 1
  // Determine the problem container, function that fails.
  test(priority_queue_type1());
  test(priority_queue_type2());
  test(priority_queue_type3());
  test(priority_queue_type4());
  test(priority_queue_type5());

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
