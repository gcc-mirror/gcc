// __gnu_pbds::test::basic_type has ambiguous string conversions in C++17
// { dg-do run { target { ! c++17 } } }
// { dg-require-time "" }
// { dg-require-cstdint "" }
// This can take long on simulators, timing out the test.
// { dg-options "-DITERATIONS=5" { target simulator } }
// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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
 * @file priority_queue_rand_regression_test.cpp
 * Contains a random-operation test for priority queues.
 */

#define PB_DS_REGRESSION

#include <regression/rand/priority_queue/rand_regression_test.hpp>
#include <regression/common_type.hpp>

#ifndef ITERATIONS
#define ITERATIONS 5000
#endif
#ifndef KEYS
#define KEYS 10000
#endif
int
main(int argc, char* a_p_argv[])
{
  using namespace __gnu_pbds::test;
  return rand_regression_test(ITERATIONS, KEYS,
			      "pq_no_data_map_rand_regression_test", 
			      pq_tl_t());
}

