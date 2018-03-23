// __gnu_pbds::test::basic_type has ambiguous string conversions in C++17
// { dg-do run { target { ! c++17 } } }
// { dg-require-time "" }
// This can take long on simulators, timing out the test.
// { dg-options "-DITERATIONS=5" { target simulator } }
// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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
 * @file tree_data_map_rand_regression_test.cpp
 * Contains a random-operation test for maps and sets.
 */

#define PB_DS_REGRESSION

#include <regression/rand/assoc/rand_regression_test.hpp>
#include <regression/common_type.hpp>

#ifndef ITERATIONS
# ifdef _GLIBCXX_DEBUG
#  define ITERATIONS 100
# else
#  define ITERATIONS 5000
#endif
#endif

#ifndef KEYS
# ifdef _GLIBCXX_DEBUG
#  define KEYS 200
# else
#  define KEYS 10000
# endif
#endif

int
main(int argc, char* a_p_argv[])
{
  using namespace __gnu_pbds::test;
  typedef tree_map_tl_t map_tl_t;

  return rand_regression_test(ITERATIONS, KEYS,
			      "tree_data_map_rand_regression_test",
			      map_tl_t());
}
