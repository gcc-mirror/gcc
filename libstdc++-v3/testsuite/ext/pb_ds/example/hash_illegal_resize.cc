// { dg-timeout-factor 2.0 }

// -*- C++ -*-

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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
 * @file hash_illegal_resize_example.cpp
 * An example of illegally externally resizing a hash-based container object.
 */

/**
 * This example shows the case where a hash-based container object is
 * resized to a value which it cannot accommodate at runtime. Illegal
 * runtime resizes cause an exception.
 */

#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>
#include <ext/pb_ds/exception.hpp>
#include <cassert>

// size of test containers
#ifdef _GLIBCXX_DEBUG
# define SIZE 100
# define RESIZE 20
#else
# define SIZE 1000
# define RESIZE 200
#endif

using namespace std;
using namespace __gnu_pbds;

// A simple hash functor.
// hash could serve instead of this functor, but it is not yet
// standard everywhere.
struct int_hash : public unary_function<int, size_t>
{
  inline size_t
  operator()(const int& r_i) const
  { return r_i; }
};


int main()
{
  // A probing hash table mapping ints to chars.
  typedef
    gp_hash_table<
    int,
    int,
    int_hash,
    equal_to<int>,
    // Combining function.
    direct_mod_range_hashing<>,
    // Probe function.
    quadratic_probe_fn<>,
    // Resize policy.
    hash_standard_resize_policy<
    hash_prime_size_policy,
    hash_load_check_resize_trigger<>,
    /* Allow external access to size.
     *     Without setting this to true, external resizing
     *     is not possible.
     */
    true> >
    map_t;

  map_t g;

  // Insert some elements.
  int i;

  for (i = 0; i < SIZE; ++i)
    g[i] = 2*  i;

  // Check all ok.
  assert(g.size() == SIZE);
  for (i = 0; i < SIZE; ++i)
    assert(g.find(i) != g.end() && g.find(i)->second == 2 * i);

  // Now attempt to resize the table to 200 (impossible).
  bool ex_thrown = false;

  try
    {
      g.resize(RESIZE);
    }
  catch(__gnu_pbds::resize_error& )
    {
      ex_thrown = true;
    }

  // Assert an exception was thrown. A probing table cannot contain
  // 1000 entries in less than 1000 places.
  assert(ex_thrown);

  // Irrespective of the fact that the resize was not successful, the
  // container object should still be in a valid state; the following
  // checks this.
  // Check all ok.
  assert(g.size() == SIZE);
  for (i = 0; i < SIZE; ++i)
    assert(g.find(i) != g.end() && g.find(i)->second == 2 * i);

  return 0;
}
