// -*- C++ -*-

// Copyright (C) 2005-2023 Free Software Foundation, Inc.
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
 * @file hash_initial_size_example.cpp
 * An example of setting an initial size for a container object.
 */

/**
 * This example shows how to set the initial size of a hash-based
 * container object through its resize-policy object.
 */

#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// A simple hash functor.
// hash could serve instead of this functor, but it is not yet
// standard everywhere.
struct int_hash
{
  size_t
  operator()(const int& r_i) const
  { return r_i; }
};

int main()
{
  // Resize policy type.
  typedef
    hash_standard_resize_policy<
    // Size-policy type.
    hash_exponential_size_policy<>,
    // Trigger-policy type.
    hash_load_check_resize_trigger<>,
    /* Allow external access to size.
     *     This is just used in this example for using the
     *     get_actual_size method (which won't be accessible without
     *     this flag.
     */
    true>
    resize_policy_t;

  // A collision-probing hash table mapping ints to chars.
  typedef
    gp_hash_table<
    int,
    char,
    int_hash,
    equal_to<
    int>,
    // Combining function.
    direct_mask_range_hashing<>,
    // Probe function.
    linear_probe_fn<>,
    // Resize policy.
    resize_policy_t>
    map_t;

  // A resize-policy object with suggested initial size 256.
  resize_policy_t res(hash_exponential_size_policy<>(256));

  map_t g(int_hash(),
	  equal_to<int>(),
	  direct_mask_range_hashing<>(),
	  linear_probe_fn<>(),
	  res);

  // Check the actual size of the container object. In this case, this
  // should be the initial size given by the size policy object.
  assert(g.get_actual_size() == 256);

  // The logical size of g, though is 0 (it does not contain any elements).
  assert(g.size() == 0);

  return 0;
}

