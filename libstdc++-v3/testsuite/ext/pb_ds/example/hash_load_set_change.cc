// -*- C++ -*-

// Copyright (C) 2005, 2006, 2009 Free Software Foundation, Inc.
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
 * @file hash_load_set_change_example.cpp
 * An example of setting and changing the load factor of a hash-based
 *    container object.
 */

/**
 * This example shows how to set and change the load-factor of
 * a hash-based container object through its resize-policy object.
 */

#include <functional>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// A simple hash functor.
// hash could serve instead of this functor, but it is not yet
// standard everywhere.
struct int_hash : public unary_function<int, size_t>
{
  inline size_t
  operator()(int i) const
  { return i; }
};

int main()
{
  // A trigger policy type.
  typedef hash_load_check_resize_trigger< true> trigger_t;

  // A resize policy type.
  typedef
    hash_standard_resize_policy<
    hash_exponential_size_policy<>,
    // Trigger type.
    trigger_t,
    /* Allow external access to size.
     * This is not necessary for setting the load factor,
     * but it allows to call get_actual_size.
     */
    true>
    resize_t;

  // A collision-chaining hash table mapping ints to chars.
  typedef
    cc_hash_table<
    int,
    char,
    int_hash,
    equal_to<int>,
    // Combining function.
    direct_mask_range_hashing<>,
    // Resize policy.
    resize_t>
    map_t;

  // A trigger policy object with load between 0.3 and 0.8.
  trigger_t trigger(static_cast<float>(0.3), static_cast<float>(0.8));

  // A resize policy object with the above trigger.
  resize_t resize(hash_exponential_size_policy<>(),
		  trigger);

  map_t r_c(int_hash(),
	    equal_to<int>(),
	    direct_mask_range_hashing<>(),
	    resize);

  r_c[1] = 'a';

  // Check the loads and sizes.
  assert(r_c.get_loads().first == static_cast<float>(0.3));
  assert(r_c.get_loads().second == static_cast<float>(0.8));
  assert(r_c.get_actual_size() == 8);
  assert(r_c.size() == 1);

  // Note that there is a discrepancy between the loads of the policy
  // object and the actual size of the container object. This is
  // because the container's construction performs an implicit
  // external resize.
  r_c[2] = 'b';
  r_c[3] = 'c';
  r_c[4] = 'd';

  assert(r_c.get_actual_size() == 8);

  // Change the loads. This causes (potentially) a resize.
  r_c.set_loads(make_pair(static_cast<float>(0.01),
			  static_cast<float>(0.05)));

  // The actual size should really change in this case.
  assert(r_c.get_actual_size() > 8);

  return 0;
}

