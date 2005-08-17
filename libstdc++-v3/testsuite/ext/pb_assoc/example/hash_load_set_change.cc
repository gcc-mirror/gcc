// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file hash_load_set_change_example.cpp
 * An example of setting and changing the load factor of a hash-based
 *	container object.
 */

// For cc_hash_assoc_cntnr.
#include <ext/pb_assoc/assoc_cntnr.hpp>
/*
 * For hash_standard_resize_policy, hash_exponential_size_policy,
 *	and direct_mask_range_hashing.
 */
#include <ext/pb_assoc/hash_policy.hpp>
// For unary_function.
#include <functional>
// For assert
#include <cassert>

/*
 * A simple hash functor.
 *	std::hash could serve instead of this functor,
 *	but it is not yet standard everywhere.
 */
struct int_hash : public std::unary_function<
  int,
	     size_t>
{
  inline size_t
  operator()(int i) const
  {
    return (i);
  }
};

int
main()
{
  // A trigger policy type.
  typedef pb_assoc::hash_load_check_resize_trigger< true> trigger_t;

  // A resize policy type.
  typedef
    pb_assoc::hash_standard_resize_policy<
    pb_assoc::hash_exponential_size_policy<>,
    // Trigger type.
    trigger_t,
    /* Allow external access to size.
     *	This is not necessary for setting the load factor,
     *	but it allows to call get_actual_size.
     */
    true>
    resize_t;

  /*
   * A collision-chaining hash table mapping ints to chars.
   */
  typedef
    pb_assoc::cc_hash_assoc_cntnr<
    int,
    char,
    int_hash,
    std::equal_to<int>,
    // Combining function.
    pb_assoc::direct_mask_range_hashing<>,
    // Resize policy.
    resize_t>
    map_t;

  // A trigger policy object with load between 0.3 and 0.8.
  trigger_t trigger(static_cast<float>(0.3), static_cast<float>(0.8));

  // A resize policy object with the above trigger.
  resize_t resize(pb_assoc::hash_exponential_size_policy<>(),
		  trigger);

  map_t c(int_hash(),
	  std::equal_to<int>(),
	  pb_assoc::direct_mask_range_hashing<>(),
	  resize);

  c[1] = 'a';

  // Check the loads and sizes.

  assert(c.get_loads().first == static_cast<float>(0.3));
  assert(c.get_loads().second == static_cast<float>(0.8));

  assert(c.get_actual_size() == 8);

  assert(c.size() == 1);

  /*
   * Note that there is a discrepancy between the loads of the policy
   *	object and the actual size of the container object. This is
   *	because the container's construction performs an implicit external
   *	resize.
   */

  c[2] = 'b';
  c[3] = 'c';
  c[4] = 'd';

  assert(c.get_actual_size() == 8);

  // Change the loads. This causes (potentially) a resize.
  c.set_loads(std::make_pair(
			     static_cast<float>(0.01),
			     static_cast<float>(0.05)));

  // The loads actually changed.
  assert(c.get_actual_size() > 8);
}

