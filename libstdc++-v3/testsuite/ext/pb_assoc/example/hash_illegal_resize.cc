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
 * @file hash_illegal_resize_example.cpp
 * An example of externally resizing a hash-based container object.
 */

// For gp_hash_assoc_cntnr
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For hash-related policies.
#include <ext/pb_assoc/hash_policy.hpp>
// For cannot_resize
#include <ext/pb_assoc/exception.hpp>
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
  operator()(const int& r_i) const
  {
    return (r_i);
  }
};

int
main()
{
  /*
   * A probing hash table mapping ints to chars.
   */
  typedef
    pb_assoc::gp_hash_assoc_cntnr<
    int,
    int,
    int_hash,
    std::equal_to<
    int>,
    // Combining function.
    pb_assoc::direct_mod_range_hashing<>,
    // Probe function.
    pb_assoc::quadratic_probe_fn<
    const int& >,
    // Resize policy.
    pb_assoc::hash_standard_resize_policy<
    pb_assoc::hash_prime_size_policy,
    pb_assoc::hash_load_check_resize_trigger<>,
    /* Allow external access to size.
     *	Without setting this to true, external resizing
     *	is not possible.
     */
    true> >
    map_t;

  map_t g;

  /*
   * Check the actual size of the container object.
   *	This should be at least
   *	the initial size given by the size policy object.
   **/
  assert(g.get_actual_size() > 8);

  // Insert some elements.

  int i;

  for (i = 0; i < 1000; ++i)
    g[i] = 2*  i;

  // Check all ok.

  assert(g.size() == 1000);

  for (i = 0; i < 1000; ++i)
    assert(g.find(i) != g.end()&&  g.find(i)->second == 2*  i);

  // Now attempt to resize the table to 200 (impossible).

  bool ex_thrown = false;

  try
    {
      g.resize(200);
    }
  catch(pb_assoc::cannot_resize& )
    {
      ex_thrown = true;
    }

  /*
   * Assert an exception was thrown. A probing table cannot contain
   *	1000 entries in less than 1000 places.
   */
  assert(ex_thrown);

  // Check all ok.

  assert(g.size() == 1000);

  for (i = 0; i < 1000; ++i)
    assert(g.find(i) != g.end()&&  g.find(i)->second == 2*  i);
}

