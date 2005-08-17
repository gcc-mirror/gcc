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
 * @file ranged_hash_example.cpp
 * A basic example showing how to write a ranged-hash functor.
 */

// For cc_hash_assoc_cntnr.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For hash-related policies.
#include <ext/pb_assoc/hash_policy.hpp>
// For unary_function and binary_function.
#include <functional>
// For assert.
#include <cassert>
// For string.
#include <string>

/**
 * A (somewhat simplistic) ranged-hash function for strings.
 *	It uses the size of the container object to determine
 *	the hashing method. For smaller sizes it uses a simple hash function;
 *	for larger sizes it uses a more complicated hash function.
 */
class simple_string_ranged_hash_fn : public std::unary_function<
  std::string,
				     size_t>
{
public:
  typedef size_t size_type;

  // Default constructor.
  simple_string_ranged_hash_fn();

  // Called to notify that the size has changed.
  void
  notify_resized(size_t size);

  /*
   * Called for hashing a string into a size_t in a
   *	given range.
   */
  size_t
  operator()(const std::string& r_string);

  // Swaps content.
  void
  swap(simple_string_ranged_hash_fn& r_other);

private:
  // Records the size of the container object.
  size_t m_container_size;
};

simple_string_ranged_hash_fn::
simple_string_ranged_hash_fn() :
  m_container_size(0)
{

}

void
simple_string_ranged_hash_fn::
notify_resized(size_t size)
{
  m_container_size = size;
}

size_t
simple_string_ranged_hash_fn::
operator()(const std::string& r_string)
{
  /*
   * This (simplified) hash algorithm decides that if there are
   *	fewer than 100 strings in the container it will hash
   *	a string by summing its characters; otherwise, it will
   *	perform a more complicated operation in order to produce
   *	hash values with fewer collisions.
   */

  std::string::const_iterator it = r_string.begin();

  size_t hash = 0;

  if (m_container_size < 100)
    {
      // For this size, perform an std::accumulate type of operation.

      while (it != r_string.end())
	hash += static_cast<size_t>(*it++);
    }
  else
    {
      // For this size, perform a different operation.

      while (it != r_string.end())
	{
	  hash += static_cast<size_t>(*it++);

	  hash *= 5;
	}
    }

  /*
   * The function must, by whatever means, return
   *	a size in the range 0 to m_container_size.
   */
  return (hash % m_container_size);
}

void
simple_string_ranged_hash_fn::
swap(simple_string_ranged_hash_fn& r_other)
{
  std::swap(m_container_size, r_other.m_container_size);
}

int
main()
{
  /*
   * A collision-chaining hash table storing strings.
   */
  typedef
    pb_assoc::cc_hash_assoc_cntnr<
    std::string,
    pb_assoc::null_data_type,
    // Null hash function
    pb_assoc::null_hash_fn,
    // Equivalence function.
    std::equal_to<
    std::string>,
    // Range hashing function.
    simple_string_ranged_hash_fn >
    set_t;

  /*
   * Note that in the above, the library determines a resize policy
   *	appropriate for direct_mod_range_hashing.
   */

  set_t h;

  // Use the table normally.

  h.insert("Hello, ");
  h.insert("world");

  assert(h.size() == 2);

  assert(h.find("Hello, ") != h.end());
  assert(h.find("world") != h.end());

  assert(h.find("Goodbye, oh cruel world!") == h.end());
}

