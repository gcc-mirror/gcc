// -*- C++ -*-

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
 * @file ranged_hash_example.cpp
 * A basic example showing how to write a ranged-hash functor.
 */

/**
 * In some cases it is beneficial to write a hash function which determines
 * hash values based on the size of the container object.
 * The example shows an example of a string-hashing function which
 * uses a fast method for hashing strings when the number of strings
 * in the container object is small, and a slower but more careful method
 * for hashing strings when the number of strings in the container object
 * is large.
 */

#include <functional>
#include <cassert>
#include <string>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

/**
 * A (somewhat simplistic) ranged-hash function for strings.
 * It uses the size of the container object to determine
 * the hashing method. For smaller sizes it uses a simple hash function;
 * for larger sizes it uses a more complicated hash function.
 */
class simple_string_ranged_hash_fn 
{
public:
  typedef size_t size_type;

  simple_string_ranged_hash_fn() : m_container_size(0) { }

  // Called to notify that the size has changed.
  void
  notify_resized(size_t size)
  { m_container_size = size; }

  // Called for hashing a string into a size_t in a given range.
  size_t
  operator()(const string& r_string)
  {
    /*
     *  This (simplified) hash algorithm decides that if there are
     *  fewer than 100 strings in the container it will hash
     *  a string by summing its characters; otherwise, it will
     *  perform a more complicated operation in order to produce
     *  hash values with fewer collisions.
     */
    string::const_iterator it = r_string.begin();
    size_t hash = 0;
    if (m_container_size < 100)
      {
	// For this size, perform an accumulate type of operation.
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

    // The function must, by whatever means, return a size in the
    // range 0 to m_container_size.
    return hash % m_container_size;
  }

  // Swaps content.
  void
  swap(simple_string_ranged_hash_fn& other)
  {
    std::swap(m_container_size, other.m_container_size);
  }

private:
  // Records the size of the container object.
  size_t m_container_size;
};

int
main()
{
  // A collision-chaining hash table storing strings.
  typedef
    cc_hash_table<
    string,
    null_type,
    null_type,
    equal_to<string>,
    simple_string_ranged_hash_fn>
    set_t;

  // Note that in the above, the library determines a resize policy
  // appropriate for modulo-based range hashing.
  set_t h;

  // Use the table normally.
  h.insert("Hello, ");
  h.insert("world");

  assert(h.size() == 2);

  assert(h.find("Hello, ") != h.end());
  assert(h.find("world") != h.end());

  assert(h.find("Goodbye, oh cruel world!") == h.end());

  return 0;
}

