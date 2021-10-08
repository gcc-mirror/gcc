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
 * @file store_hash_example.cpp
 * An example showing how to store hash-values with
 *     each entry in a hash-based container.
 */

/**
 * This example shows how to configure a hash-based container to store
 * the hash value of each key along with each entry. This technique
 * is useful for complex keys (e.g., strings in this example), since
 * it lowers the cost of collisions. For simpler types (e.g., integers),
 * where the cost of comparing keys is of the same order as the cost
 * of comparing hash values, this technique adds unnecessary overhead.
 */

#include <functional>
#include <string>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// A string hash functor.
struct string_hash
{
  size_t
  operator()(string str) const
  {
    string::const_iterator b = str.begin();
    string::const_iterator e = str.end();

    size_t hash = 0;
    while (b != e)
      {
	hash *= 5;
	hash += static_cast<size_t>(*b);
	++b;
      }
    return hash;
  }
};

int main()
{
  // A collision-chaining hash table mapping strings to ints.
  typedef
    cc_hash_table<
    string,
    int,
    string_hash,
    equal_to<string>,
    direct_mask_range_hashing<>,
    hash_standard_resize_policy<>,
    true>
    map_t;

  map_t h;

  // Use regularly.
  h["Hello, "] = 0;
  h["world"] = 1;

  return 0;
}

