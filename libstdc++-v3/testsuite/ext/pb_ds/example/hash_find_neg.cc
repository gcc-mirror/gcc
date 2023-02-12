// { dg-do compile }
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
 * @file hash_bad_find_example.cpp
 * An example showing how *not* to use unordered containers.
 */

/**
 * This non-compiling example shows wrong use of unordered
 * associative-containers. These types of containers have distinct
 * point-type and range-type iterator types.
 **/

#include <utility>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace __gnu_pbds;

int main()
{
  // A collision-chaining hash table mapping ints to chars.
  typedef cc_hash_table<int, char> map_t;

  // A map_t object.
  map_t h;

  // Insert a value mapping the int 1 to the char 'a'.
  h.insert(make_pair(1, 'a'));

  // Find the entry of the key '1' the* wrong* way.
  // The following line will not compile, since map_t::find returns a
  // point-iterator, which, by design, is not convertible to a
  // range-iterator.
  map_t::iterator it = h.find(1); // { dg-error "conversion from" }

  return 0;
}

