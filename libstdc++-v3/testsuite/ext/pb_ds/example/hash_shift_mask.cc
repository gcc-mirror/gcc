// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

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
 * @file hash_shift_mask_example.cpp
 * An example showing how to write a range-hasing functor.
 */

/**
 * In some rare cases, advance knowledge of the distribution of keys allows
 * writing more efficient hash-related policies.
 * In the rather simplistic case of the example, it is known in advance that
 * all keys have 0 two lowest bits. The example shows how to write
 * a range-hashing function disregarding the two lowest bits of the hash value.
 */

#include <functional>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/hash_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// A simple hash functor. hash could serve instead of this functor,
// but it is not yet standard everywhere.
struct simple_int_hash : public unary_function<int, size_t>
{
  inline size_t
  operator()(int i) const
  { return i; }
};

// A range-hashing function which shifts 2 bits right and then masks.
class shift_two_mask_range_hashing : private direct_mask_range_hashing<>
{
public:
  typedef size_t size_type;

  // Swaps with a different instant.
  void
  swap(shift_two_mask_range_hashing& other)
  { direct_mask_range_hashing<>::swap(other); }

  // Called by the container when internally resized.
  void
  notify_resized(size_type size)
  { direct_mask_range_hashing<>::notify_resized(size); }

  // Given a hash value, returns a number in the range of the internal
  // size of the container.
  inline size_type
  operator()(size_type hash) const
  { return direct_mask_range_hashing<>::operator()(hash >> 2); }
};

int
main()
{
  // A collision-chaining hash table mapping ints to chars.
  typedef
    cc_hash_table<
    int,
    char,
    // Hash function.
    simple_int_hash,
    equal_to<int>,
    // Range hashing function.
    shift_two_mask_range_hashing>
    map_t;

  map_t h;

  // Use normally.
  h[16] = 'a';
  h[256] = 'e';
  h[4] = 'z';

  return 0;
}

