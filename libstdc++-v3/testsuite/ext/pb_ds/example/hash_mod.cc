// -*- C++ -*-

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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
 * @file hash_mod_example.cpp
 * An example showing how to use a mod range-hasing function
 */

/**
 * This example shows how to use a hash-based container employing
 * a modulo-based range-hashing function.
 */

#include <functional>
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
  // In this case, we are worried that the key distribution will be
  // skewed. We wish to use a more robust combining function.

  // A collision-chaining hash table mapping ints to chars.
  typedef
    cc_hash_table<
    int,
    char,
    int_hash,
    equal_to<int>,
    // Combining function.
    direct_mod_range_hashing<> >
    map_t;

  map_t r_c;

  // Use regularly.
  r_c[32] = 'b';
  r_c[1024] = 'c';
  r_c[4096] = 'd';

  // The above keys are all powers of 2. A mask combining function
  // would hamper performance in such a case.
  return 0;
}

