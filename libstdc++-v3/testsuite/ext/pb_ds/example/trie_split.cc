// -*- C++ -*-

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
 * @file trie_split_example.cpp
 * A basic example showing how to split trie-based container objects.
 */

/**
 * This example shows how to split trie based containers, i.e., the opposite
 * of a join operation.
 */

#include <string>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace __gnu_pbds;

int main()
{
  // A PATRICIA trie table mapping strings to chars.
  typedef trie<string, char> map_type;

  // A map_type object.
  map_type r;

  // Inserts some entries into r.
  for (int i = 0; i < 100; ++ i)
    r.insert(make_pair(string(i, 'a'), 'b'));

  // Now split r into a different map_type object.

  // larger_r will hold the larger values following the split.
  map_type larger_r;

  // Split all elements with key larger than 'a'^1000 into larger_r.
  // This is exception free.
  r.split(string(1000, 'a'), larger_r);

  // Since there were no elements with key larger than 'a'^1000, r
  // should be unchanged.
  assert(r.size() == 100);
  assert(r.begin()->first == string(""));

  // Now perform a split which actually changes the content of r.

  // Split all elements with key larger than "aaa" into larger_r.
  r.split(string("aaa"), larger_r);

  assert(r.size() == 4);
  assert(larger_r.begin()->first == string("aaaa"));

  return 0;
}

