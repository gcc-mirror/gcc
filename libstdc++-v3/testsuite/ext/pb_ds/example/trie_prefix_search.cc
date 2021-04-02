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
 * @file trie_prefix_search_example.cpp
 * An example showing how to use a trie for searching
 *    for words with a given prefix.
 */

/**
 * This example shows how to use a PATRICIA trie for searching
 * for words with a given prefix.
 */

#include <cassert>
#include <iostream>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/trie_policy.hpp>
#include <ext/pb_ds/tag_and_trait.hpp>

using namespace std;
using namespace __gnu_pbds;

// A PATRICIA trie with a prefix-search node-updator type. Note that
// since the node updator is trie_prefix_search_node_update, then the
// container includes its method prefix_range.
typedef null_type		mapped_type;
typedef trie_string_access_traits<> 	cmp_fn;
typedef pat_trie_tag 			tag_type;

typedef trie<string, mapped_type, cmp_fn, tag_type, 
	     trie_prefix_search_node_update> trie_type;

// The following helper function takes a trie object and r_key, a
// const reference to a string, and prints all entries whose key
// includes r_key as a prefix.
void
print_prefix_match(const trie_type& t, const string& key)
{
  typedef trie_type::const_iterator 		const_iterator;
  typedef pair<const_iterator, const_iterator> 	pair_type;

  cout << "All keys whose prefix matches \"" << key << "\":" << endl;

  const pair_type match_range = t.prefix_range(key);
  for (const_iterator it = match_range.first; it != match_range.second; ++it)
    cout << *it << ' ';
  cout << endl;
}

int main()
{
  trie_type t;

  // Insert some entries.
  assert(t.insert("I").second == true);
  assert(t.insert("wish").second == true);
  assert(t.insert("that").second == true);
  assert(t.insert("I").second == false);
  assert(t.insert("could").second == true);
  assert(t.insert("ever").second == true);
  assert(t.insert("see").second == true);
  assert(t.insert("a").second == true);
  assert(t.insert("poem").second == true);
  assert(t.insert("lovely").second == true);
  assert(t.insert("as").second == true);
  assert(t.insert("a").second == false);
  assert(t.insert("trie").second == true);

  // Now search for prefixes.
  print_prefix_match(t, "");
  print_prefix_match(t, "a");
  print_prefix_match(t, "as");
  print_prefix_match(t, "ad");
  print_prefix_match(t, "t");
  print_prefix_match(t, "tr");
  print_prefix_match(t, "trie");
  print_prefix_match(t, "zzz");

  return 0;
}

