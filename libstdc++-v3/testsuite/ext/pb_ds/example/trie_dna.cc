// -*- C++ -*-

// Copyright (C) 2005-2013 Free Software Foundation, Inc.
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
 * @file trie_dna_example.cpp
 * An example showing how to use a trie for storing DNA strings.
 */

/**
 * This example shows how to use a PATRICIA trie for storing
 DNA strings. The main point is writing element-access traits
 for these strings.
*/

#include <cassert>
#include <iostream>
#include <cstdlib>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/trie_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// DNA is represented by a string.
typedef string dna_t;

// Following is an element access traits for a DNA string.
struct dna_string_access_traits
{
public:
  typedef size_t size_type;
  typedef dna_t key_type;
  typedef const key_type& key_const_reference;
  typedef char e_type;
  typedef string::const_iterator const_iterator;

  enum
    {
      // Number of distinct elements. This is 4 = |{'A', 'C', 'G', 'T'}|
      max_size = 4
    };

  // Returns a const_iterator to the firstelement of r_key.
  inline static const_iterator
  begin(key_const_reference r_key)
  { return r_key.begin(); }

  // Returns a const_iterator to the after-lastelement of r_key.
  inline static const_iterator
  end(key_const_reference r_key)
  { return r_key.end(); }

  // Maps an element to a position.
  inline static size_t
  e_pos(e_type e)
  {
    switch(e)
      {
      case 'A':
	return 0;
      case 'C':
	return 1;
      case 'G':
	return 2;
      case 'T':
	return 3;
      default:
	std::abort();
      };
  }
};

// A PATRICIA trie with DNA string element-access traits.
typedef dna_string_access_traits traits_type;
typedef trie<dna_t, string, traits_type> trie_type;

int main()
{
  trie_type t;

  // Now map some DNAs to diseases in namespace STD.
  t["ACCGGTTACTGGTA"] = "gonorrhea";
  t["CCGTTATCGGTA"] = "syphlis";

  // Check gonorrhea already contracted.
  assert(t.find("ACCGGTTACTGGTA") != t.end());

  return 0;
}

