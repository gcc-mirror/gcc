// -*- C++ -*-

// Copyright (C) 2005, 2006, 2009 Free Software Foundation, Inc.
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
 * @file erase_if_example.cpp
 * A basic example showing how to use erase_if.
 */

/**
 * The following example shows how to use a conditional-erase
 * method of associative containers to erase some of their entries.
 */

#include <iostream>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace __gnu_pbds;

// The following functor takes a map's value-type object and returns
// whether its key is between two numbers.
struct between : public unary_function<pair<const int, char>, bool>
{
  // Constructor taking two numbers determining a range.
  between(int b, int e) : m_b(b), m_e(e)
  { assert(m_b < m_e); }

  // Operator determining whether a value-type object's key is within
  // the range.
  inline bool
  operator()(const pair<const int, char>& r_val)
  { return r_val.first >= m_b&&  r_val.first < m_e; }

private:
  const int m_b;
  const int m_e;
};

/**
 * The following function performs a sequence of operations on an
 * associative container object mapping integers to characters. Specifically
 * it inserts 100 values and then uses a conditional-erase method to erase
 * the values whose key is between 10 and 90.
 */
template<class Cntnr>
void
some_op_sequence(Cntnr r_c)
{
  assert(r_c.empty());
  assert(r_c.size() == 0);

  for (int i = 0; i < 100; ++i)
    r_c.insert(make_pair(i, static_cast<char>(i)));
  assert(r_c.size() == 100);

  // Erase all values whose key is between 10 (inclusive) and 90
  // (non-inclusive).
  r_c.erase_if(between(10 , 90));

  assert(!r_c.empty());
  assert(r_c.size() == 20);
}

int main()
{
  // Perform operations on a list-update set.
  some_op_sequence(list_update<int, char>());

  // Perform operations on a collision-chaining hash set.
  some_op_sequence(cc_hash_table<int, char>());

  // Perform operations on a general-probing hash set.
  some_op_sequence(gp_hash_table<int, char>());

  // Perform operations on a red-black tree set.
  some_op_sequence(tree<int, char>());

  // Perform operations on a splay tree set.
  some_op_sequence(tree<
		   int,
		   char,
		   less<int>,
		   splay_tree_tag>());

  // Perform operations on a splay tree set.
  some_op_sequence(tree<
		   int,
		   char,
		   less<int>,
		   ov_tree_tag>());

  return 0;
}
