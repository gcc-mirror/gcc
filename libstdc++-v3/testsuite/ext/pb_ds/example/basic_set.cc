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
 * @file basic_set_example.cpp
 * A basic example showing how to use sets.
 */

/**
 * This example shows how to use "sets". It defines a
 * function performing a sequence of operations on
 * a generic container. It then calls this function with some containers.
 */

#include <iostream>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tag_and_trait.hpp>

using namespace std;
using namespace __gnu_pbds;

// The following function performs a sequence of operations on an
// associative container object storing integers.
template<class Cntnr>
void
some_op_sequence(Cntnr& r_c)
{
  assert(r_c.empty());
  assert(r_c.size() == 0);

  r_c.insert(1);
  r_c.insert(2);

  assert(!r_c.empty());
  assert(r_c.size() == 2);

  cout << "All value types in the container:" << endl;
  for (typename Cntnr::const_iterator it = r_c.begin(); it != r_c.end();
       ++it)
    cout <<* it << " ";

  cout << endl;

  r_c.clear();

  assert(r_c.empty());
  assert(r_c.size() == 0);
}

int main()
{
  {
    // Perform operations on a collision-chaining hash set.
    cc_hash_table<int, null_mapped_type> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a general-probing hash set.
    gp_hash_table<int, null_mapped_type> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a red-black tree set.
    tree<int, null_mapped_type> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a splay tree set.
    tree<
      int,
      null_mapped_type,
      less<int>,
      splay_tree_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a splay tree set.
    tree<
      int,
      null_mapped_type,
      less<int>,
      ov_tree_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a list-update set.
    list_update<int, null_mapped_type> c;
    some_op_sequence(c);
  }

  return 0;
}
