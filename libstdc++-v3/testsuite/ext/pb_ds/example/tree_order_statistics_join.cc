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
 * @file tree_order_statistics_join_example.cpp
 * An example showing how to augment a splay tree to support order statistics.
 */

// This example shows how join operations still maintain node
// invariants. Specifically, it shows how the objects of containers
// supporting order statistics can be joined into an object supporting
// order statistics.
// While the example does not show this, the same holds for split operations.

#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tree_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

// A splay tree table mapping ints to chars and storing the ints order
// statistics.
typedef
tree<int, char, less<int>,
     splay_tree_tag,
     tree_order_statistics_node_update>
tree_map_t;

int main()
{
  // Insert some entries into s0.
  tree_map_t s0;
  s0.insert(make_pair(12, 'a'));
  s0.insert(make_pair(505, 'b'));
  s0.insert(make_pair(30, 'c'));

  // The order of the keys should be: 12, 30, 505.
  assert(s0.find_by_order(0)->first == 12);
  assert(s0.find_by_order(1)->first == 30);
  assert(s0.find_by_order(2)->first == 505);

  // Insert some entries into s1.
  tree_map_t s1;
  s1.insert(make_pair(506, 'a'));
  s1.insert(make_pair(1222, 'b'));
  s1.insert(make_pair(3004, 'a'));

  // Now join s0 and s1.
  s0.join(s1);

  // The order of the keys should be: 12, 30, 505, 506, 1222, 3004.
  assert(s0.find_by_order(0)->first == 12);
  assert(s0.find_by_order(1)->first == 30);
  assert(s0.find_by_order(2)->first == 505);
  assert(s0.find_by_order(3)->first == 506);
  assert(s0.find_by_order(4)->first == 1222);
  assert(s0.find_by_order(5)->first == 3004);

  return 0;
}

