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
 * @file tree_intervals_example.cpp
 * An example showing how to augment a trees to support operations involving
 *    line intervals.
 */

/**
 * In some cases tree structure can be used for various purposes other
 * than storing entries by key order.  This example shows how a
 * tree-based container can be used for geometric-line intersection
 * determination. That is, the key of the container is a pair of
 * numbers representing a line interval. The container object can be
 * used to query whether a line interval intersects any line interval
 * it currently stores.
 *
 * This type of problem arises not only in geometric applications, but
 * also sometimes in distributed filesystems. Assume that "leases" are
 * taken for parts of files or LUNs. When a new lease is requested, it
 * is necessary to check that it does not conflict with a lease
 * already taken. In this case a file or LUN can be envisioned as a
 * line segment; leases requested and granted for contiguous parts of
 * the file or LUN can be represented as line intervals as well.
 */

#include <cassert>
#include <cstdlib>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace __gnu_pbds;

// Following are definitions of line intervals and functors operating
// on them. As the purpose of this example is node invariants, and not
// computational-geometry algorithms per-se, some simplifications are
// made (e.g., intervals are defined by unsigned integers, and not by
// a parameterized type, data members are public, etc.).

// An interval of unsigned integers.
typedef pair< unsigned int, unsigned int> interval;

// Functor updating maximal endpoints of entries. Algorithm taken from
// "Introduction to Algorithms" by Cormen, Leiserson, and Rivest.
template<class Node_CItr,
	 class Node_Itr,
	 class Cmp_Fn,
	 typename _Alloc>
struct intervals_node_update
{
public:
  // The metadata that each node stores is the largest endpoint of an
  // interval in its subtree. In this case, this is an unsigned int.
  typedef unsigned int metadata_type;

  // Checks whether a set of intervals contains at least one interval
  // overlapping some interval. Algorithm taken from "Introduction to
  // Algorithms" by Cormen, Leiserson, and Rivest.
  bool
  overlaps(const interval& r_interval)
  {
    Node_CItr nd_it = node_begin();
    Node_CItr end_it = node_end();

    while (nd_it != end_it)
      {
	// Check whether r_interval overlaps the current interval.
	if (r_interval.second >= (*nd_it)->first&&
	    r_interval.first <= (*nd_it)->second)
	  return true;

	// Get the const node iterator of the node's left child.
	Node_CItr l_nd_it = nd_it.get_l_child();

	// Calculate the maximal endpoint of the left child. If the
	// node has no left child, then this is the node's maximal
	// endpoint.
	const unsigned int l_max_endpoint =(l_nd_it == end_it)?
	  0 : l_nd_it.get_metadata();

	// Now use the endpoint to determine which child to choose.
	if (l_max_endpoint >= r_interval.first)
	  nd_it = l_nd_it;
	else
	  nd_it = nd_it.get_r_child();
      }

    return false;
  }

protected:
  // Update predicate: nd_it is a node iterator to the node currently
  // updated; end_nd_it is a const node iterator to a just-after leaf
  // node.
  inline void
  operator()(Node_Itr nd_it, Node_CItr end_nd_it)
  {
    // The left maximal endpoint is 0 if there is no left child.
    const unsigned int l_max_endpoint =(nd_it.get_l_child() == end_nd_it)?
      0 : nd_it.get_l_child().get_metadata();

    // The right maximal endpoint is 0 if there is no right child.
    const unsigned int r_max_endpoint =(nd_it.get_r_child() == end_nd_it)?
      0 : nd_it.get_r_child().get_metadata();

    // The maximal endpoint is the endpoint of the node's interval,
    // and the maximal endpoints of its children.
    const_cast<unsigned int&>(nd_it.get_metadata()) =
      max((*nd_it)->second, max<unsigned int>(l_max_endpoint, r_max_endpoint));
  }

  virtual Node_CItr
  node_begin() const = 0;

  virtual Node_CItr
  node_end() const = 0;

  virtual
  ~intervals_node_update()
  { }
};

// The following function performs some operation sequence on a
// generic associative container supporting order statistics. It
// inserts some intervals, and checks for overlap.
template<class Cntnr>
void
some_op_sequence(Cntnr r_c)
{
  // Insert some entries.
  r_c.insert(make_pair(0, 100));
  r_c.insert(make_pair(150, 160));
  r_c.insert(make_pair(300, 1000));
  r_c.insert(make_pair(10000, 100000));
  r_c.insert(make_pair(200, 100200));

  // Test overlaps.

  // Overlaps 150 - 160
  assert(r_c.overlaps(make_pair(145, 165)) == true);
  // Overlaps 150 - 160
  assert(r_c.overlaps(make_pair(145, 155)) == true);
  assert(r_c.overlaps(make_pair(165, 175)) == false);
  assert(r_c.overlaps(make_pair(100201, 100203)) == false);

  // Erase an interval
  r_c.erase(make_pair(150, 160));

  // Test overlaps again.
  assert(r_c.overlaps(make_pair(145, 165)) == false);
  assert(r_c.overlaps(make_pair(165, 175)) == false);
  assert(r_c.overlaps(make_pair(0, 300000)) == true);
}

int main()
{
  // Test a red-black tree.
  some_op_sequence(tree<
		   interval,
		   null_type,
		   less<interval>,
		   rb_tree_tag,
		   intervals_node_update>());

  // Test an ordered-vector tree.
  some_op_sequence(tree<
		   interval,
		   null_type,
		   less<interval>,
		   ov_tree_tag,
		   intervals_node_update>());

  // Test a splay tree.
  some_op_sequence(tree<
		   interval,
		   null_type,
		   less<interval>,
		   splay_tree_tag,
		   intervals_node_update>());

  return 0;
}

