// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/*
 * @file tree_set_intervals_example.cpp
 * An example showing how to augment a trees to support operations involving
 *	line intervals.
 */

// For ov_tree_set
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For assert
#include <cassert>
// For NULL
#include <cstdlib>

/*
 * Following are definitions of line intervals and functors operating on them.
 *	As the purpose of this example is node invariants, and not
 *	computational-geometry algorithms per-se, some simplifications are made
 *(e.g., intervals are defined by unsigned integers, and not by*a parameterized type, data members are public, etc.).
 */

/*
 * An interval of unsigned integers.
 **/
struct interval
{
  /*
   * Constructor.
   * @param start [i] - Start point.
   * @param end [i] - End point.
   */
  interval(unsigned int start, unsigned int end) : m_start(start),
						   m_end(end)
  {
    assert(start <= end);
  }

  /*
   * Comparison predicate.
   * @param r_rhs [i] - Right-hand object with which to compare.
   **/
  bool
  operator<(const interval& r_rhs) const
  {
    if (m_start != r_rhs.m_start)
      return (m_start < r_rhs.m_start);

    return (m_end < r_rhs.m_end);
  }

  /*
   * Start point.
   */
  unsigned int m_start;

  /*
   * End point.
   **/
  unsigned int m_end;
};

struct intervals_node_updator;

template<class Cntnr>
bool
overlaps(const Cntnr& r_c, const interval& r_interval);

/*
 * The entry of the set. It includes an interval and the
 *	maximal endpoint of the intervals in its subtree.
 */
struct entry
{
  // Constructor. The maximal endpoint is set to the endpoint
  explicit entry(unsigned int start, unsigned int end) : m_interval(start, end),
							 m_max_endpoint(end)
  { }

  // Compares two entries by their intervals.
  inline bool
  operator<(const entry& r_rhs) const
  {
    return (m_interval < r_rhs.m_interval);
  }

  // An interval
  interval m_interval;

private:
  // The maximal endpoint of the intervals in its subtree.
  mutable unsigned int m_max_endpoint;

  friend struct intervals_node_updator;

  template<class Cntnr>
  friend bool
  overlaps(const Cntnr& r_c, const interval& r_interval);

};

/*
 * Functor updating maximal endpoints of entries.
 *	Algorithm taken from "Introduction to Algorithms" by Cormen, Leiserson,
 *	and Rivest.
 */
struct intervals_node_updator
{
  inline void
  operator()(const entry* p_entry, const entry* p_l_child_entry, const entry* p_r_child_entry)
  {
    /* The left maximal endpoint is 0 if there is no left child.
     */
    const unsigned int l_max_endpoint =(p_l_child_entry == NULL)? 0 : p_l_child_entry->m_max_endpoint;

    /* The right maximal endpoint is 0 if there is no right child.
     */
    const unsigned int r_max_endpoint =(p_r_child_entry == NULL)? 0 : p_r_child_entry->m_max_endpoint;

    p_entry->m_max_endpoint = std::max(p_entry->m_interval.m_end,
				       std::max<unsigned int>(l_max_endpoint, r_max_endpoint));
  }
};

/*
 * Checks whether a set of intervals contains at least one interval
 *	overlapping some interval.
 *	Algorithm taken from "Introduction to Algorithms" by Cormen, Leiserson,
 *	and Rivest.
 **/
template<class Cntnr>
bool
overlaps(const Cntnr& r_c, const interval& r_interval)
{
  typedef typename Cntnr::const_iterator intr_set_const_it;

  typedef typename Cntnr::const_node_iterator intr_set_const_node_it;

  intr_set_const_node_it node_it = r_c.node_begin();

  while (node_it != r_c.node_end())
    {
      // Check whether r_interval overlaps the current interval.

      intr_set_const_it it =* node_it;

      if (r_interval.m_end >= it->m_interval.m_start&& 
	  r_interval.m_start <= it->m_interval.m_end)
	return (true);

      intr_set_const_node_it l_node_it = node_it.l_child();

      const unsigned int l_max_endpoint =(l_node_it == r_c.node_end())?
	0 : (*l_node_it)->m_max_endpoint;

      if (l_max_endpoint >= r_interval.m_start)
	node_it = l_node_it;
      else
	node_it = node_it.r_child();
    }

  return (false);
}

template<class Cntnr>
void
some_op_sequence(Cntnr c)
{
  // Insert some entries.

  c.insert(entry(0, 100));
  c.insert(entry(150, 160));
  c.insert(entry(300, 1000));
  c.insert(entry(10000, 100000));
  c.insert(entry(200, 100200));

  // Test overlaps.

  // Overlaps 150 - 160
  assert(overlaps(c, interval(145, 165)) == true);
  // Overlaps 150 - 160
  assert(overlaps(c, interval(145, 155)) == true);
  assert(overlaps(c, interval(165, 175)) == false);
  assert(overlaps(c, interval(100201, 100203)) == false);

  // Erase an entry.

  entry e(150, 160);

  c.erase(e);

  // Test overlaps again.

  assert(overlaps(c, interval(145, 165)) == false);
  assert(overlaps(c, interval(165, 175)) == false);
  assert(overlaps(c, interval(0, 300000)) == true);
}

int
main()
{
  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   entry,
		   pb_assoc::null_data_type,
		   std::less<entry>,
		   pb_assoc::ov_tree_ds_tag,
		   intervals_node_updator>());

  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   entry,
		   pb_assoc::null_data_type,
		   std::less<entry>,
		   pb_assoc::rb_tree_ds_tag,
		   intervals_node_updator>());

  some_op_sequence(pb_assoc::tree_assoc_cntnr<
		   entry,
		   pb_assoc::null_data_type,
		   std::less<entry>,
		   pb_assoc::splay_tree_ds_tag,
		   intervals_node_updator>());
}

