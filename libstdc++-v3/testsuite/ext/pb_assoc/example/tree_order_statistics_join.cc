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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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

/**
 * @file splay_tree_map_order_statistics_example.cpp
 * An example showing how to augment a splay tree to support order statistics.
 */

// For trees.
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For order_statistics_key
#include <ext/pb_assoc/tree_policy.hpp>
// For assert
#include <cassert>

/**
 * A splay tree table mapping ints to chars and storing the ints order statistics.
 */
typedef
pb_assoc::tree_assoc_cntnr<
  pb_assoc::order_statistics_key<int>,
  char,
  pb_assoc::order_statistics_key_cmp<std::less<int> >,
  pb_assoc::splay_tree_ds_tag,
  pb_assoc::order_statistics_node_updator<int> >
map_type;

int
main()
{
  typedef pb_assoc::order_statistics_key< int> int_order_statistics_key;

  map_type s0;

  // Insert some entries into s0.

  s0.insert(make_pair(int_order_statistics_key(12), 'a'));
  s0.insert(make_pair(int_order_statistics_key(505), 'b'));
  s0.insert(make_pair(int_order_statistics_key(30), 'c'));

  // The order of the keys should be: 12, 30, 505.

  pb_assoc::find_by_order<map_type> by_order;

  assert(by_order(s0, 0)->first == 12);
  assert(by_order(s0, 1)->first == 30);
  assert(by_order(s0, 2)->first == 505);

  map_type s1;

  // Insert some entries into s1.

  s1.insert(make_pair(int_order_statistics_key(506), 'a'));
  s1.insert(make_pair(int_order_statistics_key(1222), 'b'));
  s1.insert(make_pair(int_order_statistics_key(3004), 'c'));

  // Now join s0 and s1.

  s0.join(s1);

  // The order of the keys should be: 12, 30, 505.

  assert(by_order(s0, 0)->first == 12);
  assert(by_order(s0, 1)->first == 30);
  assert(by_order(s0, 2)->first == 505);
  assert(by_order(s0, 3)->first == 506);
  assert(by_order(s0, 4)->first == 1222);
  assert(by_order(s0, 5)->first == 3004);
}

