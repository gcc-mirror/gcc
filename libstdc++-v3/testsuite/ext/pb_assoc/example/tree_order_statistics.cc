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
 * @file tree_order_statistics_example.cpp
 * An example showing how to use functors for order-statistics
 *	in tree-based containers.
 */

// For tree_assoc_cntnr
#include <ext/pb_assoc/assoc_cntnr.hpp>
// For order-statistics related policies
#include <ext/pb_assoc/tree_policy.hpp>
// For assert
#include <cassert>

/*
 * A red-black tree table storing ints and their order statistics.
 */
typedef
pb_assoc::tree_assoc_cntnr<
  pb_assoc::order_statistics_key<int>,
  pb_assoc::null_data_type,
  pb_assoc::order_statistics_key_cmp<std::less<int> >,
  pb_assoc::rb_tree_ds_tag,
  pb_assoc::order_statistics_node_updator<int> >
set_t;

int
main()
{
  set_t s;

  typedef pb_assoc::order_statistics_key< int> int_order_statistics_key;

  // Insert some entries into s.

  s.insert(int_order_statistics_key(12));
  s.insert(int_order_statistics_key(505));
  s.insert(int_order_statistics_key(30));
  s.insert(int_order_statistics_key(1000));
  s.insert(int_order_statistics_key(10000));
  s.insert(int_order_statistics_key(100));

  // The order of the keys should be: 12, 30, 100, 505, 1000, 10000.

  pb_assoc::find_by_order<set_t> by_order;

  assert(*by_order(s, 0) == 12);
  assert(*by_order(s, 1) == 30);
  assert(*by_order(s, 2) == 100);
  assert(*by_order(s, 3) == 505);
  assert(*by_order(s, 4) == 1000);
  assert(*by_order(s, 5) == 10000);
  assert(by_order(s, 6) == s.end());

  // The order of the keys should be: 12, 30, 100, 505, 1000, 10000.

  pb_assoc::order_by_key<set_t> by_key;

  assert(by_key(s, 10) == 0);
  assert(by_key(s, 12) == 0);
  assert(by_key(s, 15) == 1);
  assert(by_key(s, 30) == 1);
  assert(by_key(s, 99) == 2);
  assert(by_key(s, 100) == 2);
  assert(by_key(s, 505) == 3);
  assert(by_key(s, 1000) == 4);
  assert(by_key(s, 10000) == 5);
  assert(by_key(s, 9999999) == 6);

  // Erase an entry.
  s.erase(int_order_statistics_key(30));

  // The order of the keys should be: 12, 100, 505, 1000, 10000.

  assert(*by_order(s, 0) == 12);
  assert(*by_order(s, 1) == 100);
  assert(*by_order(s, 2) == 505);
  assert(*by_order(s, 3) == 1000);
  assert(*by_order(s, 4) == 10000);
  assert(by_order(s, 5) == s.end());

  // The order of the keys should be: 12, 100, 505, 1000, 10000.

  assert(by_key(s, 10) == 0);
  assert(by_key(s, 12) == 0);
  assert(by_key(s, 100) == 1);
  assert(by_key(s, 505) == 2);
  assert(by_key(s, 707) == 3);
  assert(by_key(s, 1000) == 3);
  assert(by_key(s, 1001) == 4);
  assert(by_key(s, 10000) == 4);
  assert(by_key(s, 100000) == 5);
  assert(by_key(s, 9999999) == 5);
}

